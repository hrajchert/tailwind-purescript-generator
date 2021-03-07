module Tailwind.Config where

import Prelude
import Argonaut.CodecExtra (customDecoder, dictionaryDecoder')
import Control.Alt ((<|>))
import Data.Argonaut as A
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut (printJsonDecodeError)
import Data.Codec.Argonaut.Common as JA
import Data.Codec.Argonaut.Record as JAR
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Uncurried as FU
import Foreign.Object (Object)
import Node.Path (FilePath, resolve)
import Node.Process as Process

type TailwindConfig
  = { corePlugins :: Array String
    , important :: Boolean
    , separator :: String
    , theme :: Theme
    }

configCodec :: JA.JsonCodec TailwindConfig
configCodec =
  JA.object "Root"
    $ JAR.record
        { corePlugins: JA.array JA.string
        , important: JA.boolean
        , separator: JA.string
        , theme: themeCodec
        }

type Theme
  = { backgroundColor :: Object Color
    , colors :: Object Color
    , fontWeight :: Object String
    , fontSize :: Object (Tuple String { lineHeight :: String })
    , padding :: Object String
    , screens :: Object String
    }

themeCodec :: JA.JsonCodec Theme
themeCodec =
  JA.object "Theme"
    $ JAR.record
        { backgroundColor: dictionaryDecoder' colorCodec
        , colors: dictionaryDecoder' colorCodec
        , fontWeight: dictionaryDecoder' JA.string
        , fontSize:
            dictionaryDecoder'
              $ JA.tuple
                  JA.string
                  (JA.object "second argument" $ JAR.record { lineHeight: JA.string })
        , padding: dictionaryDecoder' JA.string
        , screens: dictionaryDecoder' JA.string
        }

data Color
  -- Single color is just a single color, like "#000" or "transparent"
  = SingleColor String
  -- Color shade allows the definition on multiple variants for the same color.
  -- like 50, 100, 200 or light, dark, darker
  | ColorScale (Object String)

colorCodec :: JA.JsonCodec Color
colorCodec =
  customDecoder \json ->
    (SingleColor <$> A.toString json) <|> (ColorScale <$> (A.toObject json >>= traverse A.toString))

-- FFI to resolveConfig explained here https://tailwindcss.com/docs/configuration#referencing-in-java-script
foreign import _resolveConfig :: FU.EffectFn1 FilePath Json

resolveConfig :: FilePath -> Effect TailwindConfig
resolveConfig path = do
  cwd <- Process.cwd
  absolutePath <- resolve [ cwd ] path
  json <- FU.runEffectFn1 _resolveConfig absolutePath
  case JA.decode configCodec json of
    Right val -> pure val
    Left err -> throw $ printJsonDecodeError err
