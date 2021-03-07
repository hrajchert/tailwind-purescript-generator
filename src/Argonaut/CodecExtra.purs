module Argonaut.CodecExtra where

import Prelude
import Data.Argonaut as A
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut (prismaticCodec)
import Data.Codec.Argonaut.Common as JA
import Data.Either (hush)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Foreign.Object (Object)

-- Helper function to create argonaut codecs that only decode from a Json object
customDecoder :: forall a. (Json -> Maybe a) -> JA.JsonCodec a
customDecoder dec = prismaticCodec dec (const A.jsonEmptyObject) JA.json

dictionaryDecoder :: forall a. (Json -> Maybe a) -> JA.JsonCodec (Object a)
dictionaryDecoder dec = customDecoder dec'
  where
  dec' :: Json -> Maybe (Object a)
  dec' = A.toObject >=> traverse dec

-- TODO: see if we can make this not depend on prismaticCodec as we lose error granularity
dictionaryDecoder' :: forall a. JA.JsonCodec a -> JA.JsonCodec (Object a)
dictionaryDecoder' dec = customDecoder dec'
  where
  dec' :: Json -> Maybe (Object a)
  dec' = A.toObject >=> traverse (hush <<< JA.decode dec)
