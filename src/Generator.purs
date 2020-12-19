module Generator where

import Prelude
import Css (Property, prettyPrint)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as FP
import Utils.Fs (ensureDir)

type GeneratedUtility
  = { properties :: Maybe (Array Property)
    , selector :: String
    , name :: String
    }

type Module
  = { path :: Array String
    , utilities :: Array GeneratedUtility
    }

toPurescriptFile :: Module -> String
toPurescriptFile mod = intercalate "\n" (moduleComment <> header <> utilities)
  where
  modulePath = intercalate "." mod.path

  moduleComment =
    [ "-- This module was generated automatically by the 'tailwind-purescript-generator'."
    , "-- Don't make manual modifications to this file as it can be overwritten."
    ]

  header =
    [ "module " <> modulePath <> " where"
    , ""
    , "import Halogen (ClassName(..))"
    ]

  utilities = mod.utilities >>= utility

  utility :: GeneratedUtility -> Array String
  utility { properties, selector, name } =
    [ asComment $ prettyPrint selector properties
    , name <> " :: ClassName"
    , name <> " = ClassName \"" <> selector <> "\""
    ]

  asComment description = "\n-- | " <> replaceAll (Pattern "\n") (Replacement "\n-- | ") description

writeModule :: FilePath -> Module -> Aff Unit
writeModule basePath mod = do
  contents <- liftEffect $ Buffer.fromString (toPurescriptFile mod) UTF8
  let
    fullPath = FP.concat ([ basePath ] <> mod.path) <> ".purs"
  ensureDir fullPath
  FS.writeFile fullPath contents
