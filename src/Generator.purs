module Generator where

import Prelude
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import FsUtils (ensureDir)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as FP

type Utility
  = { description :: Maybe String
    , selector :: String
    , name :: String
    }

type Module
  = { path :: Array String
    , utilities :: Array Utility
    }

generate :: Module -> String
generate mod = intercalate "\n" (moduleComment <> header <> utilities)
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

  utility :: Utility -> Array String
  utility { description, selector, name } =
    [ asComment description
    , name <> " :: ClassName"
    , name <> " = ClassName \"" <> selector <> "\""
    ]

  asComment = case _ of
    Nothing -> ""
    Just description -> "\n-- | " <> replaceAll (Pattern "\n") (Replacement "\n-- | ") description

writeModule :: FilePath -> Module -> Aff Unit
writeModule basePath mod = do
  contents <- liftEffect $ Buffer.fromString (generate mod) UTF8
  let
    fullPath = FP.concat ([ basePath ] <> mod.path) <> ".purs"
  ensureDir fullPath
  FS.writeFile fullPath contents
