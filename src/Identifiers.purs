module Identifiers where

import Prelude
import Data.Array (uncons)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replace, split)
import Utils.String (capitalize)

type Identifier
  = Array String

-- kebab-case
fromKebabCase :: String -> Identifier
fromKebabCase = split (Pattern "-")

toKebabCase :: Identifier -> String
toKebabCase = intercalate "-"

-- camelCase
toCamelCase :: Identifier -> String
toCamelCase parts =
  intercalate ""
    $ case uncons parts of
        Nothing -> [] {- This should never happen, but maybe refactor to NonEmptyArray -}
        Just { head, tail } -> [ head ] <> (capitalize <$> tail)

-- TODO: See what else we need to replace and how to do multi-replacement
escape :: String -> String
escape = replace (Pattern ".") (Replacement "_")
