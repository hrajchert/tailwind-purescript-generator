module Identifiers where

import Prelude
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Utils.String (capitalize)
import Data.Array (uncons)

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
