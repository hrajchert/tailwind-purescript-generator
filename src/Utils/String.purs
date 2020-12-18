module Utils.String where

import Prelude
import Data.String (splitAt, toUpper)

-- | Capitalize the first `Char` in a `String`
capitalize :: String -> String
capitalize str =
  let
    { before, after } = splitAt 1 str
  in
    toUpper before <> after
