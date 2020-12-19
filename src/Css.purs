module Css where

import Prelude
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))

type Property
  = { name :: String
    , value :: String
    }

prettyPrint :: String -> Maybe (Array Property) -> String
prettyPrint selector mProperties = case mProperties of
  Nothing -> ""
  Just properties ->
    intercalate "\n"
      ( [ "." <> selector <> " {" ]
          <> (properties <#> \{ name, value } -> "  " <> name <> ": " <> value <> ";")
          <> [ "}" ]
      )
