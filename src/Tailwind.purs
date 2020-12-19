module Tailwind where

import Prelude
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Generator (GeneratedUtility)
import Utils.String (capitalize)

type PropertyName
  = String

type UtilityName
  = String

-- Something like: 0.5rem
type Size
  = String

-- Something like: 100, 200, 300
type Weight
  = String

data Utility
  = FontSize Size Size
  | FontWeight Weight
  | Padding Size

generateName :: UtilityName -> PropertyName -> { selector :: String, name :: String }
generateName utilityName propertyName =
  let
    propertyParts = split (Pattern "-") propertyName
  in
    { selector: intercalate "-" $ [ utilityName ] <> propertyParts
    , name: intercalate "" $ [ utilityName ] <> (capitalize <$> propertyParts)
    }

generate :: PropertyName -> Utility -> GeneratedUtility
generate propertyName (FontSize fontSize lineHeight) =
  let
    { selector, name } = generateName "text" propertyName
  in
    { properties:
        Just
          [ { name: "font-size", value: fontSize }
          , { name: "line-height", value: lineHeight }
          ]
    , selector
    , name
    }

generate propertyName (FontWeight weight) =
  let
    { selector, name } = generateName "font" propertyName
  in
    { properties:
        Just
          [ { name: "font-weight", value: weight }
          ]
    , selector
    , name
    }

generate propertyName (Padding size) =
  let
    { selector, name } = generateName "p" propertyName
  in
    { properties:
        Just
          [ { name: "padding", value: size }
          ]
    , selector
    , name
    }

-- data Variant
--   = Responsive
--   | Focus
