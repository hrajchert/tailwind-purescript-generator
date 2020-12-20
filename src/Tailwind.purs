module Tailwind where

import Prelude
import Data.Maybe (Maybe(..))
import Generator (GeneratedUtility)
import Identifiers (fromKebabCase, toKebabCase)

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

-- FIXME almost sure that propertyName (which may need a new name) should be
-- between each Utility, as not all utilities have a """specification"""
data Utility
  = FontSize Size Size
  | FontWeight Weight
  | Padding Size

generate :: PropertyName -> Utility -> Array GeneratedUtility
generate propertyName (FontSize fontSize lineHeight) =
  [ { properties:
        Just
          [ { name: "font-size", value: fontSize }
          , { name: "line-height", value: lineHeight }
          ]
    , identifier: [ "text" ] <> fromKebabCase propertyName
    }
  ]

generate propertyName (FontWeight weight) =
  [ { properties:
        Just
          [ { name: "font-weight", value: weight }
          ]
    , identifier: [ "font" ] <> fromKebabCase propertyName
    }
  ]

generate propertyName (Padding size) = generalProperty <> directionalProperties
  where
  generalProperty =
    [ { properties:
          Just
            [ { name: "padding", value: size }
            ]
      , identifier: [ "p" ] <> fromKebabCase propertyName
      }
    ]

  directionalProperties =
    forAllDirections \directionModifier cssModifiers ->
      { properties:
          ( Just
              ( cssModifiers
                  <#> \cssModif ->
                      { name: toKebabCase [ "padding", cssModif ], value: size }
              )
          )
      , identifier: [ "p", directionModifier ] <> fromKebabCase propertyName
      }

forAllDirections :: (String -> Array String -> GeneratedUtility) -> Array GeneratedUtility
forAllDirections generator =
  [ generator "b" [ "bottom" ]
  , generator "t" [ "top" ]
  , generator "r" [ "right" ]
  , generator "l" [ "left" ]
  , generator "x" [ "left", "right" ]
  , generator "y" [ "top", "bottom" ]
  ]

-- data Variant
--   = Responsive
--   | Focus
