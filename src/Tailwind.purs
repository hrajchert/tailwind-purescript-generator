module Tailwind where

import Prelude
import Data.Maybe (Maybe(..))
import Generator (GeneratedUtility)
import Identifiers (Identifier, toKebabCase)

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
  = FontSize Identifier Size Size
  | FontWeight Identifier Weight
  | Padding Identifier Size

generate :: Utility -> Array GeneratedUtility
generate (FontSize identifier fontSize lineHeight) =
  [ { properties:
        Just
          [ { name: "font-size", value: fontSize }
          , { name: "line-height", value: lineHeight }
          ]
    , identifier: [ "text" ] <> identifier
    }
  ]

generate (FontWeight identifier weight) =
  [ { properties:
        Just
          [ { name: "font-weight", value: weight }
          ]
    , identifier: [ "font" ] <> identifier
    }
  ]

generate (Padding identifier size) = generalProperty <> directionalProperties
  where
  generalProperty =
    [ { properties:
          Just
            [ { name: "padding", value: size }
            ]
      , identifier: [ "p" ] <> identifier
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
      , identifier: [ "p", directionModifier ] <> identifier
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
