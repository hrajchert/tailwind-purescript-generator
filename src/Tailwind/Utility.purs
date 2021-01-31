module Tailwind.Utility where

import Prelude
import Data.Maybe (Maybe(..))
import Generator (GeneratedUtility)
import Identifiers (Identifier, toKebabCase)

type PropertyName
  = String

type UtilityName
  = String

-- Something like: #f9fafb, #fff, transparent
type ColorName
  = String

-- Something like: 0.5rem
type Size
  = String

-- Something like: 100, 200, 300
type Weight
  = String

-- FIXME: Check how to name negative identifiers
--   ex: https://tailwindcss.com/docs/top-right-bottom-left and https://tailwindcss.com/docs/margin
-- Check weird classes (and negative) like
--    https://tailwindcss.com/docs/space
-- Check how to name classes like w-11/12
--    https://tailwindcss.com/docs/width (also see how to handle min-content and proportions in the same customization)
data Utility
  = BackgroundColor Identifier ColorName
  | FontSize Identifier Size Size
  | FontWeight Identifier Weight
  | Padding Identifier Size
  | WordBreak

generate :: Utility -> Array GeneratedUtility
generate (BackgroundColor identifier color) =
  [ { properties:
        Just
          [ { name: "background-color", value: color }
          ]
    , identifier: [ "bg" ] <> identifier
    }
  ]

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

generate (Padding identifier size) = generalPadding <> directionalPadding
  where
  generalPadding =
    [ { properties:
          Just
            [ { name: "padding", value: size }
            ]
      , identifier: [ "p" ] <> identifier
      }
    ]

  directionalPadding =
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

generate WordBreak = [ breakNormal, breakWords, breakAll ]
  where
  breakNormal =
    { properties:
        Just
          [ { name: "overflow-wrap", value: "normal" }
          , { name: "word-break", value: "normal" }
          ]
    , identifier: [ "break", "normal" ]
    }

  breakWords =
    { properties:
        Just
          [ { name: "overflow-wrap", value: "break-word" }
          ]
    , identifier: [ "break", "words" ]
    }

  breakAll =
    { properties:
        Just
          [ { name: "word-break", value: "break-all" }
          ]
    , identifier: [ "break", "all" ]
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
