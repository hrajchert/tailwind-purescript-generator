module Tailwind where

import Prelude
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Generator (GeneratedUtility)
import Utils.String (capitalize)
import Data.Array (uncons)

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

toParts :: String -> Array String
toParts = split (Pattern "-")

fromParts :: Array String -> String
fromParts = intercalate "-"

generateName :: Array String -> { selector :: String, name :: String }
generateName names =
  { selector: fromParts names
  , name:
      intercalate ""
        $ case uncons names of
            Nothing -> [] {- This should never happen, but maybe refactor to NonEmptyArray -}
            Just { head, tail } -> [ head ] <> (capitalize <$> tail)
  }

generate :: PropertyName -> Utility -> Array GeneratedUtility
generate propertyName (FontSize fontSize lineHeight) =
  let
    { selector, name } = generateName $ [ "text" ] <> toParts propertyName
  in
    [ { properties:
          Just
            [ { name: "font-size", value: fontSize }
            , { name: "line-height", value: lineHeight }
            ]
      , selector
      , name
      }
    ]

generate propertyName (FontWeight weight) =
  let
    { selector, name } = generateName $ [ "font" ] <> toParts propertyName
  in
    [ { properties:
          Just
            [ { name: "font-weight", value: weight }
            ]
      , selector
      , name
      }
    ]

generate propertyName (Padding size) = generalProperty <> directionalProperties
  where
  generalProperty =
    let
      { selector, name } = generateName $ [ "p" ] <> toParts propertyName
    in
      [ { properties:
            Just
              [ { name: "padding", value: size }
              ]
        , selector
        , name
        }
      ]

  directionalProperties =
    forAllDirections \directionModifier cssModifiers ->
      let
        { selector, name } = generateName $ [ "p", directionModifier ] <> toParts propertyName
      in
        { properties:
            ( Just
                ( cssModifiers
                    <#> \cssModif ->
                        { name: fromParts [ "padding", cssModif ], value: size }
                )
            )
        , selector
        , name
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
