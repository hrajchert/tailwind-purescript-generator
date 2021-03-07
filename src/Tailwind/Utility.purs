module Tailwind.Utility where

import Prelude
import Control.Monad.Logger.Class (class MonadLogger, warn)
import Data.Log.Tag (empty)
import Data.Maybe (Maybe(..), maybe')
import Data.Traversable (for)
import Data.Tuple (fst, snd)
import Foreign.Object (Object, toArrayWithKey)
import Foreign.Object as Object
import Generator (GeneratedUtility)
import Identifiers (Identifier, toKebabCase)
import Tailwind.Config (Color(..), TailwindConfig)

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

getUtilities :: forall m. MonadLogger m => TailwindConfig -> m (Array Utility)
getUtilities config = do
  (enabledUtilities :: Array (Array Utility)) <-
    for config.corePlugins \plugin ->
      maybe'
        (\_ -> warn empty ("missing plugin " <> plugin) $> [])
        pure
        (Object.lookup plugin allPlugins)
  pure $ join $ enabledUtilities
  where
  allPlugins :: Object (Array Utility)
  allPlugins =
    Object.fromHomogeneous
      { backgroundColor
      , fontSize
      , fontWeight
      , padding
      , wordBreak
      }

  backgroundColor =
    join
      ( config.theme.backgroundColor
          # toArrayWithKey \id colorOpts -> case colorOpts of
              SingleColor color -> [ BackgroundColor [ id ] color ]
              (ColorScale colors) -> toArrayWithKey (\variant color -> BackgroundColor [ id, variant ] color) colors
      )

  fontSize =
    config.theme.fontSize
      # toArrayWithKey \id sizeOpts ->
          FontSize [ id ] (fst sizeOpts) (_.lineHeight $ snd sizeOpts)

  fontWeight =
    config.theme.fontWeight
      # toArrayWithKey \id size ->
          FontWeight [ id ] size

  padding = config.theme.padding # toArrayWithKey \id size -> Padding [ id ] size

  -- TODO: check in corePlugins if we need to use wordBreak. This applies for all plugins
  wordBreak = [ WordBreak ]

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
