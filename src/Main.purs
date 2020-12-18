module Main where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (message, runAff_)
import Effect.Console as Console
import Generator (Module, GeneratedUtility, writeModule)
import Node.Path (FilePath)
import Tailwind (Utility(..), generate)

textSmTooltip :: String
textSmTooltip =
  """.text-sm {
  font-size: 0.875rem;
  line-height: 1.25rem;
}"""

utilities :: Array GeneratedUtility
utilities =
  [ { description: Just textSmTooltip, selector: "text-sm", name: "textSm" }
  , { description: Nothing, selector: "text-base", name: "textBase" }
  ]

testModule :: Module
testModule =
  { path: [ "Css", "Sub", "Supersub", "Dir" ]
  , utilities
  }

testModule2 :: Module
testModule2 =
  { path: [ "Css", "Theme" ]
  , utilities:
      [ generate "xs" (FontSize "0.75rem" "1rem")
      , generate "sm" (FontSize "0.875rem" "1.25rem")
      , generate "base" (FontSize "1rem" "1.5rem")
      , generate "thin" (FontWeight "100")
      , generate "extralight" (FontWeight "200")
      , generate "light" (FontWeight "300")
      ]
  }

outputDir :: FilePath
outputDir = "./generated/"

main :: Effect Unit
main =
  runAff_ processResult do
    writeModule outputDir testModule
    writeModule outputDir testModule2
  where
  processResult = case _ of
    Left err -> Console.log $ "There was a problem: " <> message err
    Right _ -> Console.log $ "Process finished ok"
