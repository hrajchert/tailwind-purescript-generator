module Main where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (message, runAff_)
import Effect.Console as Console
import Generator (Module, Utility, writeModule)
import Node.Path (FilePath)

textSmTooltip :: String
textSmTooltip =
  """.text-sm {
  font-size: 0.875rem;
  line-height: 1.25rem;
}"""

utilities :: Array Utility
utilities =
  [ { description: Just textSmTooltip, selector: "text-sm", name: "textSm" }
  , { description: Nothing, selector: "text-base", name: "textBase" }
  ]

testModule :: Module
testModule =
  { path: [ "Css", "Sub", "Supersub", "Dir" ]
  , utilities
  }

outputDir :: FilePath
outputDir = "./generated/"

main :: Effect Unit
main = runAff_ processResult $ writeModule outputDir testModule
  where
  processResult = case _ of
    Left err -> Console.log $ "There was a problem: " <> message err
    Right _ -> Console.log $ "Process finished ok"
