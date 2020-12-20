module Main where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (message, runAff_)
import Effect.Console as Console
import Generator (Module, writeModule)
import Node.Path (FilePath)
import Tailwind (Utility(..), generate)

testModule :: Module
testModule =
  { path: [ "Css", "Theme" ]
  , utilities:
      join
        [ generate (FontSize [ "xs" ] "0.75rem" "1rem")
        , generate (FontSize [ "sm" ] "0.875rem" "1.25rem")
        , generate (FontSize [ "base" ] "1rem" "1.5rem")
        , generate (FontWeight [ "thin" ] "100")
        , generate (FontWeight [ "extralight" ] "200")
        , generate (FontWeight [ "light" ] "300")
        , generate (Padding [ "0" ] "0px")
        , generate (Padding [ "1" ] "0.25rem")
        ]
  }

outputDir :: FilePath
outputDir = "./generated/"

main :: Effect Unit
main = runAff_ processResult $ writeModule outputDir testModule
  where
  processResult = case _ of
    Left err -> Console.log $ "There was a problem: " <> message err
    Right _ -> Console.log $ "Process finished ok"
