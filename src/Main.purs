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
        [ generate "xs" (FontSize "0.75rem" "1rem")
        , generate "sm" (FontSize "0.875rem" "1.25rem")
        , generate "base" (FontSize "1rem" "1.5rem")
        , generate "thin" (FontWeight "100")
        , generate "extralight" (FontWeight "200")
        , generate "light" (FontWeight "300")
        , generate "0" (Padding "0px")
        , generate "1" (Padding "0.25rem")
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
