module Main where

import Prelude
import Data.Either (Either(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (message, runAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Generator (GeneratedModule, writeModule)
import Node.Path (FilePath)
import Tailwind.Config (resolveConfig)
import Tailwind.Utility (Utility(..), generate)

testUtilities :: Array Utility
testUtilities =
  [ (BackgroundColor [ "transparent" ] "transparent")
  , (BackgroundColor [ "white" ] "#fff")
  , (BackgroundColor [ "gray", "50" ] "#f9fafb")
  , (BackgroundColor [ "gray", "100" ] "#f3f4f6")
  , (FontSize [ "xs" ] "0.75rem" "1rem")
  , (FontSize [ "sm" ] "0.875rem" "1.25rem")
  , (FontSize [ "base" ] "1rem" "1.5rem")
  , (FontWeight [ "thin" ] "100")
  , (FontWeight [ "extralight" ] "200")
  , (FontWeight [ "light" ] "300")
  , (Padding [ "0" ] "0px")
  , (Padding [ "1" ] "0.25rem")
  , WordBreak
  ]

generateModule :: Array String -> Array Utility -> GeneratedModule
generateModule path utilityList =
  { path
  , utilities: generate =<< utilityList
  }

outputDir :: FilePath
outputDir = "./generated/"

main :: Effect Unit
main =
  runAff_ processResult
    $ do
        void $ liftEffect
          $ spy "tailWind config"
          <$> resolveConfig "./tailwind-default-config.js"
        writeModule outputDir $ generateModule [ "Css", "Theme" ] testUtilities
  where
  processResult = case _ of
    Left err -> Console.log $ "There was a problem: " <> message err
    Right _ -> Console.log $ "Process finished ok"
