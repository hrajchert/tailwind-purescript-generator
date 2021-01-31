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
import Tailwind.Utility (Utility(..), generate)
import Tailwind.Config (resolveConfig)

testModule :: GeneratedModule
testModule =
  { path: [ "Css", "Theme" ]
  , utilities:
      join
        [ generate (BackgroundColor [ "transparent" ] "transparent")
        , generate (BackgroundColor [ "white" ] "#fff")
        , generate (BackgroundColor [ "gray", "50" ] "#f9fafb")
        , generate (BackgroundColor [ "gray", "100" ] "#f3f4f6")
        , generate (FontSize [ "xs" ] "0.75rem" "1rem")
        , generate (FontSize [ "sm" ] "0.875rem" "1.25rem")
        , generate (FontSize [ "base" ] "1rem" "1.5rem")
        , generate (FontWeight [ "thin" ] "100")
        , generate (FontWeight [ "extralight" ] "200")
        , generate (FontWeight [ "light" ] "300")
        , generate (Padding [ "0" ] "0px")
        , generate (Padding [ "1" ] "0.25rem")
        , generate WordBreak
        ]
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
        writeModule outputDir testModule
  where
  processResult = case _ of
    Left err -> Console.log $ "There was a problem: " <> message err
    Right _ -> Console.log $ "Process finished ok"
