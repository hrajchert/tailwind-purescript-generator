module Main where

import Prelude
import CommandLineOptions (parseCommandLineOptions)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (message, runAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Generator (GeneratedModule, writeModule)
import Tailwind.Config (resolveConfig)
import Tailwind.Utility (Utility, generate, getUtilities)

generateModule :: Array String -> Array Utility -> GeneratedModule
generateModule path utilityList =
  { path
  , utilities: generate =<< utilityList
  }

main :: Effect Unit
main =
  runAff_ processResult
    $ do
        commandLineOpts <- liftEffect $ parseCommandLineOptions
        config <- liftEffect $ resolveConfig commandLineOpts.tailwindConfig
        writeModule commandLineOpts.outputDir $ generateModule [ "Css", "Theme" ] $ getUtilities config
  where
  processResult = case _ of
    Left err -> Console.log $ "There was a problem: " <> message err
    Right _ -> Console.log $ "Process finished ok"
