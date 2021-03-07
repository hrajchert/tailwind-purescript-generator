module Main where

import Prelude
import CommandLineOptions (parseCommandLineOptions)
import Control.Monad.Logger.Trans (class MonadLogger, runLoggerT)
import Data.Either (Either(..))
import Data.Log.Formatter.Pretty (prettyFormatter)
import Effect (Effect)
import Effect.Aff (message, runAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
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
    $ runLoggerT program (prettyFormatter >=> logAff)
  where
  logAff = liftEffect <<< Console.log

  program :: forall m. MonadLogger m => MonadAff m => m Unit
  program = do
    commandLineOpts <- liftEffect $ parseCommandLineOptions
    config <- liftEffect $ resolveConfig commandLineOpts.tailwindConfig
    utilities <- getUtilities config
    liftAff $ writeModule commandLineOpts.outputDir $ generateModule [ "Css", "Theme" ] $ utilities

  processResult = case _ of
    Left err -> Console.log $ "There was a problem: " <> message err
    Right _ -> Console.log $ "Process finished ok"
