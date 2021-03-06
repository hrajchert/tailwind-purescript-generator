module CommandLineOptions (CommandLineOptions, parseCommandLineOptions) where

import Prelude
import Effect (Effect)
import Node.Path (FilePath)
import Options.Applicative (Parser, execParser, fullDesc, help, helper, info, long, metavar, progDesc, short, showDefault, strOption, value, (<**>))

type CommandLineOptions
  = { outputDir :: FilePath
    , tailwindConfig :: FilePath
    }

parseCommandLineOptions :: Effect CommandLineOptions
parseCommandLineOptions =
  execParser
    $ info (parser <**> helper)
    $ progDesc "Generates typed identifiers from a Tailwind config"
    <> fullDesc

parser :: Parser CommandLineOptions
parser = ado
  outputDir <-
    strOption
      $ long "output"
      <> short 'o'
      <> help "Directory where the generated identifiers will be stored"
      <> value "./generated"
      <> showDefault
      <> metavar "PATH"
  tailwindConfig <-
    strOption
      $ long "config"
      <> short 'c'
      <> help "Path to the Tailwind configuration"
      <> value "./tailwind-config.js"
      <> showDefault
      <> metavar "PATH"
  in { outputDir, tailwindConfig }
