module Main where

import qualified System.IO                as IO
import qualified Logger
import qualified Bot

import System.Environment         (getArgs, getProgName)
import System.Exit                (exitFailure)
import Data.Yaml                  (FromJSON (..), decodeFileEither, withObject, (.:?), (.!=))

data Config = Config
  { cLogger       :: Logger.Config
  , cBot          :: Bot.Config
  }

instance Semigroup Config where
  l <> r = Config
    { cLogger = cLogger l <> cLogger r
    , cBot    = cBot    l <> cBot    r
    }

instance Monoid Config where
  mempty = Config
    { cLogger = mempty
    , cBot    = mempty
    }

  mappend l r = Config
    { cLogger = cLogger l <>  cLogger r
    , cBot    = cBot    l <>  cBot    r
    }

instance FromJSON Config where
  parseJSON = withObject "FromJSON Main.Config" $ \o -> Config
    <$> o .:? "logger"  .!= mempty
    <*> o .:? "bot"     .!= mempty
      
main :: IO ()
main = do
  args  <- getArgs
  progName <- getProgName
  case args of
    [configPath] -> run configPath
    _ -> do
      run "./echobot.yaml"
-- in production version uncomment 2 strings below and delete string above
      -- IO.hPutStrLn IO.stderr $ "Usage: " ++ progName ++ " <config_path>"
      -- exitFailure

run :: FilePath -> IO ()
run configPath = do
  IO.hPutStrLn IO.stderr $
    "Booting EchoBot"
  errOrConfig <- decodeFileEither configPath
  Config {..} <- either (fail . show) return errOrConfig

  Logger.withHandle cLogger $ \logger ->
    Bot.withHandle cBot logger $ \bot ->
      Bot.run bot

