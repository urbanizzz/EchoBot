module Logger
  ( LogLevel (..)
  , Config (..)
  , Handle
  , withHandle

  , debug
  , info
  , warning
  , error
  ) where

import qualified Data.Text                  as T
import qualified System.Log.FastLogger      as FL

import Control.Applicative        (Alternative (..))
import Control.Exception          (bracket)
import Data.Yaml                  (FromJSON (..), withObject, withText, (.:?))
import Data.Maybe                 (fromMaybe)
import Prelude            hiding  (error, log)

data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Show)

instance FromJSON LogLevel where
  parseJSON = withText "FromJSON Logger.LogLevel" $ \t ->
    case t of
      "debug"     -> pure Debug
      "info"      -> pure Info
      "warning"   -> pure Warning
      "error"     -> pure Error
      _           -> fail $ "Unknown loglevel: " ++ T.unpack t

data Config = Config
  { cPath       :: Maybe FilePath
  , cLogLevel   :: Maybe LogLevel
  } deriving (Show)

instance Semigroup Config where
  Config p0 l0 <> Config p1 l1 = Config (p0 <|> p1) (l0 <|> l1)

instance Monoid Config where
  mempty                              = Config empty empty
  Config p0 l0 `mappend` Config p1 l1 = Config (p0 <|> p1) (l0 <|> l1)

instance FromJSON Config where
  parseJSON = withObject "FromJSON Logger.Config" $ \o -> Config
    <$> o .:? "path"
    <*> o .:? "logLevel"

data Handle = Handle
  { hConfig     :: Config
  , hLoggerSet  :: FL.LoggerSet
  }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config f = bracket
  (case cPath config of
    Nothing   -> FL.newStderrLoggerSet FL.defaultBufSize
    Just "-"  -> FL.newStderrLoggerSet FL.defaultBufSize
    Just path -> FL.newFileLoggerSet FL.defaultBufSize path)
  FL.rmLoggerSet
  (\l -> f Handle {hConfig = config, hLoggerSet = l})

log :: FL.ToLogStr s => Handle -> LogLevel -> s -> IO ()
log Handle {..} lev x
  | lev >= loglevel = FL.pushLogStrLn hLoggerSet $ FL.toLogStr x
  | otherwise       = return ()
  where
    loglevel = fromMaybe Debug (cLogLevel hConfig)

debug, info, warning, error :: FL.ToLogStr s => Handle -> s -> IO ()
debug   h = log h Debug
info    h = log h Info
warning h = log h Warning
error   h = log h Error

