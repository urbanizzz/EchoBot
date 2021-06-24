module Bot
  ( Config (..)
  , Handle (..)
  , withHandle

  , run
  ) where

import qualified Data.Text                  as T
import qualified Data.Aeson                 as A
import qualified Logger
import qualified ClBot                      as Cl
import qualified TgBot                      as Tg
import qualified VkBot                      as Vk

import Control.Applicative        (Alternative (..))
import BotTypes

data Config = Config
  { cBotType                :: Maybe BotType
  , cBotToken               :: Maybe BotToken
  , cRepeatDefault          :: Maybe RepeatNumber
  , cRepeatMaxNumber        :: Maybe RepeatNumber
  , cRepeatQuestion         :: Maybe T.Text
  , cAbout                  :: Maybe T.Text
  , cUnknownCommandMessage  :: Maybe T.Text
  }

instance Semigroup Config where
  (<>) = mappend

instance Monoid Config where
  mempty = Config empty empty empty empty empty empty empty
  Config x1 x2 x3 x4 x5 x6 x7 `mappend` Config y1 y2 y3 y4 y5 y6 y7 =
    Config
      (x1 <|> y1)
      (x2 <|> y2)
      (x3 <|> y3)
      (x4 <|> y4)
      (x5 <|> y5)
      (x6 <|> y6)
      (x7 <|> y7)

instance A.FromJSON Config where
  parseJSON = A.withObject "FromJSON Bot.Config" $ \o -> Config
    <$> o A..:? "botType"
    <*> o A..:? "botToken"
    <*> o A..:? "repeatDefault"
    <*> o A..:? "repeatMaxNumber"
    <*> o A..:? "repeatQuestion"
    <*> o A..:? "about"
    <*> o A..:? "unknownCommandMessage"

data Handle = Handle
  { hConfig       :: Config
  , hLogger       :: Logger.Handle
  , hBotCommand   :: BotCommand
  }

data BotCommand = BotCommand
  { getMessage    :: IO Event
  , sendMessage   :: EventEscort -> IO ()
  , sendHelp      :: EventEscort -> A.Value -> IO ()
  , getRepeat     :: EventEscort -> RepeatNumber -> A.Value -> IO RepeatNumber
  }

withHandle :: Config -> Logger.Handle -> (Handle -> IO a) -> IO a
withHandle config logger f = f $ Handle config logger (selectBotCommandsHandle botType)
  where botType = case (cBotType config) of
          Just btype  -> btype
          Nothing     -> CL

selectBotCommandsHandle :: BotType -> BotCommand
selectBotCommandsHandle botType = case botType of
  CL -> BotCommand Cl.getMessage Cl.sendMessage Cl.sendHelp Cl.getRepeat
  TG -> BotCommand Tg.getMessage Tg.sendMessage Tg.sendHelp Tg.getRepeat
  VK -> BotCommand Vk.getMessage Vk.sendMessage Vk.sendHelp Vk.getRepeat


run :: Handle -> IO ()
run h = print "Bot is running"


