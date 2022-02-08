module Bot
  ( Config (..)
  , Handle (..)
  , withHandle

  , run
  ) where

import qualified Data.Text                  as T
import qualified Data.Aeson                 as A
import qualified Data.Map.Lazy              as M
import qualified Logger
import qualified ClBot                      as Cl
import qualified TgBot                      as Tg
import qualified VkBot                      as Vk

import Control.Applicative        (Alternative (..))
import Control.Monad.State
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
  { getMessage    :: IO (Either String Event)
  , sendMessage   :: EventEscort -> IO ()
  , sendHelp      :: EventEscort -> A.Value -> IO ()
  , getRepeat     :: EventEscort -> RepeatNumber -> A.Value -> IO RepeatNumber
  , systemEscort  :: EventEscort
  }


withHandle :: Config -> Logger.Handle -> (Handle -> IO a) -> IO a
withHandle config logger f = f $ Handle config logger (selectBotCommandsHandle botType)
  where botType = case (cBotType config) of
          Just btype  -> btype
          Nothing     -> CL

selectBotCommandsHandle :: BotType -> BotCommand
selectBotCommandsHandle botType = case botType of
  CL -> BotCommand Cl.getMessage Cl.sendMessage Cl.sendHelp Cl.getRepeat Cl.systemEscort
  TG -> BotCommand Tg.getMessage Tg.sendMessage Tg.sendHelp Tg.getRepeat Tg.systemEscort
  VK -> BotCommand Vk.getMessage Vk.sendMessage Vk.sendHelp Vk.getRepeat Vk.systemEscort

newState :: Environment
newState = Environment $ UsersRepeat M.empty

run :: Handle -> IO ()
run handle = do
  let botTypeMsg = maybe "CL" show $ cBotType . hConfig $ handle
  let debugMsg = logMsg ["Running bot ", botTypeMsg]
  Logger.debug (hLogger handle) debugMsg

  (result, state) <- runStateT (mainCycle $ handle) $ newState
  return ()

mainCycle :: Handle -> StateT Environment IO ()
mainCycle handle = forever $ do
  event <- lift . getBotEvent $ handle
  parseEvent event handle

parseEvent :: Event -> (Handle -> StateT Environment IO () )
parseEvent event = \handle -> case event of
  HelpCommand escort    -> execHelpCommand handle escort
  RepeatCommand escort  -> execRepeatCommand handle escort
  Message escort        -> repeatMessage handle escort

getBotEvent :: Handle -> IO Event
getBotEvent handle = do
  eitherEvent <- getMessage $ hBotCommand $ handle
  case eitherEvent of
    Left errorMsg -> do
      Logger.error logger $ logMsg [srcMsg, errorMsg]
      return $ errorEvent sysEscort "ERROR!!! See log for details"
    Right event -> do
      Logger.debug logger $ logMsg [srcMsg, "Getting event ", show event]
      return event
  where 
    logger = hLogger handle
    srcMsg = "Bot.getBotEvent: "
    sysEscort = systemEscort $ hBotCommand $ handle

getHelpMessage :: Handle -> A.Value
getHelpMessage handle = textToValue $
  case cAbout $ hConfig $ handle of
    Just msg  -> msg
-- todo log that not help in config
    Nothing   -> "echobot - simple echo bot.\n/help to get this help\n/repeat to set the number of repetitions"

execHelpCommand :: Handle -> EventEscort -> StateT Environment IO ()
execHelpCommand handle escort = do
  let helpMsg = getHelpMessage handle
  lift $ (sendHelp $ hBotCommand $ handle) escort helpMsg

getRepeatQuestion :: Handle -> A.Value
getRepeatQuestion handle = textToValue $
  case cRepeatQuestion $ hConfig $ handle of
    Just msg  -> msg
-- todo log that not repeatQuestion in config
    Nothing   -> "Enter the number of repetitions"

getRepeatDefault :: Handle -> RepeatNumber
getRepeatDefault handle = case cRepeatDefault $ hConfig $ handle of
  Just rep  -> rep
-- todo log that not repeatDefault in config
  Nothing   -> RepeatNumber 3

getUserRepeat :: Handle -> Environment -> UserName -> RepeatNumber
getUserRepeat handle state name = case M.lookup name (unUsersRepeat . usersRepeat $ state) of
    Just rep  -> rep
    Nothing   -> getRepeatDefault handle

execRepeatCommand :: Handle -> EventEscort -> StateT Environment IO ()
execRepeatCommand handle escort = do
  state <- get
  let name = userName escort
  let repeatCurrent = getUserRepeat handle state name
  let repeatMsg = getRepeatQuestion handle
  let repeatMap = unUsersRepeat . usersRepeat $ state
  repeatNew <- lift $ (getRepeat $ hBotCommand $ handle) escort repeatCurrent repeatMsg
  let repeatMapNew = M.insert name repeatNew repeatMap
  put $ state {usersRepeat = UsersRepeat repeatMapNew}

repeatMessage :: Handle -> EventEscort -> StateT Environment IO ()
repeatMessage handle escort = do
  state <- get
  let name = userName escort
  let repeat = unRepeatNumber . getUserRepeat handle state $ name
  lift $ replicateM_ repeat $ (sendMessage $ hBotCommand $ handle) escort

errorEvent :: EventEscort -> String -> Event
errorEvent escort msg = Message $ Escort name message
  where
    name = userName escort
    message = UserMessage . stringToValue $ msg

