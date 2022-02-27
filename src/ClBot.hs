module ClBot
  ( getMessage
  , sendMessage
  , sendHelp
  , getRepeat
  , systemEscort
  , trimString
  ) where

import qualified Data.Aeson                 as A
import qualified Data.Text                  as T
import qualified System.IO                  as IO

import Text.Read                            (readEither) 
import Data.List.Extra                      (lower, trim)
import BotTypes
  ( Event (..)
  , EventEscort (..)
  , UserName (..)
  , UserMessage (..)
  , RepeatNumber (..)
  , stringToValue
  , valueToString
  )

noneUser = UserName . T.pack $ "none"
nullMessage = UserMessage .stringToValue $ ""

systemEscort :: EventEscort
systemEscort = Escort noneUser nullMessage

getMessage :: IO (Either String Event)
getMessage = do
  msg <- IO.getLine
  let event = parseMessage msg
  -- Left "error" don't exist since CL is test mode
  return $ Right event

-- todo handle of exceptions
sendMessage :: EventEscort -> IO ()
sendMessage escort = 
  case (valueToString . unUserMessage . userMessage $ escort) of
    Right msg   -> IO.putStrLn $ msg
    Left error  -> IO.putStrLn $ "Error in ClBot.sendMessage: " ++ error

-- todo handle of exceptions
sendHelp :: EventEscort -> A.Value -> IO ()
sendHelp _ helpMsg =
  case valueToString helpMsg of
    Right msg   -> IO.putStrLn $ msg
    Left error  -> IO.putStrLn $ "Error in ClBot.sendHelp: " ++ error

-- todo handle of exceptions
getRepeat :: EventEscort -> A.Value -> IO (Either String RepeatNumber)
getRepeat _ repeatQuestion = do
  case valueToString repeatQuestion of
    Left error -> return . Left $ "Error in ClBot.getRepeat: " ++ error
    Right msg  -> do
      IO.putStrLn msg
      val <- IO.getLine
      case (readEither val :: Either String Int) of
        Left error -> return . Left $ "Error in ClBot.getRepeat: " ++ error
        Right rep  -> return . Right $ RepeatNumber rep

parseMessage :: String -> Event
parseMessage msg = case trimString $ msg of
  "/help"   -> HelpCommand $ Escort noneUser nullMessage
  "/repeat" -> RepeatCommand $ Escort noneUser nullMessage
  otherwise -> Message $ Escort noneUser (UserMessage . stringToValue $ msg)

trimString :: String -> String
trimString = lower . trim
