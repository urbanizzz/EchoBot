module ClBot
  ( getMessage
  , sendMessage
  , sendHelp
  , getRepeat
  , trimString
  ) where

import qualified Data.Aeson                 as A
import qualified Data.Text                  as T
import qualified System.IO                  as IO
import qualified Text.Read                  as Read

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

getMessage :: IO Event
getMessage = do
  msg <- IO.getLine
  let event = parseMessage msg
  -- debug $ "Gettting event " ++ show event
  return event

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
getRepeat :: EventEscort -> RepeatNumber -> A.Value -> IO RepeatNumber
getRepeat _ repeatOld repeatQuestion = do
  case valueToString repeatQuestion of
    Left errorMsg  -> do
      -- error $ "Error in ClBot.getRepeat: " ++ errorMsg
      return $ repeatOld
    Right msg   -> do
      IO.putStrLn msg
      val <- IO.getLine
      case (Read.readMaybe val :: Maybe Int) of
        Nothing   -> return $ repeatOld
        Just rep  -> return $ RepeatNumber rep

parseMessage :: String -> Event
parseMessage msg = case trimString $ msg of
  "/help"   -> HelpCommand $ Escort noneUser nullMessage
  "/repeat" -> RepeatCommand $ Escort noneUser nullMessage
  otherwise -> Message $ Escort noneUser (UserMessage . stringToValue $ msg)

trimString :: String -> String
trimString = lower . trim
