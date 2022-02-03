module BotTypes where

import qualified Data.Map.Lazy              as M
import qualified Data.Text                  as T
import qualified Data.Aeson                 as A 

import Data.Scientific            (coefficient)

data BotType
  = CL
  | TG
  | VK
  deriving (Eq, Ord, Show)

instance A.FromJSON BotType where
  parseJSON = A.withText "FromJSON Types.BotType" $ \t ->
    case t of
      "cl" -> pure CL
      "vk" -> pure VK
      "tg" -> pure TG
      _    -> fail $ "Unknown type of bot in config: " ++ T.unpack t

newtype BotToken      = BotToken      {unBotToken     :: T.Text}

instance A.FromJSON BotToken where
  parseJSON = A.withText "FromJSON Types.BotToken" $ return . BotToken

newtype UserName      = UserName      {unUserName     :: T.Text}
  deriving (Eq, Ord)

instance Show UserName where
  show (UserName name) = "username: " ++ T.unpack name

newtype RepeatNumber  = RepeatNumber  {unRepeatNumber :: Int}

instance A.FromJSON RepeatNumber where
  parseJSON = A.withScientific "FromJSON Types.RepeatNumber" $ return . RepeatNumber . fromInteger . coefficient

newtype UsersRepeat   = UsersRepeat   {unUsersRepeat  :: M.Map UserName RepeatNumber}
newtype UserMessage   = UserMessage   {unUserMessage  :: A.Value}

instance Show UserMessage where
  show (UserMessage msg) = "message: " ++ either show show (valueToString msg)

data Event  = HelpCommand EventEscort
            | RepeatCommand EventEscort
            | Message EventEscort
  deriving Show

data EventEscort = Escort
  { userName    :: UserName
  , userMessage :: UserMessage
  }

instance Show EventEscort where
  show (Escort name msg) =
    mconcat
      [ "Escort {"
      , show name
      , ", "
      , show msg
      , "}"
      ]

newtype Environment = Environment
  { usersRepeat         :: UsersRepeat
  }

stringToValue :: String -> A.Value
stringToValue = A.String . T.pack

textToValue :: T.Text -> A.Value
textToValue = A.String

valueToString :: A.Value -> Either String String
valueToString (A.String str)  = Right . T.unpack $ str
valueToString _               = Left "Value is not (String a)"

valueToText :: A.Value -> Either String T.Text
valueToText (A.String str)  = Right $ str
valueToText _               = Left "Value is not (String a)"

logMsg :: [String] -> String
logMsg = mconcat

