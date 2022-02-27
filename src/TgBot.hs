module TgBot
  ( getMessage
  , sendMessage
  , sendHelp
  , getRepeat
  , systemEscort
  ) where

import qualified Data.Aeson                 as A
import qualified Data.Text                  as T

import BotTypes
  ( Event (..)
  , EventEscort (..)
  , UserName (..)
  , UserMessage (..)
  , RepeatNumber (..)
  )

systemEscort :: EventEscort
systemEscort = undefined

getMessage :: IO (Either String Event)
getMessage = undefined

sendMessage :: EventEscort -> IO ()
sendMessage escort = undefined

sendHelp :: EventEscort -> A.Value -> IO ()
sendHelp escort helpMsg = undefined

getRepeat :: EventEscort -> A.Value -> IO (Either String RepeatNumber)
getRepeat escort repeatQuestion = undefined

