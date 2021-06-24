module TgBot
  ( getMessage
  , sendMessage
  , sendHelp
  , getRepeat
  ) where

import qualified  Data.Aeson                as A
import qualified  Data.Text                 as T

import BotTypes
  ( Event (..)
  , EventEscort (..)
  , UserName (..)
  , UserMessage (..)
  , RepeatNumber (..)
  )

getMessage :: IO Event
getMessage = undefined

sendMessage :: EventEscort -> IO ()
sendMessage escort = undefined

sendHelp :: EventEscort -> A.Value -> IO ()
sendHelp escort helpMsg = undefined

getRepeat :: EventEscort -> RepeatNumber -> A.Value -> IO RepeatNumber
getRepeat escort repeatOld repeatQuestion = undefined

