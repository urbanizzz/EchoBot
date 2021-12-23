module Tests.TestBot
  ( runTests
  ) where

import Bot
import BotTypes
import Test.QuickCheck

{-
parseEvent :: Event -> (Handle -> StateT Environment IO () )
parseEvent event = \handle -> case event of
  HelpCommand escort    -> execHelpCommand handle escort
  RepeatCommand escort  -> execRepeatCommand handle escort
  Message escort        -> repeatMessage handle escort

parseMessage :: String -> Event
parseMessage msg = case trimString $ msg of
  "/help"   -> HelpCommand $ Escort noneUser nullMessage
  "/repeat" -> RepeatCommand $ Escort noneUser nullMessage
  otherwise -> Message $ Escort noneUser (UserMessage . stringToValue $ msg)
 -}

noneUser = UserName . T.pack $ "none"
nullMessage = UserMessage .stringToValue $ ""

prop_parseHelpEvent :: Bool
prop_parseHelpEvent = undefined
--  parseEvent (HelpCommand $ Escort noneUser nullMessage) ==

runTests :: IO ()
runTests = do
  putStrLn "Begin Bot logic testing..."
  putStrLn "Done."

