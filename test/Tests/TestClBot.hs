module Tests.TestClBot
  ( runTests
  ) where

import ClBot
import Test.QuickCheck
import Data.Char            (chr)
import Data.List.Extra      (lower, trim)


-- source en.wikipedia.org/wiki/Whitespace_character
listOfWhitespaces = [9,10,11,12,13,32,133,160,5760,8232,8233,8239,8287,12288] ++ [8192..8202]

genTestString :: String -> Gen String
genTestString s = do
  suffix <- genSpaces
  prefix <- genSpaces
  return $ suffix ++ s ++ prefix

genSpaces :: Gen [Char]
genSpaces = listOf . elements . map chr $ listOfWhitespaces

prop_whitespaceInvariant :: String -> Property
prop_whitespaceInvariant s = 
  forAll (genTestString "/help") trimTest
    where trimTest str = (trimString str) == (trimString . trim $ str)

prop_caseInsensitive :: String -> Bool
prop_caseInsensitive s = (trimString s) == (trimString . lower $ s)

runTests :: IO ()
runTests = do
  putStrLn "Begin ClBot testing..."
  quickCheckWithResult stdArgs {maxSuccess = 10000} prop_whitespaceInvariant
  quickCheckWithResult stdArgs {maxSuccess = 10000} prop_caseInsensitive
  putStrLn "Done."
