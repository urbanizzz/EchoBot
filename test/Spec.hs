import Tests.TestClBot
import Tests.TestBot

main :: IO ()
main = do
  Tests.TestClBot.runTests
  Tests.TestBot.runTests
