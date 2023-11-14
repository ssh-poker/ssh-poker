module Main where

import Test.HUnit
import qualified CardTests as CT
import qualified DeckTests as DT
import qualified SshTests as ST
import qualified AuthTests as AT
import qualified HandleTests as HT
import qualified SecTests as SCT

-- Main function to run all tests
main :: IO ()
main = do
    putStrLn "Running tests..."
    _ <- runTestTT allTests
    return ()

-- Combine all test cases
allTests :: Test
allTests = TestList [CT.cardTests, DT.deckTests, ST.sshTests, AT.authTests, HT.handleTests, SCT.secTests]
