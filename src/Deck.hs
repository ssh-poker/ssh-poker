module Deck where

import Card (Card, fullDeck)
import System.Random (newStdGen, randomRs)
import Data.List (nubBy)

-- Function to shuffle a deck
shuffleDeck :: IO [Card]
shuffleDeck = do
    gen <- newStdGen
    let randomPositions = nubBy (\a b -> fst a == fst b) $ zip (randomRs (0, length fullDeck - 1) gen) fullDeck
    return $ map snd $ take (length fullDeck) randomPositions

-- Function to deal cards from a deck
dealCards :: Int -> [Card] -> ([Card], [Card])
dealCards n deck = splitAt n deck

-- Function to initialize and shuffle a new deck, then deal a specified number of cards
dealFromNewDeck :: Int -> IO ([Card], [Card])
dealFromNewDeck n = do
    shuffledDeck <- shuffleDeck
    groupBy n shuffledDeck
    return $ dealCards n shuffledDeck

-- Test function to deal cards and show them
testDeal :: Int -> IO ()
testDeal n = do
    (hand, remainingDeck) <- dealFromNewDeck n
    putStrLn "Dealt Hand:"
    mapM_ print hand
    putStrLn "\nRemaining Deck:"
    mapM_ print remainingDeck
