module Card where

-- Suits and Ranks
data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq, Enum)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Enum)

-- Card data type
data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq)

instance Show Card where
    show (Card rank suit) = unlines [
        "╭─────────╮",
        "│" ++ show rank ++ padRank rank ++ showSuit suit ++ "       │",
        "│         │",
        "│         │",
        "│    " ++ showSuit suit ++ "    │",
        "│         │",
        "│         │",
        "│      " ++ show rank ++ padRank rank ++ showSuit suit ++ " │",
        "╰─────────╯"
        ]

-- Show Suit
showSuit :: Suit -> String
showSuit Hearts = "♥"
showSuit Diamonds = "♦"
showSuit Clubs = "♣"
showSuit Spades = "♠"

-- Pad Rank for single digit/character
padRank :: Rank -> String
padRank Ten = ""
padRank _   = " "

-- Deck of cards
fullDeck :: [Card]
fullDeck = [Card rank suit | suit <- [Hearts .. Spades], rank <- [Two .. Ace]]

-- Test function to display a deck
displayDeck :: IO ()
displayDeck = mapM_ (putStrLn . show) fullDeck
