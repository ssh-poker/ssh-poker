module Lib
    ( startGameServer
    , GameState
    , Player
    , initializeGame
    , playerAction
    ) where

import qualified Card as C
import qualified Deck as D
import qualified Ssh as S
import Data.IORef
import Control.Concurrent (forkIO)
import System.Random (newStdGen)

-- Data types for game state, players, etc.
data GameState = GameState {
    deck :: [C.Card],
    players :: [Player],
    currentTurn :: Player
}

data Player = Player {
    playerName :: String,
    playerHand :: [C.Card],
    playerConnection :: S.SSH.Connection
    -- Other player details
}

-- Initialize game state
initializeGame :: IO GameState
initializeGame = do
    shuffledDeck <- D.shuffleDeck
    return GameState {
        deck = shuffledDeck,
        players = [],
        currentTurn = undefined -- Placeholder
    }

-- Handle player actions
playerAction :: Player -> GameState -> IO GameState
playerAction player gameState = do
    -- Do stuff
    deck <- D.shuffleDeck
    connection <- S.connectToServer "localhost" 22
    let newPlayer = Player {
        playerName = "Player 1",
        playerHand = [],
        playerConnection = connection
    }
    return gameState

-- Start the game server
startGameServer :: Int -> IO ()
startGameServer port = S.startSshServer port handleNewConnection
