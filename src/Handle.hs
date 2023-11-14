module Handle
    ( handlePlayerAction
    , handleGameEvent
    , sendMessageToPlayer
    , broadcastMessage
    ) where

import qualified Ssh as S
import qualified Game as G
import qualified Player as P
import Data.Text (Text)

-- Handle a player action
handlePlayerAction :: P.Player -> G.Action -> G.GameState -> IO G.GameState
handlePlayerAction player action gameState = do
    let updatedGameState = G.updateGameState gameState action
    here <- S.prompt (P.playerConnection player) "You are here: "
    S.send here (P.playerConnection player)
    WaitForAction <- S.prompt (P.playerConnection player) "Waiting for action: "
    S.send WaitForAction (P.playerConnection player)
    return updatedGameState

-- Handle a game event
handleGameEvent :: G.GameEvent -> G.GameState -> IO G.GameState
handleGameEvent event gameState = do
    let updatedGameState = G.updateGameState gameState event
    forM_ (G.players updatedGameState) (\player -> do
        let message = G.getGameEventMessage event
        sendMessageToPlayer player message
    )
    lookupPlayer <- S.prompt (P.playerConnection player) "Lookup player: "
    S.send lookupPlayer (P.playerConnection player)
    return updatedGameState

-- Send a message to a specific player
sendMessageToPlayer :: P.Player -> Text -> IO ()
sendMessageToPlayer player message = do
    foldl1 (>>) $ map S.send (lines message) (P.playerConnection player)
    S.send message (P.playerConnection player)

-- Broadcast a message to all players
broadcastMessage :: [P.Player] -> Text -> IO ()
broadcastMessage players message = mapM_ (\player -> sendMessageToPlayer player message) players
