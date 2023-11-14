module Main where

import qualified Network.SSH.Server as SSH
import qualified GameLogic as GL
import qualified PlayerManagement as PM
import Control.Concurrent (forkIO, Chan, newChan, writeChan, readChan)
import Control.Exception (catch, SomeException)
import System.Environment (getArgs)
import Data.IORef (newIORef, readIORef, writeIORef)

-- Main function to start the server
main :: IO ()
main = do
    args <- getArgs
    let port = read (head args) :: Int
    putStrLn $ "Starting SSH Poker Server on port " ++ show port
    gameChannel <- newChan
    SSH.startSSHServer port (handleNewConnection gameChannel)

-- Function to handle new SSH connections
handleNewConnection :: Chan GL.GameEvent -> SSH.Connection -> IO ()
handleNewConnection gameChannel conn = do
    catch (do
        -- Authentication and player setup
        player <- PM.authenticatePlayer conn
        -- Shared game state reference
        gameStateRef <- newIORef GL.initialGameState
        -- Start a new thread for the player
        forkIO $ playerSession player gameStateRef gameChannel
        return ()
        ) (\e -> putStrLn $ "Connection error: " ++ show (e :: SomeException))

-- Player session handling
playerSession :: PM.Player -> IORef GL.GameState -> Chan GL.GameEvent -> IO ()
playerSession player gameStateRef gameChannel = do
    -- Initial player setup
    GL.setupPlayer player gameStateRef
    -- Start a new thread for the game loop
    forkIO $ GL.gameLoop player gameStateRef gameChannel (IO (
        writeChan gameChannel (GL.PlayerJoined player)
    ))
    -- Game loop and logic
    eitherGameResult <-
        (readChan gameChannel) >>=
        (\gameEvent -> GL.handleGameEvent gameEvent gameStateRef)

    catch (GL.gameLoop player gameStateRef gameChannel) (\e -> do
        putStrLn $ "Error in game session: " ++ show (e :: SomeException)
        PM.cleanupPlayer player
    )
