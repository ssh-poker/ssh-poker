module Ssh where

import Network.Socket
import System.IO (Handle, hSetBuffering, BufferMode(..), hClose)
import qualified Data.ByteString.Char8 as B
import qualified Network.SSH.Server as SSH
import Control.Concurrent (forkIO)
import Control.Exception (bracket)

-- Configuration for SSH server
sshConfig :: SSH.Config
sshConfig = SSH.defaultConfig
    { SSH.configHostKey = ... -- Load or generate host key
    , SSH.configAuthMethods = [passwordAuthMethod]
    }

-- Public key authentication method
publicKeyAuthMethod :: SSH.AuthMethod
publicKeyAuthMethod = SSH.AuthMethod
    { SSH.methodName = "publickey"
    , SSH.methodInstructions = "Enter your public key"
    , SSH.methodAction = \_ -> return True
    }

-- Start the SSH server
startSshServer :: Int -> (SSH.Session -> IO ()) -> IO ()
startSshServer port sessionHandler = withSocketsDo $ do
    addrinfos <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing (Just $ show port)
    let serveraddr = head addrinfos
    bracket (socket (addrFamily serveraddr) Stream defaultProtocol)
            close
            (\sock -> do
                bind sock (addrAddress serveraddr)
                listen sock 2
                putStrLn $ "Listening on port " ++ show port
                acceptLoop sock
                SSH.serve sshConfig sock sessionHandler
            )

-- Session handler for SSH connections
sshSessionHandler :: SSH.Session -> IO ()
sshSessionHandler session = do
    -- Set up session
    hSetBuffering (SSH.sessionStdin session) NoBuffering
    hSetBuffering (SSH.sessionStdout session) NoBuffering
    hSetBuffering (SSH.sessionStderr session) NoBuffering
    -- Start a new thread for the session
    forkIO $ sessionLoop session
    return ()
