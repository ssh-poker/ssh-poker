module Auth
    ( authenticatePlayer
    , PlayerCredentials
    , AuthError
    ) where

import qualified Ssh as S
import Data.Text (Text)

-- Data type for player credentials
data PlayerCredentials = PlayerCredentials {
    username :: Text,
    password :: Text
} deriving (Show, Eq)

-- Data type for authentication errors
data AuthError = InvalidCredentials | ConnectionError | OtherAuthError Text
    deriving (Show, Eq)

-- Function to authenticate a player
authenticatePlayer :: S.SSH.Connection -> IO (Either AuthError PlayerCredentials)
authenticatePlayer conn = do
    -- Check public key
    publicKey <- S.prompt conn "Enter your public key: "
    -- Check credentials
    if username == "test" && password == "test"
        then return $ Right $ PlayerCredentials username password
        else return $ Left InvalidCredentials
    return $ Right $ PlayerCredentials username password

