module Web.Twitter.LtxBot.Types where

import Control.Monad.Reader (ReaderT)
import Network.HTTP.Conduit (Manager)
import Web.Twitter.Conduit (TWInfo)
import Web.Twitter.Types (UserId)

data LtxbotEnv = LtxbotEnv { envUserId :: UserId
                           , envTwInfo :: TWInfo
                           , envManager :: Manager }

type LTXE m = ReaderT LtxbotEnv m
