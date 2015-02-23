{-# LANGUAGE QuasiQuotes #-}

module Web.Twitter.LtxBot.Types where

import Control.Monad.Reader (ReaderT)
import Network.HTTP.Conduit (Manager)
import Web.Twitter.Conduit (TWInfo)
import Web.Twitter.Types (UserId)

import qualified Record as R

type LtxbotEnv =
   [R.record| { userId :: UserId, twInfo :: TWInfo, manager :: Manager } |]

type LTXE m = ReaderT LtxbotEnv m
