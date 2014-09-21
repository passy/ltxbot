{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Main where

import Prelude

import Control.Lens
import Control.Monad (liftM, when)
import Data.Maybe (listToMaybe, isNothing, fromJust)
import System.Environment (getArgs)
import Web.Twitter.Types (UserId)
import Web.Twitter.Conduit (stream, statusesFilterByTrack)
import Web.Twitter.LtxBot (actTL, normalizeMentions)
import Web.Twitter.LtxBot.Common (runTwitterFromEnv')

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Configurator as Conf
import qualified Data.Text as T

main :: IO ()
main = do
    [confFile] <- getArgs
    -- TODO: Instead of passing around conf, could
    -- I wrap all this in a ReaderT?
    -- UPDATE: Turns out TW is already a `type TW m = ReaderT TWEnv m`
    -- Does that help me?
    conf <- Conf.load [Conf.Required confFile]
    username <- Conf.lookupDefault "" conf "userName"

    maybeUid <- liftM (listToMaybe . T.split (== '-')) (Conf.lookupDefault "" conf "accessToken")
    let userId = fmap (read . T.unpack) maybeUid :: Maybe UserId
    when (isNothing userId) $ error "accessToken must contain a '-'"

    runTwitterFromEnv' conf $ do
        src <- stream $ statusesFilterByTrack $ T.concat ["@", username]
        -- TODO: Add conduit that removes mentions
        src C.$=+ normalizeMentions C.$$+- CL.mapM_ (^! act (actTL $ fromJust userId))

    return ()
