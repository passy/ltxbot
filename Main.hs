{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Main where

import Prelude

import Web.Twitter.LtxBot
import Web.Twitter.LtxBot.Common (runTwitterFromEnv')

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Configurator as Conf
import qualified Data.Text as T

import Control.Lens
import System.Environment (getArgs)
import Web.Twitter.Conduit (stream, statusesFilterByTrack)

main :: IO ()
main = do
    [confFile] <- getArgs
    -- TODO: Instead of passing around conf, could
    -- I wrap all this in a ReaderT?
    -- UPDATE: Turns out TW is already a `type TW m = ReaderT TWEnv m`
    -- Does that help me?
    conf <- Conf.load [Conf.Required confFile]
    username <- Conf.lookupDefault "" conf "userName"

    runTwitterFromEnv' conf $ do
        src <- stream $ statusesFilterByTrack $ T.concat ["@", username]
        -- TODO: Add conduit that removes mentions
        src C.$=+ normalizeMentions C.$$+- CL.mapM_ (^! act actTL)

    return ()
