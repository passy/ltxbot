{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveDataTypeable #-}
module Main where

import Prelude

import Control.Lens
import Control.Monad (liftM, when)
import Data.Data (Data)
import Data.Maybe (listToMaybe, isNothing, fromJust)
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import Paths_ltxbot (version)
import System.Console.CmdArgs.Explicit (HelpFormat(..), helpText)
import Web.Twitter.Conduit (stream, statusesFilterByTrack)
import Web.Twitter.LtxBot (actTL, normalizeMentions)
import Web.Twitter.LtxBot.Common (runTwitterFromEnv')

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Configurator as Conf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Console.CmdArgs.Implicit as CA

data Ltxbot = Ltxbot { config :: FilePath }
    deriving (Show, Data, Typeable)

programName :: String
programName = "ltxbot"

args :: CA.Mode (CA.CmdArgs Ltxbot)
args = CA.cmdArgsMode $ Ltxbot { config = CA.def CA.&= CA.args }
    CA.&= CA.summary (unwords [programName, showVersion version])
    CA.&= CA.program programName

main :: IO ()
main = do
    mainArgs <- CA.cmdArgsRun args
    let confFile = config mainArgs
    if null confFile
        then print $ helpText [] HelpFormatDefault args
        else runBot confFile

runBot :: FilePath -> IO ()
runBot confFile = do
    -- TODO: Instead of passing around conf, could
    -- I wrap all this in a ReaderT?
    -- UPDATE: Turns out TW is already a `type TW m = ReaderT TWEnv m`
    -- Does that help me?
    conf <- Conf.load [Conf.Required confFile]
    username <- Conf.lookupDefault "" conf "userName"

    maybeUid <- liftM (listToMaybe . T.split (== '-')) (Conf.lookupDefault "" conf "accessToken")
    let userId = fmap (read . T.unpack) maybeUid
    when (isNothing userId) $ error "accessToken must contain a '-'"

    T.putStrLn $ T.unwords ["Listening for Tweets to", username, "..."]
    runTwitterFromEnv' conf $ do
        src <- stream $ statusesFilterByTrack $ T.concat ["@", username]
        src C.$=+ normalizeMentions C.$$+- CL.mapM_ (^! act (actTL $ fromJust userId))
