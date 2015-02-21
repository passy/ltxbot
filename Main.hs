{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveDataTypeable #-}
module Main where

import Prelude

import Control.Lens.Action
import Control.Monad (when)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Data.Data (Data)
import Data.Maybe (listToMaybe, isNothing, fromJust)
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import Paths_ltxbot (version)
import System.Console.CmdArgs.Explicit (HelpFormat(..), helpText)
import Web.Twitter.Conduit (stream, statusesFilterByTrack)
import Web.Twitter.LtxBot (actTL, normalizeMentions)
import Web.Twitter.LtxBot.Common (getTWInfoFromEnv, LtxbotEnv(..))

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Configurator as Conf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.HTTP.Conduit as HTTP
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
    conf <- Conf.load [Conf.Required confFile]
    username <- Conf.lookupDefault "" conf "userName"

    twInfo <- getTWInfoFromEnv conf

    -- TODO: Remove.
    maybeUid <- liftIO $ fmap (listToMaybe . T.split (== '-')) (Conf.lookupDefault "" conf "accessToken")
    -- Just userId' <- ...?
    let userId' = fmap (read . T.unpack) maybeUid
    when (isNothing userId') $ error "accessToken must contain a '-'"

    HTTP.withManager $ \mngr -> do
        liftIO . T.putStrLn $ T.unwords ["Listening for Tweets to", username, "..."]

        let lenv = LtxbotEnv { ltxeUserId = fromJust userId'
                             , ltxeTwInfo = twInfo
                             , ltxeMngr = mngr }

        src <- stream twInfo mngr (statusesFilterByTrack $ T.concat ["@", username])
        src C.$=+ normalizeMentions C.$$+- CL.mapM_ (^! act ((`runReaderT` lenv) . actTL))
    return ()
