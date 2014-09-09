{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Main where

import Prelude

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Configurator as Conf
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Common (runTwitterFromEnv')
-- import Latex (renderLaTeXToFile)
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Logger (MonadLogger)
import Control.Lens
import System.Environment (getArgs)
import Web.Twitter.Conduit (stream, statusesFilterByTrack, MediaData(..), updateWithMedia, call, TW, inReplyToStatusId, update)
import Web.Twitter.Types (StreamingAPI(..), Status(..))
import Web.Twitter.Types.Lens (AsStatus(..), userScreenName)

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
        src <- stream $ statusesFilterByTrack username
        src C.$$+- CL.mapM_ (^! act printTL)

    return ()

-- TODO: That's not at all what it does now.
printTL ::
    (MonadLogger m, MonadResource m) =>
    StreamingAPI ->
    TW m ()
printTL (SStatus s) = do
    liftIO $ T.putStrLn $ showStatus s
    replyToStatus s
printTL _ = liftIO $ T.putStrLn "Other event"

showStatus ::
    AsStatus s =>
    s ->
    T.Text
showStatus s = T.concat [ s ^. user . userScreenName
                        , ":"
                        , s ^. text
                        ]

replyToStatus ::
    (MonadLogger m, MonadResource m) =>
    Status ->
    TW m ()
replyToStatus s = do
    -- TODO: Do something with res, don't return ()
    _ <- call $ update (T.concat ["@", s ^. user . userScreenName, " Hello World"]) & inReplyToStatusId ?~ (statusId s)
    return ()

updateStatusWithImage ::
    (MonadLogger m, MonadResource m) =>
    String ->
    FilePath ->
    TW m ()
updateStatusWithImage status filepath = do
    res <- call $ updateWithMedia (T.pack status) (MediaFromFile filepath)
    liftIO $ print res
    return ()
