{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Main where

import Prelude

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Configurator as Conf
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Common (runTwitterFromEnv')
import Latex (renderLaTeXToHandle, standaloneLaTeX)
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Catch (MonadCatch, MonadMask)
import Control.Lens
import System.Environment (getArgs)
import System.IO.Temp (withSystemTempFile)
import System.IO (hFlush, hClose)
import Web.Twitter.Conduit (stream, statusesFilterByTrack, MediaData(..), updateWithMedia, call, TW, inReplyToStatusId)
import Web.Twitter.Types (StreamingAPI(..), Status(..))
import Web.Twitter.Types.Lens (AsStatus(..), userScreenName)
import System.Process (system)

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
        -- TODO: Add conduit that removes mentions
        src C.$$+- CL.mapM_ (^! act actTL)

    return ()

actTL ::
    (MonadLogger m, MonadResource m, MonadCatch m, MonadMask m) =>
    StreamingAPI ->
    TW m ()
actTL (SStatus s) = do
    liftIO $ T.putStrLn $ showStatus s
    withSystemTempFile "hatmp.tex" (\ tmpFile tmpHandle -> do
        -- Yuck, this is state, global state even. Let's figure out
        -- if this can be piped through stdin.
        _ <- liftIO $ do
            renderLaTeXToHandle tmpHandle (standaloneLaTeX (s ^. text))
            hClose tmpHandle -- We can't write to the handle otherwise
            system $ unwords ["./docker-tex2png.sh", tmpFile]
        replyStatusWithImage s (replaceTexWithPng tmpFile))
    where
        -- Should only replace the file ending, there's probably a
        -- way of only getting the base name, but should suffice for now.
        replaceTexWithPng :: FilePath -> FilePath
        replaceTexWithPng = T.unpack . (T.replace ".tex" ".png") . T.pack
actTL _ = liftIO $ T.putStrLn "Other event"

showStatus ::
    AsStatus s =>
    s ->
    T.Text
showStatus s = T.concat [ s ^. user . userScreenName
                        , ": "
                        , s ^. text
                        ]

replyStatusWithImage ::
    (MonadLogger m, MonadResource m) =>
    Status ->
    FilePath ->
    TW m ()
replyStatusWithImage status filepath = do
    -- TODO: Do something with res, don't return ()
    res <- call $ updateCall
    liftIO $ print res
    return ()
    where
        statusString = (T.concat ["@", status ^. user . userScreenName])
        media = MediaFromFile filepath
        updateCall = updateWithMedia statusString media & inReplyToStatusId ?~ statusId status
