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
import System.FilePath (replaceExtension)
import System.IO (hClose)
import Web.Twitter.Conduit (stream, statusesFilterByTrack, MediaData(..), updateWithMedia, call, TW, inReplyToStatusId)
import Web.Twitter.Types (StreamingAPI(..), Status(..))
import qualified Web.Twitter.Types.Lens as TL -- (AsStatus(..), userScreenName)
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
        src <- stream $ statusesFilterByTrack $ T.concat ["@", username]
        -- TODO: Add conduit that removes mentions
        src C.$$+- CL.mapM_ (^! act actTL)

    return ()

actTL ::
    (MonadLogger m, MonadResource m, MonadCatch m, MonadMask m) =>
    StreamingAPI ->
    TW m ()
actTL (SStatus s) = actStatus s
actTL _ = liftIO $ T.putStrLn "Other event"

actStatus :: (MonadLogger m, MonadResource m, MonadCatch m, MonadMask m) =>
    Status ->
    TW m ()
actStatus s = do
    liftIO $ T.putStrLn $ showStatus s
    withSystemTempFile "hatmp.tex" (\ tmpFile tmpHandle -> do
        -- Yuck, this is state, global state even. Let's figure out
        -- if this can be piped through stdin.
        _ <- liftIO $ do
            renderLaTeXToHandle tmpHandle (standaloneLaTeX (s ^. TL.text))
            hClose tmpHandle -- We can't write to the handle otherwise
            system $ unwords ["./docker-tex2png.sh", tmpFile]
        replyStatusWithImage s (replaceExtension tmpFile ".png"))

showStatus ::
    TL.AsStatus s =>
    s ->
    T.Text
showStatus s = T.concat [ s ^. TL.user . TL.userScreenName
                        , ": "
                        , s ^. TL.text
                        ]

replyStatusWithImage ::
    (MonadLogger m, MonadResource m) =>
    Status ->
    FilePath ->
    TW m ()
replyStatusWithImage status filepath = do
    -- TODO: Do something with res, don't return ()
    res <- call updateCall
    liftIO $ print res
    return ()
    where
        statusString = T.concat ["@", status ^. TL.user . TL.userScreenName]
        media = MediaFromFile filepath
        updateCall = updateWithMedia statusString media & inReplyToStatusId ?~ statusId status


extractStatusMentions :: Status -> [TL.UserEntity]
extractStatusMentions s = do
    -- Should be obvious that this needs to be refactored ...
    let ues = s ^. TL.statusEntities >>= (^? TL.enUserMentions)

    -- This should be just flattening two functors into one.
    -- The (ues >>= return) is stupid too. There's probably a lens operation
    -- for this.
    case ues >>= return . (fmap (^. TL.entityBody)) of
        Just n  -> n
        Nothing -> []
