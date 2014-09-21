{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Web.Twitter.LtxBot where

import Prelude

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Configurator as Conf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Web.Twitter.Types.Lens as TL
import qualified Web.Twitter.Types as TT

import Web.Twitter.LtxBot.Common (runTwitterFromEnv')
import Web.Twitter.LtxBot.Latex (renderLaTeXToHandle, standaloneLaTeX)
import Control.Monad (liftM, join)
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Catch (MonadCatch, MonadMask)
import Control.Lens
import Control.Applicative ((<$>))
import System.Environment (getArgs)
import System.IO.Temp (withSystemTempFile)
import System.FilePath (replaceExtension)
import System.IO (hClose)
import Web.Twitter.Conduit (stream, statusesFilterByTrack, MediaData(..), updateWithMedia, call, TW, inReplyToStatusId)
import Web.Twitter.Types (StreamingAPI(..), Status(..))
import System.Process (system)
import Data.Maybe (maybeToList)

normalizeMentions :: Monad m => C.Conduit StreamingAPI m StreamingAPI
normalizeMentions = C.await >>= maybe (return ()) C.yield

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
        allMentions = extractStatusMentions status
        -- TODO: Nopedeynope. Let's read the ID from TW as it's part of the
        -- accessToken
        otherMentions = filter (\u -> TT.userEntityUserId u /= (2810142194 :: TT.UserId)) allMentions
        mentionsString = T.unwords $ (\m -> T.concat ["@", m ^. TL.userEntityUserScreenName]) <$> otherMentions
        statusString = T.unwords [T.concat ["@", status ^. TL.user . TL.userScreenName], mentionsString]
        media = MediaFromFile filepath
        updateCall = updateWithMedia statusString media & inReplyToStatusId ?~ statusId status


extractStatusMentions :: Status -> [TL.UserEntity]
extractStatusMentions s = do
    -- Should be obvious that this needs to be refactored ...
    -- I'm sure there's a way to do all of this in a single combined
    -- lens operation
    let ues = s ^. TL.statusEntities >>= (^? TL.enUserMentions)
    let mentions = liftM (fmap (^. TL.entityBody)) ues
    join $ maybeToList mentions

