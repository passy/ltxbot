{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Web.Twitter.LtxBot where

import Prelude

import qualified Data.Conduit as C
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Web.Twitter.Types.Lens as TL
import qualified Web.Twitter.Types as TT

import Web.Twitter.LtxBot.Latex (renderLaTeXToHandle, standaloneLaTeX)
import Control.Monad (liftM, join)
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Catch (MonadCatch, MonadMask)
import Control.Lens
import Control.Applicative ((<$>))
import System.IO.Temp (withSystemTempFile)
import System.FilePath (replaceExtension)
import System.IO (hClose)
import Web.Twitter.Conduit (MediaData(..), updateWithMedia, call, TW, inReplyToStatusId)
import Web.Twitter.Types (StreamingAPI(..), Status(..), UserId)
import System.Process (system)
import Data.Maybe (maybeToList)

-- | Remove all mentions from StreamingAPI SStatus messages
-- so that this bot doesn't have to deal with it further down the line.
normalizeMentions :: (MonadIO m) => C.Conduit StreamingAPI m StreamingAPI
normalizeMentions = C.awaitForever handleStream
    where
        handleStream (SStatus s) = do
            let text = s ^. TL.statusText
            -- WIZARDRY!
            let mentions = s ^.. TL.statusEntities
                             . _Just
                             . TL.enUserMentions
                             . traverse
                             . TL.entityIndices
            -- TODO: Strip out the mentions. Can't think of a good algorithm
            -- right now ... Too tired.
            let newText = foldl (const . id) text []
            C.yield $ SStatus (s & TL.statusText .~ newText)
        handleStream s@_          = C.yield s

-- | Strip the entities defined by the given indices.
stripEntities :: [TT.EntityIndices] -> T.Text -> T.Text
stripEntities i t = t

actTL ::
    (MonadLogger m, MonadResource m, MonadCatch m, MonadMask m) =>
    UserId ->
    StreamingAPI ->
    TW m ()
actTL u (SStatus s) = actStatus u s
actTL _ _ = liftIO $ T.putStrLn "Other event"

actStatus :: (MonadLogger m, MonadResource m, MonadCatch m, MonadMask m) =>
    UserId ->
    Status ->
    TW m ()
actStatus uid s = do
    liftIO $ T.putStrLn $ showStatus s
    withSystemTempFile "hatmp.tex" (\ tmpFile tmpHandle -> do
        -- Yuck, this is mutable state, global mutable state even. Let's figure
        -- out if this can be piped through stdin.
        _ <- liftIO $ do
            renderLaTeXToHandle tmpHandle (standaloneLaTeX (s ^. TL.text))
            hClose tmpHandle -- We can't write to the handle otherwise
            system $ unwords ["./docker-tex2png.sh", tmpFile]
        replyStatusWithImage uid s (replaceExtension tmpFile ".png"))

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
    UserId ->
    Status ->
    FilePath ->
    TW m ()
replyStatusWithImage uid status filepath = do
    -- TODO: Do something with res, don't return ()
    res <- call updateCall
    liftIO $ print res
    return ()
    where
        allMentions = extractStatusMentions status
        otherMentions = filter (\u -> TT.userEntityUserId u /= uid) allMentions
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
