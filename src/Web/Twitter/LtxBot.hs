{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Web.Twitter.LtxBot where

import Prelude

import qualified Data.Conduit as C
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Web.Twitter.Types.Lens as TL
import qualified Web.Twitter.Types as TT

import Data.Aeson (FromJSON)
import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad (join)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Reader.Class (asks)
import Data.Maybe (maybeToList)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (readProcessWithExitCode)
import Web.Twitter.Conduit (MediaData(..), APIRequest, updateWithMedia, call, update)
import Web.Twitter.Conduit.Parameters (inReplyToStatusId)
import Web.Twitter.LtxBot.Latex (renderLaTeXStatus)
import Web.Twitter.LtxBot.Types (LTXE, LtxbotEnv(..))
import Web.Twitter.Types (StreamingAPI(..), Status(..), UserId)

-- | Remove all mentions from StreamingAPI SStatus messages
-- so that this bot doesn't have to deal with it further down the line.
normalizeMentions :: (MonadIO m) => C.ConduitT StreamingAPI StreamingAPI m ()
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
            let newText = stripEntities mentions text
            C.yield $ SStatus (s & TL.statusText .~ newText)
        handleStream ss = C.yield ss

-- | Strip the entities defined by the given indices.
-- Indices have to be tuples of two, must be in order and most not overlap.
-- Dependent types would be totally rad here. Also a proper EntityIndices type.
stripEntities :: [TT.EntityIndices] -> T.Text -> T.Text
stripEntities i t =
    -- Read this backwards: Create a string annotated with its index,
    -- then filter by the ranges of characters to exclude and put it back
    -- together.
    (T.pack . fmap snd) . filter (\ e -> fst e `notElem` excludeRange) $ zip [0..] (T.unpack t)
    where
        -- These are all indices of the original string we want to avoid.
        excludeRange :: [Int]
        excludeRange = join [[x..y] | [x, y] <- i]

actTL ::
    (MonadResource m, MonadMask m) =>
    StreamingAPI ->
    LTXE m ()
actTL (SStatus s) = actStatus s
actTL _ = liftIO $ T.putStrLn "Other event"

actStatus :: (MonadResource m, MonadMask m) =>
    Status ->
    LTXE m ()
actStatus s = do
    uid <- asks envUserId
    let content = T.unpack $ renderLaTeXStatus s
    withSystemTempFile "hatmp.png" (\ tmpFile tmpHandle -> do
        -- Yuck, this is mutable state, global mutable state even. Let's figure
        -- out if this can be piped through stdin.
        liftIO $ do
            T.putStrLn $ showStatus s
            hClose tmpHandle -- The runtime maintains a lock on the file otherwise.

        (status, _, _) <- liftIO $ readProcessWithExitCode "./docker-tex2png.sh" [tmpFile] content
        case status of
            ExitSuccess   -> replyStatusWithImage uid s tmpFile
            ExitFailure _ -> replyStatusWithError s)

showStatus ::
    TL.AsStatus s =>
    s ->
    T.Text
showStatus s = T.concat [ s ^. TL.user . TL.userScreenName
                        , ": "
                        , s ^. TL.text
                        ]

call' ::
    (MonadIO m, MonadResource m, FromJSON responseType) =>
     APIRequest apiName responseType ->
     LTXE m responseType
call' request = do
    twInfo <- asks envTwInfo
    mngr <- asks envManager
    liftIO $ call twInfo mngr request

replyStatusWithError ::
    (MonadResource m) =>
    Status ->
    LTXE m ()
replyStatusWithError status = do
    res <- call' updateCall
    liftIO $ print res
    where
        errorMessage :: T.Text
        errorMessage = "Sorry, I could not compile your LaTeX, friend."

        statusString = T.unwords [T.concat ["@", status ^. TL.user . TL.userScreenName], errorMessage]
        updateCall   = update statusString & inReplyToStatusId ?~ statusId status

replyStatusWithImage ::
    (MonadResource m) =>
    UserId ->
    Status ->
    FilePath ->
    LTXE m ()
replyStatusWithImage uid status filepath = do
    -- TODO: Do something with res, don't return ()
    res <- call' updateCall
    liftIO $ print res
    where
        allMentions    = extractStatusMentions status
        otherMentions  = filter (\u -> TT.userEntityUserId u /= uid) allMentions
        mentionsString = T.unwords $ (\m -> T.concat ["@", m ^. TL.userEntityUserScreenName]) <$> otherMentions
        statusString   = T.unwords [T.concat ["@", status ^. TL.user . TL.userScreenName], mentionsString]
        media          = MediaFromFile filepath
        updateCall     = updateWithMedia statusString media & inReplyToStatusId ?~ statusId status

extractStatusMentions :: Status -> [TL.UserEntity]
extractStatusMentions s = do
    -- Should be obvious that this needs to be refactored ...
    -- I'm sure there's a way to do all of this in a single combined
    -- lens operation
    let ues      = s ^. TL.statusEntities >>= (^? TL.enUserMentions)
    let mentions = fmap (fmap (^. TL.entityBody)) ues
    join $ maybeToList mentions
