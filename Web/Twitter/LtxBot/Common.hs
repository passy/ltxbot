{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Web.Twitter.LtxBot.Common where

import qualified Data.ByteString.Char8 as S8
import qualified Data.CaseInsensitive as CI
import qualified Data.Configurator as Conf
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Network.URI as URI
import qualified Web.Authenticate.OAuth as OA

import Control.Applicative ((<$>), (<|>), (<*>))
import Control.Lens
import Control.Monad (liftM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe, isNothing, fromJust)
import Control.Monad.Base (liftBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Resource (ResourceT, MonadBaseControl)
import Data.Configurator.Types (Config)
import Network.HTTP.Conduit (Proxy(..))
import System.Environment (getEnvironment)
import Web.Authenticate.OAuth (OAuth(..), Credential, newOAuth, newCredential)
import Web.Twitter.Conduit (TW, setCredential, twProxy, runTW)
import Web.Twitter.Types (UserId)

data LtxbotEnv = LtxbotEnv { userId :: UserId }
    deriving (Show)

-- | My Reader thingy to pass around the environment implicitly,
--   the same way the Conduit TW env is used, but wrapping it.
type LTXE m = ReaderT LtxbotEnv (TW m)

getProxyEnv :: IO (Maybe Proxy)
getProxyEnv = do
    env <- M.fromList . over (mapped . _1) CI.mk <$> getEnvironment
    let u = M.lookup "https_proxy" env <|>
            M.lookup "http_proxy" env <|>
            M.lookup "proxy" env >>= URI.parseURI >>= URI.uriAuthority
    return $ Proxy <$> (S8.pack . URI.uriRegName <$> u) <*> (parsePort . URI.uriPort <$> u)
  where
    parsePort :: String -> Int
    parsePort []       = 8080
    parsePort (':':xs) = read xs
    parsePort xs       = error $ "port number parse failed " ++ xs

getOAuthTokens ::
    Config ->
    IO (OAuth, Credential)
getOAuthTokens conf = do
    oauth <- makeOAuth
    cred <- makeCredential

    return (oauth, cred)

    where
        makeOAuth = do
            key <- Conf.lookupDefault "" conf "oauthConsumerKey"
            secret <- Conf.lookupDefault "" conf "oauthConsumerSecret"
            return $ newOAuth {
                oauthConsumerKey = key,
                oauthConsumerSecret = secret
            }
        makeCredential = do
            token <- Conf.lookupDefault "" conf "accessToken"
            secret <- Conf.lookupDefault "" conf "accessSecret"
            return $ newCredential token secret

runTwitterFromEnv ::
    (MonadIO m, MonadBaseControl IO m) =>
    Config ->
    LTXE (ResourceT m) a ->
    m a
runTwitterFromEnv conf task = do
    -- TODO: Store in env, too.
    -- username <- Conf.lookupDefault "" conf "userName"

    maybeUid <- liftIO $ liftM (listToMaybe . T.split (== '-')) (Conf.lookupDefault "" conf "accessToken")
    let userId = fmap (read . T.unpack) maybeUid
    when (isNothing userId) $ error "accessToken must contain a '-'"
    let lenv = LtxbotEnv $ fromJust userId

    pr <- liftBase getProxyEnv
    (oa, cred) <- liftBase $ getOAuthTokens conf
    let tenv = (setCredential oa cred OA.def) { twProxy = pr }
    runTW tenv (runReaderT task lenv)

runTwitterFromEnv' ::
    (MonadIO m, MonadBaseControl IO m) =>
    Config ->
    LTXE (ResourceT (NoLoggingT m)) a ->
    m a
runTwitterFromEnv' = (runNoLoggingT .) . runTwitterFromEnv
