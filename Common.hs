{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Common where

import qualified Web.Authenticate.OAuth as OA
import qualified Network.URI as URI
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S8
import qualified Data.CaseInsensitive as CI
import qualified Data.Configurator as Conf

import Web.Twitter.Conduit (TW, setCredential, twProxy, runTW)
import System.Environment (getEnvironment)
import Control.Lens
import Control.Applicative
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Network.HTTP.Conduit (Proxy(..))
import Data.Configurator.Types (Config)
import Web.Authenticate.OAuth (OAuth(..), Credential, newOAuth, newCredential, signOAuth)

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
    TW (ResourceT m) a ->
    m a
runTwitterFromEnv conf task = do
    pr <- liftBase getProxyEnv
    (oa, cred) <- liftBase $ getOAuthTokens conf
    let env = (setCredential oa cred OA.def) { twProxy = pr }
    runTW env task

runTwitterFromEnv' ::
    (MonadIO m, MonadBaseControl IO m) =>
    Config ->
    TW (ResourceT (NoLoggingT m)) a ->
    m a
runTwitterFromEnv' = (runNoLoggingT .) . runTwitterFromEnv
