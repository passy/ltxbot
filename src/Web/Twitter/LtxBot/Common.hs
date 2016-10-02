{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Web.Twitter.LtxBot.Common where

import qualified Data.ByteString.Char8 as S8
import qualified Data.CaseInsensitive as CI
import qualified Data.Configurator as Conf
import qualified Data.Map as M
import qualified Network.URI as URI
import qualified Web.Authenticate.OAuth as OA

import Control.Applicative ((<$>), (<|>), (<*>))
import Control.Lens
import Data.Configurator.Types (Config)
import Network.HTTP.Conduit (Proxy(..))
import System.Environment (getEnvironment)
import Web.Authenticate.OAuth (OAuth(..), Credential, newOAuth, newCredential)
import Web.Twitter.Conduit (setCredential, twProxy, TWInfo)
import Data.Monoid ((<>))

getProxyEnv ::
    IO (Maybe Proxy)
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
    parsePort xs       = error $ "port number parse failed " <> xs

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

getTWInfoFromEnv ::
    Config ->
    IO TWInfo
getTWInfoFromEnv conf = do
    pr <- getProxyEnv
    (oa, cred) <- getOAuthTokens conf
    return $ (setCredential oa cred OA.def) { twProxy = pr }
