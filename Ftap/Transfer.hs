module Ftap.Transfer where

import Data.Conduit
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as JSON
import Network.HTTP.Conduit
import Network.HTTP.Types
import Web.Authenticate.OAuth

mkConsumer :: BS.ByteString -> BS.ByteString -> OAuth
mkConsumer consumerKey consumerSecret = newOAuth
    { oauthServerName = "twitter"
    , oauthRequestUri = "https://twitter.com/oauth/request_token"
    , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
    , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
    , oauthSignatureMethod = HMACSHA1
    , oauthConsumerKey = consumerKey
    , oauthConsumerSecret = consumerSecret
    , oauthVersion         = OAuth10a
    }

authorize :: OAuth -> (Credential -> IO BS.ByteString) -> IO Credential
authorize oauth getVerifier = do
  tmp <- withManager $ getTemporaryCredential oauth
  verifier <- getVerifier tmp
  let tmp' = injectVerifier verifier tmp
  cred <- withManager $ getTokenCredential oauth tmp'
  return cred

endpoint x = "https://api.twitter.com/1.1/" ++ x ++ ".json"

fetch :: Method -> OAuth -> Credential -> String -> SimpleQuery -> IO JSON.Value
fetch mth oauth cred p q = withManager $ \man -> do
  req <- parseUrl (endpoint p)
  req' <- signOAuth oauth cred (req {method=mth,queryString = renderSimpleQuery True q})
  res <- httpLbs req' man
  maybe (fail "JSON decoding error") return $ JSON.decode (responseBody res)

fetchGET = fetch methodGet
fetchPOST = fetch methodPost

fetchHomeTL oauth cred = fetchGET oauth cred "statuses/home_timeline" []
