{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import System.Console.OptMatch
import System.Console.OptMatch.Popular
import System.Environment(getArgs)
import System.IO(hFlush, stdout)
import Web.Authenticate.OAuth(authorizeUrl, newCredential, Credential(..))
import qualified Data.ByteString.Char8 as BS
import Ftap.Transfer
import Ftap.Formatter

data Options = Options
               { isAuthorize :: Bool
               , consumerKey :: String
               , consumerSecret :: String
               , accessToken :: String
               , accessTokenSecret :: String
               , isPrintHomeTimeline :: Bool
               }
             deriving (Show, Eq)


main :: IO ()
main = do
  args <- getArgs
  case runOptMatch optionParser args of
    Just (opts, args) -> main' opts
    Nothing -> printUsage

optionParser :: OptMatch Options
optionParser = popular defaultOptions $ \opts ->
  opts { isAuthorize = True } <$ keyword "--authorize" <|>
  (\tkn scrt -> opts {
      accessToken = tkn
    , accessTokenSecret = scrt
      }) <$ keyword "--access-token" <*> argument <*> argument <|>
  (\key scrt -> opts {
      consumerKey = key
    , consumerSecret = scrt
      }) <$ keyword "--consumer-key" <*> argument <*> argument <|>
  opts { isPrintHomeTimeline = True } <$ keyword "--home-timeline"
  where
    defaultOptions = Options {
        isAuthorize = False
      , consumerKey = "hgIoke5ye0avWtWiM4COw"
      , consumerSecret = "CypWkvWfTJ6OFfyO15mgVj1HZh0h1Hy8oYgpjRWaQ"
      , accessToken = error "access token wasn't given."
      , accessTokenSecret = error "access token secret wasn't given."
      , isPrintHomeTimeline = False
        }

printUsage :: IO ()
printUsage = do
  putStrLn "Usage: ftap < --authorize | --access-token TOKEN SECRET >"
  putStrLn "            [ --consumer-key KEY SECRET ]"

main' :: Options -> IO ()
main' Options { isAuthorize = True, consumerKey, consumerSecret } = do
  let oauth = mkConsumer (BS.pack consumerKey) (BS.pack consumerSecret)
  (Credential map) <- authorize oauth $ \tmp -> do
    putStrLn $ authorizeUrl oauth tmp
    putStr "PIN: "
    hFlush stdout
    BS.getLine
  let Just accessToken = lookup "oauth_token" map
      Just accessTokenSecret = lookup "oauth_token_secret" map
      Just userId = lookup "user_id" map
      Just screenName = lookup "screen_name" map
  putStrLn $ BS.unpack accessToken
  putStrLn $ BS.unpack accessTokenSecret

main' opts = do
  let cnsm = mkConsumer (BS.pack $ consumerKey opts) (BS.pack $ consumerSecret opts)
      cred = newCredential (BS.pack $ accessToken opts) (BS.pack $ accessTokenSecret opts)
  json <- fetchHomeTL cnsm cred
  forM (formatStatuses json) $ \str -> do
    putStrLn str
  return ()
