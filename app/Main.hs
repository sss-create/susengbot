{-# LANGUAGE OverloadedStrings #-}

module Main where


import Database.SQLite.Simple

import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL

import Data.Aeson.Micro

import Data.ByteString.Char8 as B hiding (null)
import Data.ByteString.Lazy (LazyByteString)

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Data.Time (getCurrentTime)

import System.IO


data Cat = Cat
  { catId :: !T.Text
  , url   :: !T.Text
  } deriving Show


instance FromJSON Cat where
  parseJSON (Object v) =
    Cat <$> v .: "id"
        <*> v .: "url"
  parseJSON _ = fail "json parse err"


telegramUrl :: B.ByteString
telegramUrl = "https://api.telegram.org/bot"


readToken :: IO B.ByteString
readToken = withFile "data/token.txt" ReadMode B.hGetLine


sendPhoto :: B.ByteString -> B.ByteString -> IO (Response LazyByteString)
sendPhoto token url = do
  manager <- newOpenSSLManager
  initRequest <- parseRequest $ B.unpack (telegramUrl <> token <> "/sendPhoto")
  let body = [("chat_id", "1234"), ("photo", url)]
  httpLbs (urlEncodedBody body initRequest) manager


storeCat :: T.Text -> T.Text -> IO ()
storeCat catId url = do
  conn <- open "data/cats.db"
  date <- getCurrentTime
  execute conn "INSERT INTO cats (id, url, date_added) VALUES (?, ?, ?)" (catId, url, date)


checkForEntry :: T.Text -> IO Bool
checkForEntry catId = do
  conn <- open "data/cats.db"
  result <- query conn "SELECT id FROM cats WHERE id = ?" (Only catId) :: IO [Only T.Text]
  pure . not . null $ result


-- TODO: do not simply call main again
handleCat :: B.ByteString -> Cat -> IO ()
handleCat token (Cat catId catUrl) = do
  isInData <- checkForEntry catId
  if isInData then main
  else sendPhoto token (encodeUtf8 catUrl) >> storeCat catId catUrl


-- manager is created twice (sendPhoto); maybe use Reader Monad?
main :: IO ()
main = do
  token <- readToken
  manager <- newOpenSSLManager
  response <- httpLbs "https://api.thecatapi.com/v1/images/search" manager
  let catResponse = decode $ responseBody response

  case catResponse of
    Just (cat:_) -> handleCat token cat
    _ -> pure ()

