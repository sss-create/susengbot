{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Simple
import Network.HTTP.Conduit

import Database.SQLite.Simple

import Data.Aeson
import Data.ByteString.UTF8 (fromString)
import Data.ByteString.Lazy (LazyByteString)
import Data.Time (getCurrentTime)

import Control.Monad (mzero)
import System.IO


data Cat = Cat 
  { catId :: !String
  , url   :: !String
  } deriving Show


instance FromJSON Cat where
  parseJSON (Object v) = 
    Cat <$> v .: "id"
        <*> v .: "url"
  parseJSON _ = mzero


telegramUrl :: String
telegramUrl = "https://api.telegram.org/bot"


readToken :: IO String
readToken = do
  handle <- openFile "data/token.txt" ReadMode
  token <- hGetLine handle
  hClose handle >> pure token


sendPhoto :: String -> String -> IO (Response LazyByteString)
sendPhoto token url = do
  request <- parseRequest $ telegramUrl ++ token ++ "/sendPhoto"
  let payload = [("chat_id", "1234"), ("photo", fromString url)]
  httpLBS $ urlEncodedBody payload request


storeCat :: String -> String -> IO ()
storeCat catId url = do
  conn <- open "data/cats.db"
  date <- getCurrentTime
  execute conn "INSERT INTO cats (id, url, date_added) VALUES (?, ?, ?)" (catId, url, date)


checkForEntry :: String -> IO Bool
checkForEntry catId = do
  conn <- open "data/cats.db"
  result <- query conn "SELECT id FROM cats WHERE id = ?" (Only catId) :: IO [Only String]
  pure . not . null $ result


handleCat :: String -> Cat -> IO ()
handleCat token (Cat catId catUrl) = do
  isInData <- checkForEntry catId
  if isInData then main
  else sendPhoto token catUrl >> storeCat catId catUrl


main :: IO ()
main = do
  token <- readToken
  response <- httpLBS "https://api.thecatapi.com/v1/images/search"
  let catResponse = decode $ getResponseBody response

  case catResponse of
    Just (cat:_) -> handleCat token cat
    _ -> pure ()
