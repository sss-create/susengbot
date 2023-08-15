{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.UTF8 (fromString)
import Data.Aeson
import Data.Time (getCurrentTime)
import Control.Monad (mzero)
import System.IO
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Database.SQLite.Simple


data Cat = Cat {cat_id :: String, url :: String} deriving Show


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
    hClose handle
    return token


sendPhoto :: String -> String -> IO ()
sendPhoto token url = do
    request <- parseRequest $ telegramUrl ++ token ++ "/sendPhoto"
    let payload = [("chat_id", "1234"), ("photo", fromString url)]
    response <- httpLBS $ urlEncodedBody payload request
    print response


storeCat :: String -> String -> IO ()
storeCat catId url = do
    conn <- open "data/cats.db"
    date <- getCurrentTime
    execute conn "INSERT INTO cats (id, url, date_added) VALUES (?, ?, ?)" (catId, url, date)


checkForEntry :: String -> IO Bool
checkForEntry catId = do
    conn <- open "data/cats.db"
    result <- query conn "SELECT id FROM cats WHERE id = ?" (Only catId) :: IO [Only String]
    return (not (null result))


main :: IO ()
main = do
    token <- readToken
    -- decoded response e.g.: Just [Cat {cat_id = "d5u", url = "https://cdn2.thecatapi.com/images/d5u.jpg"}]
    response <- httpLBS "https://api.thecatapi.com/v1/images/search"
    let catResponse = decode $ getResponseBody response :: Maybe [Cat]
    case catResponse of
        Just (cat:_) -> do
            isInData <- checkForEntry (cat_id cat)
            if isInData
                then main
                else do
                    sendPhoto token (url cat)
                    storeCat (cat_id cat) (url cat)
        _ -> putStrLn "Could not parse JSON"
