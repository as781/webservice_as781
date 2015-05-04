{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Parsing where

import Data.ByteString.Lazy.Char8 (ByteString)
import Control.Concurrent
import Control.Applicative
import Control.Monad
import Data.Either

import Data.Aeson
import Types

import Data.Time
import System.Locale
import Data.Text (Text, unpack)

--Converts the JSON data to UTCTime
strToUTCTime :: Text -> Maybe UTCTime
strToUTCTime t = parseTime defaultTimeLocale format s
    where format = "%FT%X%z"
          s = unpack t

--Parses JSON data.
instance FromJSON Measurement where
    parseJSON (Object v) =
        do
            time_str <- v .: "date"
            let time = case time_str of
                    String st -> strToUTCTime st
                    _ -> Nothing
            case time of
                Just t -> Measurement <$>
                    pure t <*>
                    v .: "temperature"
                Nothing -> mzero
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

--Derives the instance of FromJSON typeclasses to generics.
instance FromJSON Temperatures


--Tries to parse a given string into Haskell data structure.
parse :: (String,ByteString) -> Either String (String, Temperatures)
parse (web,s) = 
    case eitherDecode s of
        Left err -> Left err
        Right ok -> Right (web,ok)


--Parsing thread.
parseThread :: MVar [(String,ByteString)] -> MVar [(String,Temperatures)] -> IO()
parseThread m_strs m_ms = 
    do
        -- Take raw JSONs
        strs <- takeMVar m_strs
            -- Parse JSONs
        let ms' = map parse strs
            -- Filter the wrong data, and obtain measurements
            ms  = map (\(Right x) -> x) $ filter isRight ms'
            -- Get the number of errors, perhaps use it later.
            --errors = length $ filter isLeft ms'
        --Putting the data forward
        putMVar m_ms ms
        --Recursion for infinite loop.
        parseThread m_strs m_ms