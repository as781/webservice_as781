module Crawling (crawlThread) where

--import Network.Curl
import Network.HTTP.Conduit
import Data.ByteString.Lazy.Char8 (ByteString)
import Control.Concurrent
import Control.Exception
import Data.Either

import qualified Data.Map as Map
import Data.Acid  ( AcidState, query)
import Data
import Types

--Performs crawling for the web servers.
crawlThread :: AcidState Locations -> MVar [(String,ByteString)] -> IO ()
crawlThread acid mvar = 
    do
        --Put text data into mvar
        sites' <- query acid PeekAll 
        sites <- return $ Map.keys sites'              
        results <- mapM webGetString sites
        let ok_results = _processData $ zip sites results
        _showErrors $ zip sites results
        threadDelay 500000  
        putMVar mvar ok_results
        --wait 500 miliseconds
        --Recursion to make this an infinite loop
        crawlThread acid mvar
    where webGetString s = try (simpleHttp s) 

--A function that filters the error messages 
_processData :: [(String, Either SomeException ByteString)] -> [(String,ByteString)]
_processData eithers =  map (\(x,Right y) ->(x,y)) $ filter (isRight.snd) eithers

_showErrors :: [(String, Either SomeException ByteString)] -> IO ()
_showErrors eithers = mapM (\x -> putStrLn x) errors >> return ()
    where errors = map (\(x,Left y) -> x ++ " " ++ show y) $ filter (isLeft.snd) eithers
