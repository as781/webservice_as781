module Main where



import Control.Concurrent
import Crawling
import Parsing
import Data
import WebService
import Types 

--Map data structure, used with Map. namespace.
import qualified Data.Map as Map


--Hardcoded list of websites to crawl through
websites :: [String]
websites = 
	[ "http://www.phoric.eu/temperature.json"
	]

--Changes the initial list of websites into the Locations state.
toLocations :: [String] -> Locations
toLocations webs = Map.fromList locs
    where locs = map (\x -> (x, emptyTemp)) webs

--Infinite loop
delayMyself :: Int -> IO ()
delayMyself n = do
	threadDelay n
	delayMyself n

-- The main program
main :: IO ()
main = 
    do
        --Set up persistent data storage.
        acid <- theAcid (toLocations websites)
        -- Setting up the first few mvars
        parse_store_mvar <- newEmptyMVar
        crawl_parse_mvar <- newEmptyMVar
        -- Forking the threads
        _ <- forkIO (crawlThread acid crawl_parse_mvar)
        _ <- forkIO (parseThread crawl_parse_mvar parse_store_mvar)
        _ <- forkIO (storageThread acid parse_store_mvar)
        _ <- forkIO (htmlThread acid)
        -- Delaying the main thread
        delayMyself 500000
