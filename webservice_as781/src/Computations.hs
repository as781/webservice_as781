module Computations (
    rangeFromMin, rangeToMax, 
    range, cAverage, cMin, cMax) where

import Data.Time
import Types

import qualified Data.Set as Set



--Gets measurements with time < max
rangeToMax :: UTCTime -> Set.Set Measurement -> Set.Set Measurement
rangeToMax max_t s_ms = fst $ Set.split max_ms s_ms
    where max_ms = Measurement max_t 0

--Gets measurement with time > max
rangeFromMin :: UTCTime -> Set.Set Measurement -> Set.Set Measurement
rangeFromMin min_t s_ms = snd $ Set.split min_ms s_ms
    where min_ms = Measurement min_t 0

--New measurements: min < measurements < max
range :: UTCTime -> UTCTime -> Set.Set Measurement -> Set.Set Measurement
range min_t max_t s_ms = set2
    where min_ms = Measurement min_t 0
          max_ms = Measurement max_t 0
          (_    , set1) = Set.split min_ms s_ms
          (set2 , _   ) = Set.split max_ms set1

--Computes average
cAverage :: Set.Set Measurement -> Double
cAverage s_ms = (sum tmps) / (fromIntegral $ length tmps)
    where tmps = map temperature $  Set.toList s_ms

--Computes min
cMax :: Set.Set Measurement -> Double
cMax s_ms = foldl1 max tmps
    where tmps = map temperature $  Set.toList s_ms

--Computes max
cMin :: Set.Set Measurement -> Double
cMin s_ms = foldl1 min tmps
    where tmps = map temperature $  Set.toList s_ms  
