-- Language extensions that allow to derive Generic and Typeable instances.
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Types where

import GHC.Generics
import Data.Data            ( Data, Typeable )
import qualified Data.Set as Set
import Data.Time
import qualified Data.Map as Map


-- |Map structure that binds website locations to temperatures.
type Locations = Map.Map String Temperatures

-- |Temperatures data type.
data Temperatures = Temperatures {
	temperatures :: Set.Set Measurement
} deriving (Read,Show,Generic,Data,Typeable)

-- |An empty Temperature
emptyTemp :: Temperatures
emptyTemp = Temperatures Set.empty

-- |Measurement data type.
--  Stores date in String format.
data Measurement = Measurement {
	date           :: UTCTime,
	temperature    :: Double
} deriving (Read,Show,Generic,Data,Typeable)

instance Eq Measurement where
    m1 == m2 = date1 == date2
        where date1 = date m1
              date2 = date m2 

instance Ord Measurement where
    compare m1 m2 = compare date1 date2
        where date1 = date m1
              date2 = date m2 