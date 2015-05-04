module WebService (htmlThread) where

import Control.Monad
import Happstack.Server

import Data.Acid  ( AcidState)

import HTML
import Data
import Types

--Runs the server.
htmlThread :: AcidState Locations -> IO ()
htmlThread acid = simpleHTTP nullConf $ msum [  handlers acid,
                                                home
                                             ]