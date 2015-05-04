-- These are language extensions taken from a tutorial of Acid-state.
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards, PatternGuards, TypeSynonymInstances, FlexibleInstances #-}

module Data where
--Imports are also from the tutorial.
import Control.Monad        ( msum )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Control.Applicative (optional)
import Data.Data            ( Typeable )
import Happstack.Server     
import Data.Acid            ( AcidState, Query, Update, IsAcidic
                            , makeAcidic, openLocalState, createCheckpoint )
import Data.Acid.Advanced   ( query', update' )
import Data.SafeCopy        ( base, deriveSafeCopy )

--Own imports
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import qualified Data.Set as Set
import qualified Data.Map as Map

--Time
import Data.Time
import System.Locale 

--Own modules.
import Types
import HTML
import Computations


--Implements SafeCopy instances Measurement, Temperatures and Location
--Using Template Haskell
$(deriveSafeCopy 0  'base ''Measurement)
$(deriveSafeCopy 0  'base ''Temperatures)

--Creates an initial state.
theAcid :: (IsAcidic a, Typeable a ) => a -> IO (AcidState a)
theAcid state = openLocalState state


-- Pure function which updates the temperatures, using own data structures.
updateTemperatures :: Set.Set Measurement -> Temperatures -> Temperatures
updateTemperatures s_ms1 (Temperatures s_ms2) = Temperatures (Set.union s_ms2 s_ms1) 


-- Acid-state function which updates temperatures to a location
updateLocations :: String -> Set.Set Measurement -> Update Locations ()
updateLocations loc new =
    do  c <- get
        let newLocations = Map.adjust (updateTemperatures new) loc c
        put $ newLocations
        return ()

-- Acid-state function which adds new location
newLocation :: String -> Update Locations ()
newLocation key = do
    c <- get
    let newLocations = Map.alter ((\_ -> Just emptyTemp)) key c 
    put $ newLocations
    return ()

-- Acid-state function which returns the whole data.
peekAll :: Query Locations Locations
peekAll = ask

--Creates types out of updateLocations, newLocation and peekAll functions.
--Using TemplateHaskell
$(makeAcidic ''Locations ['updateLocations, 'peekAll, 'newLocation])




--Some additional code to handle time ranges.
--The time ranges are in yyyy-mm-dd format

--Parses time from yyyy-mm-dd format to UTCTime
toUTCTime :: String -> Maybe UTCTime
toUTCTime s = parseTime defaultTimeLocale format s
    where format = "%F"

--Checks whenever the time can be parsed.
checkTimes :: Maybe String -> Maybe String -> (Maybe UTCTime, Maybe UTCTime)
checkTimes m_st1 m_st2 = (left,right)
    where left  = m_st1 >>= toUTCTime
          right = m_st2 >>= toUTCTime

--Performs filtering on data.
ranged :: Maybe UTCTime -> Maybe UTCTime -> Set.Set Measurement -> Set.Set Measurement
ranged (Nothing)    (Nothing)    s_ms = s_ms
ranged (Just min_t) (Nothing)    s_ms = rangeFromMin min_t s_ms
ranged (Nothing)    (Just max_t) s_ms = rangeToMax   max_t s_ms
ranged (Just min_t) (Just max_t) s_ms = range  min_t max_t s_ms


--Function used by Happstack to query for min_t and max_t URL variables.
rangedFilter :: Set.Set Measurement -> ServerPart (Set.Set Measurement)
rangedFilter s_ms =
    do  min_t <- optional $ look "min_t"
        max_t <- optional $ look "max_t"
        let checkedTimes = checkTimes min_t max_t
            rangedTimes  = (uncurry ranged) checkedTimes s_ms
        return rangedTimes

--Pure function which returns locations and their temperatures
locationFilter' :: String -> Locations -> [(String,Set.Set Measurement)]
locationFilter' ""  locs = map (\(x,y) -> (x, temperatures y)) $ Map.toList locs
locationFilter' key locs = case value of
        Nothing -> []
        Just v  -> [(key, temperatures v)]
    where value = Map.lookup key locs

--Function used by Happstack to use location URL variable to get only one location.
--Will return all locations in none specified.
locationFilter :: Locations -> ServerPart [(String, Set.Set Measurement)]
locationFilter locs =
    do  key <- optional $ look "location"
        case key of
            Nothing  -> return $ locationFilter' ""  locs
            Just str -> return $ locationFilter' str locs

--Filters both range and locations.
allFilter :: Locations -> ServerPart [(String, Set.Set Measurement)]
allFilter locs = 
    do  locs' <- locationFilter locs
        let lefts  = map fst locs'
            rights = map snd locs'
        rights' <- mapM rangedFilter rights
        return $ zip lefts rights'




--Handlers
--Server Response for acid-state computations.
computationsHandler :: AcidState Locations -> ServerPart Response
computationsHandler acid = msum
    [ dir "average" $ do
        c <- query' acid PeekAll
        locs <- allFilter c
        liftIO $ createCheckpoint acid
        let avs = map (\(x,y) -> (x, cAverage y)) locs
            html = computationSimpleListed avs
        computationPage "average" html,
      dir "min" $ do
        c <- query' acid PeekAll
        locs <- allFilter c
        liftIO $ createCheckpoint acid
        let mins = map (\(x,y) -> (x, cMin y)) locs
            html = computationSimpleListed mins
        computationPage "minimum" html,
      dir "max" $ do
        c <- query' acid PeekAll
        locs <- allFilter c
        liftIO $ createCheckpoint acid
        let maxs = map (\(x,y) -> (x, cMax y)) locs
            html = computationSimpleListed maxs
        computationPage "maximum" html,
      computationPageWrong
    ]

--Happstack function for retrieving POST data after adding a website.
addPost :: AcidState Locations -> ServerPart String
addPost acid = 
    do  method POST
        decodeBody (defaultBodyPolicy "tmp" 1024 1024 2048)
        msg <- optional $ look "website"
        case msg of
            Nothing  -> return "Failed to add JSON source."
            Just str -> (update' acid $ NewLocation str) >> return "Success!"

--Happstack function which returns HTML output for POST website.
addHandler :: AcidState Locations -> ServerPart Response
addHandler acid = msum [addGetForm, addPostForm]
    where addPostForm = do
            method POST
            msg <- addPost acid
            liftIO $ createCheckpoint acid
            addProcessedForm msg

--Global handlers for the stateful part of the web service.
handlers :: AcidState Locations -> ServerPart Response
handlers acid = msum
    [ dir "peek" $ do
        c <- query' acid PeekAll
        locs <- allFilter c
        liftIO $ createCheckpoint acid
        peekPage $ computationSimpleListed2 $ map (\(x,y) -> (x, Set.toList y)) locs,       
        --peekPage $ computationSimpleListed locs,       
        dir "compute" $ computationsHandler acid,
        dir "add" $ addHandler acid
    ]


--Storage thread, it stores data in acid-state.
storageThread :: AcidState Locations -> MVar [(String,Temperatures)] -> IO ()
storageThread acid m_st = do 
    tmps <- takeMVar m_st
    let ms = map (\(x,y) -> (x, temperatures y)) tmps :: [(String, Set.Set Measurement)]
    _ <- mapM (\(x,y) ->update' acid $ UpdateLocations x y) ms
    createCheckpoint acid 
    storageThread acid m_st