{-# LANGUAGE OverloadedStrings #-}

module HTML where

import Control.Monad
import Happstack.Server

import qualified Data.Map as Map
import qualified Data.Set as Set

-- Blaze

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


import Types
{-
This module contains HTML templates generated using Blaze library.

It also contains basic HTTP server implemented in Happstack
For now it runs on default configuration: localhost:8000/
Entering localhost:8000/?type=<average|...> will return computed values.

These functions:

    H.html
    H.body
    H.title

correspond to <html> <body> <title> tags.
-}

--Starting home page
home :: ServerPart Response
home = ok $ toResponse $ --Changes the blaze template into happstack response.  
    H.html $ do
        H.head $ do
            H.title "Welcome"
        H.body $ do
            H.h1 "Welcome"
            H.p  "This website allows you to browse temperature"
            H.h2 "Usage:"
            H.p $ do 
                H.i  "/peek"
                " - See all stored data."
            H.p $ do
                H.i  "/compute/..."
                " - Perform a computation on stored data."
                H.p "Possible computations:"
                "/compute/average"
                H.br
                "/compute/min"
                H.br
                "/compute/max"
            H.p $ do
                H.i  "/add"
                " - Add a website"  
            H.p $ do
                H.i  "?min_t=YYYY-MM-DD&max_t=YYYY-MM-DD"
                " - "
                H.b "OPTIONAL"
                " Filter the temperature data based on time range. Applies to both peek and compute queries."
            H.p $ do
                H.i  "?location=http://www.phoric.eu/temperature.json"
                " - "
                H.b "OPTIONAL"
                " Filter the temperature data based on a specific website."


--Function which returns listed variables.
listData :: (Show a) => [a] -> H.Html
listData as = H.ul $ forM_ as (H.li . H.toHtml . show)


--Functions which returns locations themselves.
printLocations :: Locations -> H.Html
printLocations ls = listData locs
    where locs = Map.keys ls

--Function which returns locations with their temperatures.
printLocationsAndTemperatures :: Locations -> H.Html
printLocationsAndTemperatures ls = H.ul $ forM_ (Map.toList ls) printOne
    where printOne l = H.h3 (H.toHtml (fst l)) >> listData (Set.toList $ temperatures (snd l))

--Show one computation
computationSimple :: (Show a) => a -> H.Html
computationSimple val = (H.toHtml.show) val

--Show key-value like data.
computationSimpleListed :: (Show a) => [(String, a)] -> H.Html
computationSimpleListed vals = H.ul $ forM_ vals (\(s,v) -> ((H.li . H.b . H.string) s) >> ((H.ul . H.li . H.toHtml . show) v ))

--Show key-list of values like data.
computationSimpleListed2 :: (Show a) => [(String, [a])] -> H.Html
computationSimpleListed2 vals = H.ul $ forM_ vals (\(s,v) -> ((H.li . H.b . H.string) s) >> listData v )

--Computation page
computationPage :: String -> H.Html -> ServerPart Response
computationPage str html = ok $ toResponse $ --Changes the blaze template into happstack response.
    H.html $ do
        H.head $ do
            H.title $ H.string $ comp
        H.body $ do
            H.h2 $ H.string $ comp
            H.p $ H.string $ comp ++ " result:"
            html
    where comp =  "Computation \"" ++ str ++ "\""

--Peek page.
peekPage :: H.Html -> ServerPart Response
peekPage html = ok $ toResponse $ --Changes the blaze template into happstack response.
    H.html $ do
        H.head $ do
            H.title $ H.string $ comp
        H.body $ do
            H.h2 $ H.string $ comp
            html
    where comp =  "Browse data"

--Computation page for wrongly specified computation type
computationPageWrong :: ServerPart Response
computationPageWrong = ok $ toResponse $ --Changes the blaze template into happstack response.
    H.html $ do
        H.head $ do
            H.title $ "Wrong type of computation"
        H.body $ do
            H.h2 "Wrong type of computation"
            H.p "Error - wrong type of computation."
            H.p "Please select a proper type of computation:"
            H.i"average"
            H.br
            H.i "min"
            H.br
            H.i "max"


--Add page for GET method.
addGetForm :: ServerPart Response
addGetForm =
    do  method GET
        ok $ toResponse $ do
            H.form H.! A.action "" H.! A.enctype "multipart/form-data" H.! A.method "POST" $ do
                H.label H.! A.for "website" $ "Specify a new website "
                H.input H.! A.type_ "text" H.! A.id "website" H.! A.name "website"
                H.input H.! A.type_ "submit" H.! A.value "Submit"

--Add page for POST method.
addProcessedForm :: String -> ServerPart Response
addProcessedForm str =
        ok $ toResponse $ do
            H.h2 (H.string str)

