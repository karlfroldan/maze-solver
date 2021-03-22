module Lib
    ( MazePath (..) 
    , buildGraph 
    , sampleGraph
    , neighbors
    , sampleGraph2 
    , makeVertices 
    , endPoint
    , startPoint
    , makeEdges
    , calculatePos
    , bfs
    , newPq
    , cameFromTable
    , processPath
    ) where

import MazeToGraph
import AStar

import Data.Maybe ( fromJust )

import qualified Data.HashMap.Strict as Map

data MazePath = Wall | Waypoint Int | Start Int | End Int deriving (Show, Eq)

toPath :: [Int] -> [MazePath] 
toPath xs = Waypoint <$> xs

isPath :: Int -> [MazePath] -> Bool 
isPath v xs = Waypoint v `elem` xs

processPath :: Int -> Int -> Map.HashMap Int (Maybe Int) -> [Int]
processPath start end g
    | start == end = [start]
    | otherwise    = processPath start node g ++ [end]
    where 
        node = fromJust $ Map.lookupDefault (Just 0) end g