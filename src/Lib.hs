module Lib
    ( buildGraph 
    , sampleGraph
    , neighbors
    , sampleGraph2 
    , makeVertices 
    , endPoint
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

processPath :: Int -> Int -> Map.HashMap Int (Maybe Int) -> [Int]
processPath start end g
    | start == end = [start]
    | otherwise    = processPath start node g ++ [end]
    where 
        node = fromJust $ Map.lookupDefault (Just 0) end g