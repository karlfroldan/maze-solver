module Lib
    ( someFunc
    , buildGraph
    , newGraph 
    , sampleGraph
    , makeVertices 
    , endPoint
    ) where

import MazeToGraph ( buildGraph, newGraph, sampleGraph, makeVertices, endPoint )

someFunc :: IO ()
someFunc = putStrLn "someFunc"
