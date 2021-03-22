module MazeToGraph 
    ( buildGraph
    , sampleGraph
    , startPoint
    , sampleGraph2
    , makeVertices
    , endPoint
    , makeEdges
    , calculatePos
    , neighbors
    , Neighbors
    , Graph
    ) where

import Data.Maybe ( fromMaybe )
import Data.Tuple ( swap )
import qualified Data.HashSet as Set
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as Map
import Data.List (transpose)

type Graph = HashMap Int Neighbors
type Neighbors = Set.HashSet Int

-- We build the vertex names from the graph given
makeVertices :: Int -> [String] -> [[Int]]
makeVertices counter []           = []
makeVertices counter (list:lists) =
    makeVertices' 1 (noWhitespace list) : makeVertices (counter + numZero) lists
    where 
        -- We count how many zeroes are in the maze
        -- The zeroes are actually the passable tiles of the maze
        numZero = length $ filter (=='0') list
        apply n m i -- = if i == '1' then 0 else n + m
            | i == '1'  = 0
            | otherwise = n + m
        -- Here, makeVertices' is a function that flips and
        -- actually gives names to the vertex.
        -- It sets all unpassable parts of the maze to 0 and 
        -- unique names to passable tiles in the maze.
        -- The start part of the maze is always tile 1.
        -- We assume that there is always a path from start to end
        makeVertices' _ []     = []
        makeVertices' c (x:xs) = let applyC = apply counter c x 
                                 in  applyC : makeVertices' (c + hold applyC) xs
        noWhitespace = filter (/= ' ')

makeEdges :: [[Int]] -> [(Int, Int)]
makeEdges xs = horizontalEdges ++ verticalEdges ++ invertedHor ++ invertedVert
    where 
        findEdges [x]      = []
        -- x and y should both not be zero as that is not a passable
        -- tile in the graph
        findEdges (x:y:xs)
            | 0 `notElem` [x, y] = (x, y) : findEdges (y:xs)
            | otherwise          = findEdges (y:xs)
        -- find adjacent horizontal tiles in the matrix
        horizontalEdges = concatMap findEdges xs 
        -- find adjacent vertical tiles in the matrix
        verticalEdges   = concatMap findEdges (transpose xs)
        invertedHor     = swap <$> horizontalEdges
        invertedVert    = swap <$> verticalEdges


-- The startpoint of any graph is always the minimum vertex
startPoint :: [[Int]] -> Int 
startPoint vs = minimum $ filter (> 0) (concat vs)
-- The end point of any graph is always the largest integer
endPoint :: [[Int]] -> Int 
endPoint vs = maximum $ concat vs

-- Build the graph that will represent the maze
buildGraph :: [String] -> Graph
buildGraph g = foldl (\map (k, v) -> Map.adjust (Set.union (Set.fromList [v])) k map) buildMap edges
    where edges = makeEdges (makeVertices 0 g)
          buildMap = foldl (\map (e, _) -> Map.insert e Set.empty map) Map.empty edges

calculatePos :: Int -> [[Int]] -> (Int, Int) 
calculatePos n xs = (xPos, yPos)
    where 
        listPos = foldl findX 0 (concat xs)
        dim = length xs 
        findX acc x
            | x   == n  = acc * (-1) 
            | acc >  0  = acc
            | acc == 0  = acc - 1
            | otherwise = acc - 1
        yPos = listPos `div` dim 
        xPos = listPos `mod` dim

neighbors :: Int -> Graph -> Neighbors
neighbors k g = fromMaybe Set.empty $ Map.lookup k g

hold :: Int -> Int 
hold x
    | x == 0    = 0
    | otherwise = 1

noZero :: (Integral a) => [a] -> [a] 
noZero = filter (/= 0)

sampleGraph :: [String]
sampleGraph = [ "1 1 1 1 1 1"
              , "0 0 0 0 0 1"
              , "1 0 1 1 0 1"
              , "1 0 0 1 0 1"
              , "1 1 0 0 0 1"
              , "1 1 1 1 0 1"]

sampleGraph2 :: [String]
sampleGraph2 = [ "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1"
               , "0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 1"
               , "1 0 1 0 1 0 1 1 1 1 1 0 1 0 1 0 1 0 1 1 1 1 1 0 1"
               , "1 0 1 0 1 0 1 0 0 0 1 0 1 0 0 0 1 0 1 0 0 0 0 0 1"
               , "1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 0 1 1 1 1 1"
               , "1 0 0 0 1 0 1 0 1 0 0 0 1 0 1 0 0 0 1 0 1 0 0 0 1"
               , "1 0 1 0 1 0 1 0 1 1 1 1 1 0 1 0 1 1 1 0 1 1 1 0 1"
               , "1 0 0 0 1 0 1 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 1 0 1"
               , "1 0 1 0 1 0 1 1 1 1 1 1 1 0 1 0 1 0 1 1 1 0 1 0 1"
               , "1 0 1 0 1 0 0 0 1 0 0 0 1 0 1 0 1 0 0 0 1 0 1 0 1"
               , "1 0 1 0 1 1 1 0 1 0 1 1 1 0 1 1 1 0 1 0 1 0 1 0 1"
               , "1 0 1 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 1 0 1 0 0 0 1"
               , "1 0 1 0 1 0 1 1 1 0 1 0 1 1 1 0 1 0 1 0 1 1 1 0 1"
               , "1 0 1 0 1 0 1 0 0 0 1 0 0 0 1 0 1 0 1 0 1 0 0 0 1"
               , "1 1 1 0 1 0 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 1 1 1 1"
               , "1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 0 0 1"
               , "1 0 1 1 1 1 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1"
               , "1 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 1 0 0 0 1 0 1"
               , "1 0 1 0 1 0 1 1 1 1 1 1 1 0 1 0 1 0 0 0 1 1 1 0 1"
               , "1 0 0 0 1 0 0 0 0 0 0 0 1 0 1 0 0 0 1 0 0 0 0 0 1"
               , "1 0 1 1 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 1 1 0 1 0 1"
               , "1 0 0 0 0 0 0 0 1 0 1 0 1 0 1 0 1 0 1 0 0 0 0 0 1"
               , "1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1"
               , "1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 1"
               , "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1"
               ]

