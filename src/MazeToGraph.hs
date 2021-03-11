module MazeToGraph 
    ( buildGraph
    , sampleGraph
    , makeVertices
    , endPoint
    , makeEdges
    ) where

import qualified Algebra.Graph as G
import Data.List ( transpose )

sampleGraph :: [String]
sampleGraph = ["111111", "001001", "101101", "100101", "110001", "111101"]

-- We build the vertex names from the graph given
makeVertices :: Int -> [String] -> [[Int]]
makeVertices counter []           = []
makeVertices counter (list:lists) =
    makeVertices' 1 list : makeVertices (counter + numZero) lists
    where 
        -- We count how many zeroes are in the maze
        -- The zeroes are actually the passable parts of the maze
        numZero = length $ filter (=='0') list
        apply n m i = if i == '1' then 0 else n + m
        -- Here, makeVertices' is a function that flips and
        -- actually gives names to the vertex.
        -- It sets all unpassable parts of the maze to 0 and 
        -- unique names to passable tiles in the maze.
        -- The start part of the maze is always tile 1.
        -- We assume that there is always a path from start to end
        makeVertices' _ []     = []
        makeVertices' c (x:xs) = 
                if apply counter c x == 0
                then 0 : makeVertices' c xs 
                else apply counter c x : makeVertices' (c + 1) xs

-- A function that builds edges by scanning adjacent 
-- passable tiles in the matrix
makeEdges :: [[Int]] -> [(Int, Int)]
makeEdges xs = horizontalEdges ++ verticalEdges
    where findEdges [x]      = []
          findEdges (x:y:xs) = 
              if x /= 0 && y /= 0
              then (x,y):findEdges (y:xs)
              else findEdges (y:xs)
          -- find adjacent horizontal tiles in the matrix
          horizontalEdges = concatMap findEdges xs 
          -- find adjacent vertical tiles in the matrix
          verticalEdges   = concatMap findEdges (transpose xs)
-- The startpoint of any graph is always 1
startPoint :: Int 
startPoint = 1
-- The end point of any graph is always the largest integer
endPoint :: [[Int]] -> Int 
endPoint vertices = maximum $ concat vertices

-- Build the graph that will represent the maze
buildGraph :: [String] -> G.Graph Int 
buildGraph xs = G.edges $ makeEdges $ makeVertices 0 xs