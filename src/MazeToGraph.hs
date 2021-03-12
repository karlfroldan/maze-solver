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

makeEdges :: [[Int]] -> [(Int, Int)]
makeEdges xs = horizontalEdges ++ verticalEdges
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

-- The startpoint of any graph is always 1
startPoint :: Int 
startPoint = 1
-- The end point of any graph is always the largest integer
endPoint :: [[Int]] -> Int 
endPoint vertices = maximum $ concat vertices

-- Build the graph that will represent the maze
buildGraph :: [String] -> G.Graph Int 
buildGraph xs = G.edges $ makeEdges $ makeVertices 0 xs

hold :: Int -> Int 
hold x
    | x == 0    = 0
    | otherwise = 1