module AStar ( bfs, bfs', newPq, cameFromTable ) where

import Data.Hashable ( Hashable )
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import qualified Data.PQueue.Min as PQ
import Data.Maybe ( fromMaybe )

import MazeToGraph ( Graph
                   , Neighbors, neighbors
                   )

bfs :: Graph -> Int -> Map.HashMap Int (Maybe Int)
bfs g start = bfs' g (PQ.singleton start) (Map.singleton start Nothing) start

bfs' :: Graph                       -- The graph to traverse
     -> PQ.MinQueue Int             -- The priorty queue
     -> Map.HashMap Int (Maybe Int) -- A hashmap to store which node we came from
     -> Int                         -- The start node
     -> Map.HashMap Int (Maybe Int)
bfs' g pq map start 
    | PQ.null pq = map
    | otherwise  = bfs' g pq'' cameFrom start
    where
        -- We pop the current node from the priority queue
        (current, pq') = PQ.deleteFindMin pq
        -- We get the neighbors of the current node
        currentN            = neighbors current g
        -- Node list of G
        nodeList            = Map.keysSet g
        pq''                = newPq pq' currentN map
        cameFrom            = cameFromTable current map currentN

newPq :: Ord k => PQ.MinQueue k -> Set.HashSet k -> Map.HashMap k a -> PQ.MinQueue k
-- n being the neighbors of the current in G
newPq oldPq n table = Set.foldl' place oldPq n
    where place acc x   
            | x `elem` keyset = acc 
            | otherwise       = PQ.insert x acc
          keyset = Map.keysSet table

cameFromTable :: (Eq k, Hashable k) => a -> Map.HashMap k (Maybe a) -> Set.HashSet k -> Map.HashMap k (Maybe a)
-- c being the current value 
-- n being the neighbors of c in graph G
cameFromTable c oldMap n = Set.foldl' place oldMap n 
    where place acc x 
            | Set.member x keys = acc 
            | otherwise         = Map.insert x (Just c) acc
          keys = Map.keysSet oldMap

-- search :: Graph 
--        -> Set.HashSet Int -- Open set
--        -> Set.HashSet Int -- Closed set
--        -> Int             -- cost 
--        -> 

-- astar :: Graph          -- The graph to be traversed
--       -> Int            -- The start node
--       -> Int            -- The end node
--       -> (Int -> Int)   -- The heuristic function
--       -> (Int, [Int])   -- (Cost of the path, Path)
-- astar g start end f =

-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
-- @@@@@ Heuristic Algorithms @@@@@
-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

-- We are only having 4 directions so we use Manhattan
-- manhattan :: (Int, Int) -> (Int, Int) -> Int 
-- manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- getCoords :: Gr (Int, Int) Int -> [Node] -> [LNode (Int, Int)]
-- getCoords g xs = (\v -> (v, (fromMaybe (0, 0) . lab g) v)) <$> xs