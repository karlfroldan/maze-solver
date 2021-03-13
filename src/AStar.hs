module AStar (

) where 

import Data.List ( sortBy, intersect )
import Data.Function (on)

import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree ( Gr )

import MazeToGraph

type ListPairs a = ([LNode a], [LNode a])
type StartEnd  a = (LNode a, LNode a)

-- I'm using filters a lot. Let me declare a shorthand
(@==) :: (a -> Bool) -> [a] -> [a]
f @== xs = filter f xs
-- Initially keep f at 0
-- f :: LNode a -> (LNode a, Int)
-- f n = (n, 0)
astar' :: Gr (Int, Int) Int -> ListPairs (Int, Int) -> StartEnd (Int, Int) -> [LNode (Int, Int)]
-- We can assume that the algorithm is terminating
-- We simply return the openset when it's empty
astar' g ([], closedset) _ = closedset
astar' g sets endpoints    = []
    --if goalInList goal leastNodeNeighbours
    --then leastNodeF:closedset
    --else
    where
        (openset, closedset) = sets
        closedSetNodes       = fst <$> closedset
        (start, goal)        = endpoints
        -- Calculate all the fs in the openset
        -- and get the minimum value of f
        -- we separate the node from its position or label
        (leastNode, fValue)  = minSnd $ f <$> openset
        (fNode, fLabel)      = leastNode
        -- This is the open set after removing the node with the least f
        openset'             = (/= leastNode) @== openset
        -- get the neighbors of the leastNodeF
        lnn                  = neighbors g fNode
        leastNodeNeighbours  = (`notElem` closedSetNodes) @== lnn
        f                    = g + h fLabel goalPos
        goalPos              = snd goal
        
-- 
astar :: Gr (Int, Int) Int -> StartEnd (Int, Int) -> [LNode (Int, Int)]
astar g endpoints = astar' g ([start], []) endpoints
    where 
        (start, _) = endpoints 


minSnd :: (Ord b) => [(a, b)] -> (a, b)
minSnd ((x,y):xs) = foldl cmp (x, y) xs
    where cmp (a, b) (x, y) = if b <= y 
                              then (a, b)
                              else (x, y)

minFst :: (Ord a) => [(a, b)] -> (a, b)
minFst ((x, y):xs) = foldl cmp (x, y) xs 
    where cmp (a, b) (x, y) = if a <= x 
                              then (a, b)
                              else (x, y)

maxFst :: (Ord a) => [(a, b)] -> (a, b)
maxFst ((x, y):xs) = foldl cmp (x, y) xs 
    where cmp (a, b) (x, y) = if a <= x 
                              then (x, y)
                              else (a, b)

-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
-- @@@@@ Heuristic Algorithms @@@@@
-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

goalInList :: (Foldable t, Eq a) => a -> t a -> Bool
goalInList goal xs = goal `elem` xs 

-- We are only having 4 directions so we use Manhattan
manhattan :: (Int, Int) -> (Int, Int) -> Int 
manhattan current@(x1, y1) goal@(x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- This is our heuristics function
h :: (Int, Int) -> (Int, Int) -> Int
h = manhattan

g :: [LNode a] -> Int 
g = length



