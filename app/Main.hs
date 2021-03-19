module Main where

import Lib

import Control.Monad (forM_)
import Data.List ( zipWith )

import System.IO

import qualified Graphics.UI.Threepenny         as UI
import Graphics.UI.Threepenny.Core ( Window, UI, (#), (#+), defaultConfig, set )
import qualified Graphics.UI.Threepenny.Core    as UICore
import qualified Graphics.UI.Threepenny.Canvas  as UICanvas
import qualified Graphics.UI.Threepenny.Elements as E

import Foreign.JavaScript ( Server, loadFile, MimeType, URI )
import qualified Foreign.JavaScript as FFIJS

canvasX :: Int
canvasX = 800
canvasY :: Int
canvasY = 800

passableTile :: [(String, String)]
passableTile = [("border", "solid white 1px"), ("background", "white"), ("padding", "0px"), ("margin", "0px")]
nonPassableTile :: [(String, String)]
nonPassableTile = [("border", "solid black 1px"), ("background", "black"), ("padding", "solid black 0px"), ("margin", "0px")]
startTile :: [(String, String)]
startTile = [("border", "solid white 1px"), ("background", "blue"), ("padding", "0px"), ("margin", "0px")]
goalTile :: [(String, String)]
goalTile = [("border", "solid white 1px"), ("background", "red"), ("padding", "0px"), ("margin", "0px")]
waypointTile :: [(String, String)]
waypointTile = [("border", "solid white 1px"), ("background", "cyan"), ("padding", "0px"), ("margin", "0px")]


main :: IO ()
main = do
    let maze       = makeVertices 0 sampleGraph2
        g          = buildGraph sampleGraph2
        path       = init $ tail $ processPath 1 (endPoint maze) $ bfs g 1
        maze'      = filterPath maze path
    
    UICore.startGUI defaultConfig (setupGUI maze maze')

filterPath maze path = fmap (fmap f) maze
    where ep = endPoint maze
          f x 
            | x `elem` path = -1
            | otherwise     = x

setupGUI :: [[Int]] -> [[Int]] -> Window -> UI ()
setupGUI maze maze' window = do
    return window # set UICore.title "Maze solver"
    UICore.getBody window # set UI.style [("width", "100%")]
    let cells = mkCells maze maze'
        rows  = fmap (UI.tr #+) cells
        table = UI.table # set UI.style [("align", "center")] #+ rows 

        -- Legend
        legendStr  = UI.string "legend:" # set UI.style [("font-weight", "bold"), ("padding", "15px")]
        startState = UI.string "start"   # set UI.style [("color", "blue"), ("padding", "15px")]
        endState   = UI.string "goal"    # set UI.style [("color", "red"), ("padding", "15px")]
        legend     = UI.div 
                   # set UI.style [("border", "solid black 5px"), ("margin", "13px"), ("padding", "13px"), ("display", "inline-block")]
                   #+ [legendStr, startState, endState]

    UICore.getBody window #+ [legend, table]
    return ()

-- Make cells create a maze with dimensions of 800x800
mkCells :: [[Int]] -> [[Int]] -> [[UI UICore.Element]]
mkCells maze maze' =
    fmap (fmap (\(namedCell, cell) -> 
        UI.td # set UI.width dims 
              # set UI.height dims 
              # set UI.style (defineStyle maze' cell)
              #+ [cellName namedCell])) zipped
    where 
        zipped         = zipWith zip maze maze'
        mazeDim        = length maze' 
        dims           = 600 `div` mazeDim
        cellName cellN = UI.string (show cellN)

defineStyle :: [[Int]] -> Int -> [(String, String)]
defineStyle maze n
    | n == 0                   = nonPassableTile
    | n == 1                   = startTile
    | n == (-1)                = waypointTile
    | n == endPoint maze       = goalTile
    | otherwise                = passableTile