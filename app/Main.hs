module Main where

import Lib

import Control.Monad (forM_)

import System.IO

import qualified Graphics.UI.Threepenny         as UI
import Graphics.UI.Threepenny.Core ( Window, UI, (#), (#+), defaultConfig, set )
import qualified Graphics.UI.Threepenny.Core    as UICore
import qualified Graphics.UI.Threepenny.Canvas  as UICanvas
import qualified Graphics.UI.Threepenny.Elements as E

canvasX :: Int
canvasX = 400
canvasY :: Int
canvasY = 300

passableTile = [("border", "solid white 1px"), ("background", "white"), ("padding", "0px"), ("margin", "0px")]
nonPassableTile = [("border", "solid black 1px"), ("background", "black"), ("padding", "solid black 0px"), ("margin", "0px")]
startTile = [("border", "solid white 1px"), ("background", "blue"), ("padding", "0px"), ("margin", "0px")]
goalTile = [("border", "solid white 1px"), ("background", "red"), ("padding", "0px"), ("margin", "0px")]

main :: IO ()
main = do
    let maze = makeVertices 0 sampleGraph
        mazeDim = length sampleGraph
        canvasDim = mazeDim * 50
    UICore.startGUI defaultConfig (setupGUI canvasDim maze)

setupGUI :: Int -> [[Int]] -> Window -> UI ()
setupGUI canvasDim maze window = do
    return window # set UICore.title "Maze solver"
    UICore.getBody window # set UI.style [("width", "100%")]
    let cells = mkCells maze
        rows  = fmap (UI.tr #+) cells
        table = UI.table # set UI.style [("align", "center")] #+ rows 
        -- Legend
        legendStr  = UI.string "legend:" # set UI.style [("font-weight", "bold"), ("padding", "15px")]
        startState = UI.string "start" # set UI.style [("color", "blue"), ("padding", "15px")]
        endState   = UI.string "goal" # set UI.style [("color", "red"), ("padding", "15px")]
        legend     = UI.div 
                   # set UI.style [("border", "solid black 5px"), ("margin", "13px"), ("padding", "13px"), ("display", "inline-block")]
                   #+ [legendStr, startState, endState]

    UICore.getBody window #+ [legend, table]
    return ()
-- setupGUI :: Int -> [[Int]] -> Window -> UI ()
-- setupGUI canvasDim maze window  = do
--     return window # UICore.set UI.title "Maze Solver"
--     UICore.getBody window #+ [ mkDisplay canvasDim maze ]
--     return ()

-- mkDisplay :: Int -> [[Int]] -> UI UI.Element
-- mkDisplay canvasDim maze = do
--     let canvas = UI.canvas
--                # UICore.set UI.width canvasDim
--                # UICore.set UI.height canvasDim
--                # UICore.set UICore.style tableStyle
--         rowNames = fmap (\x -> "row" ++ show x) [1..canvasDim]
--     canvas <- UI.canvas
--         # UICore.set UI.width  canvasDim
--         # UICore.set UI.height canvasDim
--         # UICore.set UICore.style [("border", "solid black 1px"), ("background", "white")]
--     let canvasUI  = return canvas :: UI UI.Element
--         cells     = mkCells $ tileName 0 maze
--         grid      = UICore.grid cells
--     canvasUI #+ [grid]

tileName :: Int -> [[Int]] -> [[String]]
tileName _ []                 = []
tileName counter table@(t:ts) =
    tileName' 0 t : tileName (counter + listLength) ts
    where
        listLength = length table
        tileName' _ [] = []
        tileName' c (x:xs) =
            show (c + counter) : tileName' (c + 1) xs

mkCells :: [[Int]] -> [[UI UI.Element]]
mkCells maze =
    fmap (fmap (\cell ->
        UI.td # set UI.width 30
              # set UI.height 30
              # set UI.style (defineStyle maze cell))) maze

defineStyle :: [[Int]] -> Int -> [(String, String)]
defineStyle maze n
    | n == 0             = nonPassableTile
    | n == 1             = startTile
    | n == endPoint maze = goalTile
    | otherwise          = passableTile