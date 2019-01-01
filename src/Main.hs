{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Prelude hiding (Either(..))

import System.Console.ANSI
import System.Console.Terminal.Size
import Control.Monad (when)
import Data.Maybe (isNothing, fromJust)
import Data.List (replicate, null)
import Data.Colour.RGBSpace
import System.IO (hSetEcho, hSetBuffering, stdin, stdout, BufferMode(NoBuffering))

isEmpty = null

data GameColor
  = GCGreen
  | GCYellow
  | GCOrange
  | GCRed
  | GCPurple
  | GCBlue

data Direction
  = Left
  | Right
  | Quit
  deriving (Eq)

type Coords = (Int, Int)
type Bounds = Coords

main :: IO ()
main = do
  -- Window Size
  actualSize <- size
  when (isNothing actualSize) (return ())
  let (Window{height, width}) = fromJust actualSize

  setupGame (width, height)

setupGame :: Bounds -> IO ()
setupGame bounds@(maxX, maxY) = do
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor

  -- quot divides two integers and rounds
  gameLoop ((maxX `quot` 2), maxY) bounds


gameLoop :: Coords -> Bounds -> IO ()
gameLoop coords bounds@(width, height) = do
  clearScreen

  -- Create Game Field
  let colorArray = replicate 6 $ replicate width True
  drawRows 0 [GCGreen,GCYellow,GCOrange,GCRed,GCPurple,GCBlue] colorArray

  drawPlayer coords
  dir <- getDirectionFromInput
  when (dir == Quit) (return ())
  gameLoop (updatePlayerCoords coords bounds dir) bounds

drawRows :: Int -> [GameColor] -> [[Bool]] -> IO ()
drawRows _  _ [] = return ()
drawRows row (w:ws) (x:xs) = do
  drawRow row 0 w x
  drawRows (row + 1) ws xs

-- draws all blocks of a single row
drawRow :: Int -> Int -> GameColor -> [Bool] -> IO ()
drawRow _ _ _ [] = return ()
drawRow row column color (x:xs) = do
  setCursorPosition row column
  setColor color
  putStrLn "▆"
  drawRow row (column + 1) color xs

drawPlayer :: Coords -> IO ()
drawPlayer (x,y) = do
  setCursorPosition y x
  putStrLn "━"

setColor :: GameColor -> IO ()
setColor color = case color of
  GCGreen  -> setSGR [SetColor Foreground Vivid Green]
  GCYellow -> setSGR [SetColor Foreground Vivid Yellow]
  GCOrange -> setSGR [SetColor Foreground Dull Red]
  GCRed    -> setSGR [SetColor Foreground Vivid Red]
  GCPurple -> setSGR [SetColor Foreground Vivid Magenta]
  GCBlue   -> setSGR [SetColor Foreground Vivid Blue]

updatePlayerCoords :: Coords -> Bounds -> Direction -> Coords
updatePlayerCoords coords@(x,y) (maxX, _) dir = case dir of
  Left  -> if (x > 0) then (x-1,y) else coords
  Right -> if (x < maxX-1) then (x+1,y) else coords

getDirectionFromInput :: IO Direction
getDirectionFromInput = do
  directionToInput =<< getChar
  where directionToInput char = case char of
          'a' -> return Left
          'd' -> return Right
          'q' -> return Quit
          _   -> getDirectionFromInput

-- Check if over
isDone :: [[Bool]] -> Bool
isDone [] = True
isDone (x:xs)
  | isEmpty x = isDone xs
  | otherwise = False
