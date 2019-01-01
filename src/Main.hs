{-# LANGUAGE NamedFieldPuns #-}

module Main where

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

type Coord = (Int, Int)

main :: IO ()
main = do
  -- Preparations
  hSetEcho stdin False -- Hides input characters
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  clearScreen

  -- Window Size
  actualSize <- size
  when (isNothing actualSize) (return ())
  let (Window{height, width}) = fromJust actualSize

  -- TODO: check if the playing field is big enough

  -- Create Game Field
  let colorArray = replicate 6 $ replicate width True
  drawRows 0 [GCGreen,GCYellow,GCOrange,GCRed,GCPurple,GCBlue] colorArray
  drawPlayer (width `quot` 2, height) -- quot divides ints, returns int


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

drawPlayer :: Coord -> IO ()
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

-- Check if over
isDone :: [[Bool]] -> Bool
isDone [] = True
isDone (x:xs)
  | isEmpty x = isDone xs
  | otherwise = False
