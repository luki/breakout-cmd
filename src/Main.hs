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

  -- Create Game Field
  let colorArray = replicate 6 $ replicate width True
  renderRows 0 [GCGreen,GCYellow,GCOrange,GCRed,GCPurple,GCBlue] colorArray

  putStrLn "Hello, Haskell!"

renderRows :: Int -> [GameColor] -> [[Bool]] -> IO ()
renderRows _  _ [] = return ()
renderRows row (w:ws) (x:xs) = do
  renderRow row 0 w x
  renderRows (row + 1) ws xs

-- Renders all blocks of a single row
renderRow :: Int -> Int -> GameColor -> [Bool] -> IO ()
renderRow _ _ _ [] = return ()
renderRow row column color (x:xs) = do
  setCursorPosition row column
  setColor color
  putStrLn "▆"
  renderRow row (column + 1) color xs

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
