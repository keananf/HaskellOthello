module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI
import Game

import Debug.Trace

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can worlde a very useful way of debugging!
handleInput :: Event -> World -> World
handleInput (EventMotion (x, y)) world
    = trace ("Mouse moved to: " ++ show (x',y')) world
    where (x',y') = convertCoords x y
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) world
  | not (gameOver board) && inRange board (x', y') = case makeMove board col (x',y') of
      (Just newBoard') -> world {gameboard = newBoard', turn = newCol, oldworld = world}
      (Nothing) -> world --invalid move. Don't change turns
  | not (gameOver board) = undo world --click outside the board undoes a move
  | otherwise = world
  where (x',y') = convertCoords x y
        board = gameboard world
        col = turn world
        newCol = other col

handleInput (EventKey (SpecialKey KeySpace) Down _ _) world
    = trace ("Space key down") world
handleInput (EventKey (SpecialKey KeySpace) Up _ _) world
    | not (gameOver board) = world {gameboard = newBoard, oldworld = world, turn = newCol}
    | otherwise = initWorld --Reset game when gameover and space pressed
    where board = gameboard world
          numPasses = passes board
          newBoard = board {passes = numPasses + 1}
          newCol = other (turn world)
handleInput e world = world

-- | convert the coordinates (with original origin at the center)
-- | to be integers 0 - 7 and with the origin starting in the bottom left
convertCoords :: Float -> Float -> Position
convertCoords x y | y < 0 && x < 0 = (x'-1, y' -1)
                  | y >= 0 && x < 0 = (x'-1, y')
                  | y < 0 = (x', y'-1)
                  | x < 0 = (x'-1, y')
                  | otherwise = (x', y')
  where x' = truncate (x /50) + 4
        y' = truncate (y /50) + 4

