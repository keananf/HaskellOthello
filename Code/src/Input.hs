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
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) world =
  case makeMove board col (x',y') of
    (Just newBoard') -> world {gameboard = newBoard', turn = newCol}
    (Nothing) -> world
  where (x',y') = convertCoords x y
        board = gameboard world
        col = turn world
        newCol = other col

handleInput (EventKey (Char k) Down _ _) world
    = trace ("Key " ++ show k ++ " down") world
handleInput (EventKey (Char k) Up _ _) world
    = trace ("Key " ++ show k ++ " up") world
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

