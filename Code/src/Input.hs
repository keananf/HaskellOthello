module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI
import Game
import ClientMain
import System.IO.Unsafe


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
  | validNetworkMove world x' y' = unsafePerformIO (getNetwork world x' y')

  | validGameMove world x' y' = case makeMove board (turn world) (x',y') of
      (Just newBoard') -> world {gameboard = newBoard', turn = newCol, oldworld = world}
      (Nothing) -> world --invalid move. Don't change turns

  | validUndo world x' y' = unsafePerformIO (undoMove world) --click outside the board undoes a move
  | otherwise = world --game is over, don't update world
  where (x',y') = convertCoords x y
        newCol = other (turn world)
        board = gameboard world


handleInput (EventKey (SpecialKey KeySpace) Down _ _) world
    = trace ("Space key down") world
handleInput (EventKey (SpecialKey KeySpace) Up _ _) world
    | not (gameOver board) = world {gameboard = newBoard, oldworld = world, turn = newCol}
    | otherwise = initWorld (args world)--Reset game when gameover and space pressed
    where board = gameboard world
          numPasses = passes board
          newBoard = board {passes = numPasses + 1}
          newCol = other (turn world)

handleInput (EventKey (Char k) Up _ _) world
  | k == 'h' = world {hints = (not (hints world))}
  | k == 'r' = world {gameboard = (board {reversi = rev})}
  | otherwise = world
  where rev = not (reversi (gameboard world))
        board = gameboard world
handleInput e world = world



-- | convert the coordinates (with original origin at the center)
-- | to be integers 0 - 7 and with the origin starting in the bottom left
convertCoords :: Float -> Float -> Position
convertCoords x y | y < 0 && x < 0 = (x'-1, y' -1)
                  | y >= 0 && x < 0 = (x'-1, y')
                  | y < 0 = (x', y'-1)
                  | x < 0 = (x'-1, y')
                  | otherwise = (x', y')
  where x' = truncate (x / 50) + 4
        y' = truncate (y / 50) + 4

-------------------------------------------------------------------
validNetworkMove :: World -> Int -> Int -> Bool
validNetworkMove world x y = not (gameOver board) && (userCol world) == (turn world)
  && network world && inRange board (x, y)
  where board = gameboard world

validGameMove :: World -> Int -> Int  -> Bool
validGameMove world x y = not (gameOver board) && ((userCol world) == (turn world) || not (ai world))
  && not (network world) && inRange board (x, y)
  where board = gameboard world

validUndo :: World -> Int -> Int -> Bool
validUndo world x y = not (inRange board (x, y)) && not (gameOver board) && not (network world)
  where board = gameboard world
--------------------------------------------------------------------

-- | Makes a move and attempts to send it across the network
getNetwork :: World -> Int -> Int -> IO World
getNetwork world x y = case makeMove board col (x,y) of --make move and send across network
    (Just newBoard') -> do let newWorld = world {gameboard = newBoard', turn = newCol, oldworld = world}
                           sendAcrossNetwork networkHandle x y
                           return newWorld
    (Nothing) -> do let newWorld = world --bad move, dont update world
                    print ""
                    return newWorld
    where board = gameboard world
          col = turn world
          newCol = other col
          networkHandle = handle world
