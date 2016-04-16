module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Extent
import Draw
import Board
import Game
import ClientMain

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
handleInput :: Event -> World -> IO World
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) world
  | (gameState world) == Paused = return world --ignore input until it is not paused
  | (gameState world) == Menu = return (handleMenuInput x' y' world)

    --Functionality for buttons
  | coordInExtent (menuExtent) (x',y') && not (network world)= return (initWorld (args world))
  | coordInExtent (undoExtent) (x',y') && validUndo world x' y' = undoMove world
  | coordInExtent (hintsExtent) (x',y') = return world {hints = (not (hints world))}

  | validNetworkMove world x' y' = (moveOverNetwork world x' y')

  | validGameMove world x' y' = case makeMove board (turn world) (x',y') of
      (Just newBoard') -> return world {gameboard = newBoard', turn = newCol,
                                        oldworld = world, time = 10.0}
      (Nothing) -> return world --invalid move. Don't change turns

  | gameOver board = return world {gameState = GameOver} --game is over, don't update world
  | otherwise = return world --click out of range, or other undefined behaviour is ignored
  where (x',y') = convertCoords board x y
        newCol = other (turn world)
        board = gameboard world


handleInput (EventKey (SpecialKey KeySpace) Up _ _) world
    | (gameState world) == Paused = return world --ignore space on pause
    | not (network world) && not (gameOver board) = return world {gameboard = newBoard,
                                                                  oldworld = world, turn = newCol}
    | (network world) && not (gameOver board) = (passOverNetwork world)
    | otherwise = return (initWorld (args world))--Reset game when gameover and space pressed
    where board = gameboard world
          newBoard = board {passes = (passes board) + 1}
          newCol = other (turn world)

handleInput (EventKey (Char k) Up _ _) world
  | k == 'r' && not (network world )= return world {gameboard = (board {reversi = rev})}
  | k == 'p' && (gameState world) == Paused = return world {gameState = Playing} --unpause
  | k == 'p' && (gameState world)== Playing && not (network world)=
      return world {gameState = Paused} --pause
  | otherwise = return world
  where rev = not (reversi (gameboard world))
        board = gameboard world

handleInput e world = return world


------------------------------------------------------------------------
handleMenuInput :: Int -> Int -> World -> World
handleMenuInput x y world| coordInExtent (playExtent) (x, y) = world {gameState=Playing}
                         | coordInExtent (aiExtent) (x, y) = world {ai= not(ai world)}
                         | coordInExtent (aiEasyExtent) (x, y) = world {difficulty=1, ai=True}
                         | coordInExtent (aiMedExtent) (x, y) = world {difficulty=2, ai=True}
                         | otherwise = world
  where board = gameboard world


-- | convert the coordinates (with original origin at the center)
-- | to be integers 0 - 7 and with the origin starting in the bottom left
convertCoords :: Board -> Float -> Float -> Position
convertCoords b x y | y < 0 && x < 0 = (x'-1, y' -1)
                    | y >= 0 && x < 0 = (x'-1, y')
                    | y < 0 = (x', y'-1)
                    | x < 0 = (x'-1, y')
                    | otherwise = (x', y')
  where x' = truncate (x / 50) + 4
        y' = truncate (y / 50) + 4

-------------------------------------------------------------------
--Valid Move functions
validNetworkMove :: World -> Int -> Int -> Bool
validNetworkMove world x y = not (gameOver board) && state == Playing&& (userCol world) == (turn world)
  && network world && inRange board (x, y)
  where board = gameboard world
        state = gameState world

validGameMove :: World -> Int -> Int  -> Bool
validGameMove world x y = not (gameOver board) && ((userCol world) == (turn world) || not (ai world))
  && not (network world) && inRange board (x, y)
  where board = gameboard world

validUndo :: World -> Int -> Int -> Bool
validUndo world x y = not (inRange board (x, y)) && not (gameOver board) && not (network world)
  where board = gameboard world
--------------------------------------------------------------------

--sends a pass over the network
passOverNetwork :: World -> IO World
passOverNetwork world = do hand <- (handle world)
                           sendAcrossNetwork hand (-1) (-1)
                           return world {oldworld = world, turn = other (turn world)}

-- | Makes a move and attempts to send it across the network
moveOverNetwork :: World -> Int -> Int -> IO World
moveOverNetwork world x y = case makeMove board col (x,y) of --make move and send across network
    (Just newBoard') -> do hand <- (handle world)
                           sendAcrossNetwork hand x y
                           return world {gameboard = newBoard', turn = newCol, oldworld = world}
    (Nothing) -> return world --bad move, dont update world
    where board = gameboard world
          col = turn world
          newCol = other col
