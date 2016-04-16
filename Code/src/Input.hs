module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Extent
import Draw
import Board
import Game
import ClientMain

-- | Update the world state given a click event. It controls calling
-- | the functions for checking if a button was pressed, or if the move
-- | is valid based upon the current settings (network, ai or not)
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

-- | This function details the different behaviour of the space bar based
-- | upon the world state. If paused, it is ignored. Likewise, it takes care
-- | of calling the function to send a pass over the network, if that option is active.
handleInput (EventKey (SpecialKey KeySpace) Up _ _) world
    | (gameState world) == Paused = return world --ignore space on pause
    | not (network world) && not (gameOver board) = return world {gameboard = newBoard,
                                                                  oldworld = world, turn = newCol}
    | (network world) && not (gameOver board) = (passOverNetwork world)
    | otherwise = return (initWorld (args world))--Reset game when gameover and space pressed
    where board = gameboard world
          newBoard = board {passes = (passes board) + 1}
          newCol = other (turn world)

-- | This function is in charge of managing toggling of pausing.
handleInput (EventKey (Char k) Up _ _) world
  | k == 'p' && (gameState world) == Paused = return world {gameState = Playing} --unpause
  | k == 'p' && (gameState world)== Playing && not (network world)=
      return world {gameState = Paused} --pause
  | otherwise = return world
  where rev = not (reversi (gameboard world))
        board = gameboard world

handleInput e world = return world


------------------------------------------------------------------------

-- | Checks to see where the click was on the menu. This is called when
-- | the game is at the menu, and will set the different options depending on
-- | which extent was clicked.
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
  where x' = truncate (x / 50) + (size b) `div` 2
        y' = truncate (y / 50) + (size b) `div` 2

-------------------------------------------------------------------
--Valid Move functions to check when somewhere is clicked on the board

-- | Checks if the move a user wants to make is valid given that the game
-- | is over the network. It checks that the move is in range, that it is the user's turn
-- | and also that its a network game and that the game is not over.
validNetworkMove :: World -> Int -> Int -> Bool
validNetworkMove world x y = not (gameOver board) && state == Playing&& (userCol world) == (turn world)
  && network world && inRange board (x, y)
  where board = gameboard world
        state = gameState world

-- | Checks if the move a user wants to make is valid given that the game
-- | is not over the network. It checks that the move is in range, that it is the user's turn
-- | and also that its not a network game and that the game is not over.
validGameMove :: World -> Int -> Int  -> Bool
validGameMove world x y = not (gameOver board) && ((userCol world) == (turn world) || not (ai world))
  && not (network world) && inRange board (x, y)
  where board = gameboard world

-- | Checks to see if it is a valid undo, that is that its not a network game, the game isnt over,
-- | and that the click was not within the board. 
validUndo :: World -> Int -> Int -> Bool
validUndo world x y = not (inRange board (x, y)) && not (gameOver board) && not (network world)
  where board = gameboard world
--------------------------------------------------------------------

-- Sends a pass over the network, which is indicated through
-- -1,-1
passOverNetwork :: World -> IO World
passOverNetwork world = do hand <- handle world
                           sendAcrossNetwork hand (-1) (-1)
                           return world {oldworld = world, turn = other (turn world)}

-- | Makes a move and attempts to send it across the network.
-- | The world is then updated.
moveOverNetwork :: World -> Int -> Int -> IO World
moveOverNetwork world x y = case makeMove board col (x,y) of --make move and send across network
    (Just newBoard') -> do hand <- (handle world)
                           sendAcrossNetwork hand x y
                           return world {gameboard = newBoard', turn = newCol, oldworld = world}
    (Nothing) -> return world --bad move, dont update world
    where board = gameboard world
          col = turn world
          newCol = other col
