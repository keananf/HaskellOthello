module Game where

import Data.Char
import System.IO
import ClientMain

data Col = Black | White
  deriving (Show, Eq)

type Position = (Int, Int)
type Piece = (Position, Col)


data GameState = Menu | Paused | GameOver | Playing
  deriving Eq
-- A Board is a record containing the board size (a board is a square grid, n *
-- n), the number of consecutive passes, and a list of pairs of position and
-- the colour at that position.

data Board = Board { size :: Int,
                     passes :: Int,
                     reversi :: Bool,
                     pieces :: [Piece]
                   }
  deriving Show

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (infomation for the AI, for example, such as where the
-- most recent moves were).
data World = World { gameboard :: Board,
                     gameState :: GameState,
                     oldworld :: World, --allows for undo
                     hints :: Bool, --if hints are on or not
                     ai :: Bool, --if ai is on
                     difficulty :: Int, --difficulty of ai
                     network :: Bool, --if network is on
                     handle :: IO Handle, --handle representing network connection
                     file :: FilePath,
                     aiCol :: Col, --ai colour
                     userCol :: Col, --user colour
                     args :: [String], --all args passed in
                     time :: Float, --time remaining for this turn
                     turn :: Col }

initBoard :: Bool -> Board
initBoard reversi | reversi== False = Board 8 0 reversi [((3,4), Black), ((4,4), White),
                                                         ((3,3), White), ((4,3), Black)]
                  | otherwise = Board 8 0 reversi []

initWorld :: [String] -> World
initWorld args = (World board Menu oldWorld hints ai difficulty network
                  getHandle filePath aiCol userCol args 10.0 Black)
  where board = initBoard (isReversi args network)
        oldWorld = initWorld args
        hints = hasHints args
        ai = hasAI args
        network = hasNetwork args
        filePath = "/saveFile.txt"
        difficulty = aiDifficulty args
        userCol = userColour args
        aiCol = opp userCol

getHandle :: IO Handle
getHandle = do handle <- client "localhost" 4024
               return handle

userColour :: [String] -> Col
userColour arguments | length arguments >= 1 && any (=="user=white") arguments = White
                     | otherwise = Black

aiDifficulty :: [String] -> Int
aiDifficulty arguments | length arguments >= 1 && any (=="medium") arguments = 2
                       | otherwise = 1

hasAI :: [String] -> Bool
hasAI arguments | length arguments >= 1 && any (=="ai") arguments = True
                | otherwise = False

hasNetwork :: [String] -> Bool
hasNetwork arguments | length arguments >= 1 && any (=="network") arguments = True
                | otherwise = False

-- | Check to see if the user wishes to play the game with the
-- | reversi rule set, that is, that the first two moves by each colour
-- | don't need to result in a flip.
isReversi :: [String] -> Bool -> Bool
isReversi arguments net| length arguments >= 1 && net == False && any (== "reversi") arguments = True
                       | otherwise = False

hasHints :: [String] -> Bool
hasHints arguments | length arguments >= 1 && any (=="hints") arguments = True
                   | otherwise = False

opp :: Col -> Col
opp Black = White
opp White = Black


-----------------------------------------------------------------
-- Check the current score
-- Returns a pair of the number of black pieces, and the number of
-- white pieces
checkScore :: Board -> (Int, Int)
checkScore board = countPieces (0,0) (colors)
                   where colors = map (snd) (pieces board)

-- | counts the numbers of each type of piece and returns a tuple
countPieces :: (Int, Int) -> [Col] -> (Int, Int)
countPieces (x,y) [] = (x,y)
countPieces (x,y) (z:zs) | Black == z = countPieces (x+1,y) zs
                         | otherwise = countPieces (x,y+1) zs
----------------------------------------------------------------

-- Return true if the game is complete (that is, either the board is
-- full or there have been two consecutive passes)
gameOver :: Board -> Bool
gameOver board | (passes board) == 2 = True
               | length (pieces board) == (size board) * (size board) = True
               | otherwise = False

-- | Attempts to undo a move given the particular type of game
undoMove :: World -> IO World
undoMove world | ((gameState (oldworld world)) == Menu && not (ai world)) ||
                 ((gameState (oldworld (oldworld world)) == Menu) && (ai world)) = return world

               | not (ai world) = (return (oldworld world){time=10.0}) --undoes move for player v player
               | otherwise = return ((oldworld (oldworld world)){time=10.0}) --undoes for ai v player
               where networkHandle = handle world
                     userColour = userCol world
                     col = turn world

-- | Saves the list of pieces into the file stored in the world.
saveGame :: World -> IO()
saveGame world = do let game = (pieces (gameboard world))
                    handle <- openFile (file world) WriteMode
                    hPrint handle (args world)
                    hPrint handle game

loadGame :: World -> IO World
loadGame world = do handle <- readFile (file world)
                    args <- readLines 1 handle
                    pieces <- rList handle
                    let world = initWorld args
                    return world{gameboard = (loadPiecesToBoard pieces)}


loadPiecesToBoard :: [Piece] -> Board
loadPiecesToBoard gamePieces = (initBoard False){pieces=gamePieces}
