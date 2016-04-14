module Game where

import Data.Char
import System.IO
import System.IO.Unsafe
import ClientMain

data Col = Black | White
  deriving (Show, Eq)

type Position = (Int, Int)
type Piece = (Position, Col)


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
                     oldworld :: World, --allows for undo
                     hints :: Bool, --if hints are on or not
                     ai :: Bool,
                     difficulty :: Int,
                     network :: Bool,
                     handle :: Handle,
                     aiCol :: Col,
                     userCol :: Col,
                     args :: [String],
                     turn :: Col }

initBoard :: Bool -> Int -> Board
initBoard reversi size = Board size 0 reversi [((3,4), Black), ((4,4), White),
                       ((3,3), White), ((4,3), Black)]

initWorld :: [String] -> World
initWorld args = World board world hints ai difficulty network
  (unsafePerformIO getHandle) aiCol userCol args Black
  where board = initBoard (isReversi args) (checkSize args)
        hints = hasHints args
        ai = hasAI args
        network = hasNetwork args
        difficulty = aiDifficulty args
        userCol = userColour args
        aiCol = opp userCol
        world = initWorld args

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

getHandle :: IO Handle
getHandle = do
  handle <- client "localhost" 4024
  return (handle)

-- | Check to see if the user wishes to play the game with the
-- | reversi rule set, that is, that the first two moves by each colour
-- | don't need to result in a flip.
isReversi :: [String] -> Bool
isReversi arguments | length arguments >= 1 && any (== "reversi") arguments = True
                    | otherwise = False


-- | Read first command line argument for board size, if it is present
-- | otherwise return the default of 8
checkSize :: [String] -> Int
checkSize arguments | length nums >= 1 = read (head nums) :: Int
                    | otherwise = 8 --default board size
                    where nums = filter (\arg -> all isDigit arg) arguments

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
undoMove world | network world && userColour == col= do let newWorld = oldworld (oldworld world)
                                                        sendAcrossNetwork networkHandle (-2) (-2)
                                                        return newWorld
               | not (ai world) = do let newWorld = oldworld world--undoes move for player v player
                                     print ""
                                     return newWorld
               | otherwise = do let newWorld = oldworld (oldworld world) --undoes for ai v player
                                print ""
                                return newWorld
               where networkHandle = handle world
                     userColour = userCol world
                     col = turn world
