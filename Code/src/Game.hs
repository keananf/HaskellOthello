module Game where

data Col = Black | White
  deriving (Show, Eq)

type Position = (Int, Int)
type Piece = (Position, Col)


-- A Board is a record containing the board size (a board is a square grid, n *
-- n), the number of consecutive passes, and a list of pairs of position and
-- the colour at that position.

data Board = Board { size :: Int,
                     passes :: Int,
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
                     oldboard :: Board,
                     turn :: Col }


-- Default board is 8x8, neither played has passed, with 4 initial pieces 
initBoard :: Board
initBoard = Board 8 0 [((3,4), Black), ((4,4), White),
                       ((3,3), White), ((4,3), Black)]

initWorld :: World
initWorld = World board board Black
  where board = initBoard

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
-----------------------------------------------------------------


-- Return true if the game is complete (that is, either the board is
-- full or there have been two consecutive passes)
gameOver :: Board -> Bool
gameOver board | (passes board) == 2 = True
               | length (pieces board) == (size board) * (size board) = True
               | otherwise = False

