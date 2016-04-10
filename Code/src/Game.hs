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
                     args :: [String],
                     turn :: Col }

initBoard :: Bool -> Int -> Board
initBoard reversi size = Board size 0 reversi [((3,4), Black), ((4,4), White),
                       ((3,3), White), ((4,3), Black)]

initWorld :: [String] -> World
initWorld args = World board world args Black
  where board = initBoard (isReversi args) (checkSize args)
        world = initWorld args


-- | Check to see if the user wishes to play the game with the
-- | reversi rule set, that is, that the first two moves by each colour
-- | don't need to result in a flip.
isReversi :: [String] -> Bool
isReversi arguments | length arguments >= 1 && any (== "True") arguments = True
                  | otherwise = False


-- | Read first command line argument for board size, if it is present
-- | otherwise return the default of 8
checkSize :: [String] -> Int
checkSize arguments | length arguments >= 1 = read (head arguments) :: Int
               | otherwise = 8 --default board size


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

-- | Rolls back the world to the previous state
undo :: World -> World
undo world = oldworld world
