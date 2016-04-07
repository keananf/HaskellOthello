module Board where

import Data.Maybe
import Data.List

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
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { gameboard :: Board,
                     turn :: Col }


-- Default board is 8x8, neither played has passed, with 4 initial pieces 
initBoard :: Board
initBoard = Board 8 0 [((3,3), White), ((3,4), Black),
                       ((4,3), Black), ((4,4), White)]

initWorld :: World
initWorld = World initBoard Black

directions :: [Position]
directions =  [(x,y )| x <- [-1..1], y <- [-1..1]]

changeColour :: Col -> Piece -> Piece
changeColour col (pos,_) = (pos, col)

other :: Col -> Col
other Black = White
other White = Black

--------------------------------------------------------------------
-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, there is a piece already there,
-- or the move does not flip any opposing pieces)
makeMove :: World -> Position -> Maybe World
makeMove world pos | not (isOccupied board pos) && (inRange board pos) =
                       Just newWorld {gameboard = newBoard}
                   | otherwise = Nothing
  where board = (gameboard world)
        col = (turn world)
        pieces = flatten (flipPieces board col pos)
        newBoard = updatePieces board pieces pos col
        newWorld = changeTurn world

changeTurn :: World -> World
changeTurn world = world {turn = (other col)}
  where col = turn world

updatePieces :: Board -> [Piece] -> Position -> Col -> Board
updatePieces board newPieces pos col = board {pieces = allPieces}
  where otherPieces = deleteFirstsBy (\x y -> ((fst x) == (fst y))) (pieces board) (newPieces)
        newPiece = (pos, col)
        allPieces = newPiece:newPieces ++ otherPieces

-- | Flatten 2D list of pieces which results from flipPieces and FindPieces
-- | It is 2D to represent all the pieces to flip in all directions
flatten :: [[Piece]] -> [Piece]
flatten [] = []
flatten (p:ps) = p ++ (flatten ps)

-- | Flip the colours of all pieces returned from findPieces
flipPieces :: Board -> Col -> Position -> [[Piece]]
flipPieces board col pos = flippedPieces
  where allPieces = (findPieces board col pos directions)
        piecesToFlip = (filter (\list -> not (null list) && snd (last list) == col) allPieces) --eliminate directions which don't have the same colour at the end 
        flippedPieces =map (\list -> map (changeColour col) list) piecesToFlip

-- | Finds pieces to flip in each direction
findPieces :: Board -> Col -> Position -> [Position] -> [[Piece]]
findPieces board col pos [] = []
findPieces board col pos (d:ds) = pieces:(findPieces board col pos ds)
  where maybePieces = searchDirection board pos d col --find infinite list of maybe pieces in d
        justPieces = takeWhile (isJust) maybePieces --get rid of all nothings
        pieces = map (fromJust) justPieces --all pieces in this direction


-- | Retrieves an infinite list of maybe pieces in this direction
searchDirection :: Board -> Position -> Position -> Col -> [Maybe Piece]
searchDirection board (x,y) (x',y') col = piece:pieces
  where piece = findPiece (board) (x+x', y+y')
        pieces = (searchDirection board (x+x', y+y') (x',y') col)

-- | Finds a maybe piece in the given direction.
-- | Used to find the infinite list for lazy evaluation
findPiece :: Board -> Position -> Maybe Piece
findPiece board pos | length list == 1 = Just (head list)
                    | otherwise = Nothing
  where list = filter (\piece -> (fst piece) == pos) (pieces board)

-- | Check if the space is occupied
isOccupied :: Board -> Position -> Bool
isOccupied board pos = any (== pos) positions
  where positions = map (fst) (pieces board)

-- | Check if the desired position is in range of the board
inRange :: Board -> Position -> Bool
inRange board (x,y) = x < len && x >= 0 && y < len && y >= 0
  where len = (size board)
-----------------------------------------------------------------

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

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate = undefined



