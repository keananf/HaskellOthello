module Board where

import Data.Maybe
import Data.List
import Game

directions :: [Position]
directions =  delete (0,0) [(x,y )| x <- [-1..1], y <- [-1..1]]

changeColour :: Col -> Piece -> Piece
changeColour col (pos,_) = (pos, col)

other :: Col -> Col
other Black = White
other White = Black

--------------------------------------------------------------------
-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, there is a piece already there,
-- or the move does not flip any opposing pieces)
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove board col pos |
  not (isOccupied board pos) && (inRange board pos)
  && ((reversi board && length (pieces board) < 8) --dont need to flip
      || length flippedPieces > 0) = Just newBoard
                   | otherwise = Nothing
  where flippedPieces = flatten (flipPieces board col pos)
        newBoard = updatePieces board flippedPieces pos col

updatePieces :: Board -> [Piece] -> Position -> Col -> Board
updatePieces board newPieces pos col = board {pieces = allPieces, passes = 0}
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
        --eliminate directions which don't have the same colour at the end
        filteredPieces = filterList allPieces 
        piecesToFlip = map (takeWhile (\piece -> (snd piece) /= col)) filteredPieces
        flippedPieces =map (\list -> map (changeColour col) list) piecesToFlip --flip

        filterList :: [[Piece]] -> [[Piece]]
        filterList allPieces = filter (\list -> not (null list) && containsCol list col) allPieces
        containsCol :: [Piece] -> Col ->Bool
        containsCol list col = (any (\elem -> (snd elem) == col) list)

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

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate = undefined



