module Board(evaluate, makeMove, inRange, isOccupied, allPositions, detectMoves, findPiece, other) where

import Data.Maybe
import Data.List
import Game

directions :: [Position]
directions =  [(x,y )| x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]

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
makeMove board col pos | validMove = Just newBoard
                   | otherwise = Nothing
  where flippedPieces = (flipPieces board col pos)
        newBoard = updatePieces board flippedPieces pos col

        -- | Detects a valid move given the rule set
        validMove :: Bool
        validMove = not (isOccupied board pos) && (inRange board pos) --in range and open space
          && ((reversi board && validReversi board pos && length flippedPieces == 0)
              || (not (validReversi board pos) && length flippedPieces > 0)) --if not reversi, have to flip for legal move

-- | This function looks at all flipped pieces, and checks
-- | if the piece existed before. If so, the original piece is discarded
-- | and then the list representing non-flipped pieces is added to the list of
-- | flipped Pieces.
updatePieces :: Board -> [Piece] -> Position -> Col -> Board
updatePieces board newPieces pos col = board {pieces = allPieces, passes = 0}
  where otherPieces = deleteFirstsBy (\x y -> ((fst x) == (fst y))) (pieces board) (newPieces)
        newPiece = (pos, col)
        allPieces = newPiece:newPieces ++ otherPieces


-- | Flip the colours of all pieces returned from findPieces
-- | by mapping the changeColour function to all pieces returned
-- | from it, and then returning that list.
flipPieces :: Board -> Col -> Position -> [Piece]
flipPieces board col pos = flippedPieces
  where allPieces = (findPieces board col pos directions)
        flippedPieces = map (changeColour col) allPieces --flip


-- | Finds all pieces to flip in all directions, if any.
-- | This function is called with detectMoves and flipPieces, to
-- | ascertain if a move is valid (flips pieces). A direction
-- | which does not contain the colour of the current turn is discarded
findPieces :: Board -> Col -> Position -> [Position] -> [Piece]
findPieces board col pos [] = []
findPieces board col pos (d:ds) | containsCol pieces col = piecesToFlip ++ (findPieces board col pos ds)
                                | otherwise = findPieces board col pos ds
  where maybePieces = searchDirection board pos d col --find infinite list of maybe pieces in d
        justPieces = takeWhile (isJust) maybePieces --get rid of all nothings
        pieces = map (fromJust) justPieces --all pieces in this direction
        piecesToFlip = (takeWhile (\piece -> (snd piece) /= col)) pieces -- pieces to flip in this dir

        containsCol :: [Piece] -> Col ->Bool
        containsCol list col = (any (\elem -> (snd elem) == col) list)

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


-----------------------------------------------------------------
-- | Following functions ascertain if a move is valid

-- | Check if the space is occupied
isOccupied :: Board -> Position -> Bool
isOccupied board pos = any (== pos) positions
  where positions = map (fst) (pieces board)

-- | Check if the desired position is in range of the board
inRange :: Board -> Position -> Bool
inRange board (x,y) = x < len && x >= 0 && y < len && y >= 0
  where len = (size board)

-- | This function looks to see if the given position represents a valid
-- | reversi move, that is, that it is within the middle four squares and that
-- | there are less than four pieces on the board.
validReversi :: Board -> Position-> Bool
validReversi board pos | x <= len `div` 2 && x >= len `div` 2 -1
                         && y <= len `div` 2 && y >= len `div` 2 -1
                         && length (pieces board) < 4 = True
                       | otherwise = False
  where x = fst pos
        y = snd pos
        len = size board

----------------------------------------------------------------
--Following functions for checking which tiles are valid moves
--to print hints, as well as for the AI to check which moves are valid

-- | Function for generating a list of all positions
allPositions :: Board -> [Position]
allPositions board =  [(x,y) | x <- [0..len-1], y <- [0..len-1]]
  where len = size board

-- | Detect for each position on the board which ones result in valid moves.
-- | This function is called on for every position (the result of allPositions)
-- | and is used to display hints (which moves are valid) and for the AI to look
-- | for the best move.
detectMoves :: Board -> Col -> [Position] ->[Position]
detectMoves board col [] = []
detectMoves board col (x:xs) | (validReversi board x)
                               && length flippedPieces ==0 = [x] ++ positions
                             | length (pieces board) >= 4
                               && length flippedPieces >= 1 = [x] ++ positions
                             | otherwise = positions
  where flippedPieces = flipPieces board col x
        positions = detectMoves board col xs

-----------------------------------------------------------------



-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> World -> Int
evaluate board col w | col == userColour && userColour == Black = fst scores - snd scores
                     | col /= userColour && userColour == Black = snd scores - fst scores
                     | col == userColour && userColour == White = snd scores - fst scores
                     | col /= userColour && userColour == White = fst scores - snd scores
                     | otherwise = snd scores - fst scores
  where scores = checkScore board
        userColour = userCol w



