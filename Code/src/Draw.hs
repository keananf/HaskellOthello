module Draw(drawWorld) where

import Graphics.Gloss
import Board
import Game

type Types = (Picture, Picture, Picture, Picture)

-- This extracts the Board from the world state and draws it
-- as a grid plus pieces.
drawWorld :: Types -> World -> Picture
drawWorld tiles world | not (gameOver board) = scale 0.5 0.5 picture
                      | otherwise = scale 0.5 0.5 (pictures (drawGameOver ++ (drawScore board)))
  where board = gameboard world
        boardTiles = (getTiles board world tiles)
        score = drawScore board
        turnImg = drawTurn world
        picture = translate (-350.0) (-350.0) (pictures(turnImg:(score ++ boardTiles)))

-- | draws the gameover screen
drawGameOver :: [Picture]
drawGameOver = pics
  where background = (rectangleSolid 800 800)
        gameOverMsg :: Picture
        gameOverMsg = color white (translate (-350) (-150) (text ("Game Over.")))
        continueMsg = color white (translate (-850) (-350) (text ("Press Space to Start Again")))
        pics = (background:[(scale 0.5 0.5 gameOverMsg), (scale 0.5 0.5 continueMsg)])

-- | Display whose turn it is at the bottom of the board
drawTurn :: World -> Picture
drawTurn world = scale 0.5 0.5 (color white pic)
  where pic = translate 375 (-300) (text ("Turn: " ++ show(col)))
        col = turn world

-- | Prints the Score of each colour to the right of the board
drawScore :: Board -> [Picture]
drawScore board = map (\pic -> scale 0.5 0.5 (color white pic)) pics 
  where scores = checkScore board
        score1 = translate (1700) (800) (text ("Black: " ++ show(fst scores)))
        score2 = translate (1700) (300)(text ("White: " ++ show(snd scores)))
        pics = (score1:[score2])

-- | Retrieves the composite picture of each individual picture representing
-- | each tile
getTiles :: Board -> World -> Types -> [Picture]
getTiles board world types = [getTile world board (x,y) types | x <- [0..len-1], y <- [0..len-1]]
  where len = size board


-- | Figure out each type of tile and return the Picture that needs to be printed
getTile :: World -> Board -> (Int,Int) -> Types -> Picture
getTile world board (x,y) (tile, blackPiece, whitePiece, movePiece) = case (findPiece board (x,y)) of
  (Just piece) -> translate x' y' (getColour (blackPiece, whitePiece) piece)
  (Nothing) -> translate x' y' (checkValidMove board world x y (tile, movePiece))
  where pos' = getOffset (x,y)
        x' = fromIntegral (fst pos')
        y' = fromIntegral (snd pos')

checkValidMove :: Board -> World -> Int -> Int -> (Picture,Picture) -> Picture
checkValidMove board world x y (tile, moveTile)
  | any (==(x,y)) potentialMoves  && (hints world) = moveTile
  | otherwise = tile
  where potentialMoves = detectMoves board col (allPositions board)
        col = turn world

-- | if a space contains a piece, return the correct type of piece
getColour :: (Picture, Picture) -> Piece ->Picture
getColour (blackPiece, whitePiece) piece | (snd piece) == Black = blackPiece
                                         | (snd piece) == White = whitePiece

-- | Get the offset so each image is concatenated properly
getOffset :: Position -> (Int, Int)
getOffset (x,y) = (x*100, y*100)


