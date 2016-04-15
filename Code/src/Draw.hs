module Draw(drawWorld, undoExtent, hintsExtent, playExtent, aiExtent, aiEasyExtent, aiMedExtent) where

import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Board
import Game

type Types = (Picture, Picture, Picture, Picture)

-- This extracts the Board from the world state and draws it
-- as a grid plus pieces.
drawWorld :: Types -> World -> Picture
drawWorld tiles world | (gameState world) == Playing = scale sf sf picture --draw board if not game over
                      | (gameState world) == Menu = scale sf sf (drawMenu b)
                      | (gameState world) == Paused = scale sf sf (pictures ((drawPause b) ++ (map (centreImg) score)))
                      | otherwise = scale sf sf (pictures ((drawGameOver b) ++ (map (centreImg) score)))
  where b = gameboard world
        boardTiles = (getTiles b world tiles) --list of pictures representing the tiles
        score = drawScore b
        turnImg = drawTurn world
        picture = centreImg (pictures(turnImg:(score ++ boardTiles ++(drawButtons b))))

-- | Retrieves a list of each individual picture representing
-- | each tile
getTiles :: Board -> World -> Types -> [Picture]
getTiles board world types = [getTile world board (x,y) types | x <- [0..len], y <- [0..len]]
  where len = (size board) - 1


-- | Figure out each type of tile and return the Picture that needs to be printed
getTile :: World -> Board -> (Int,Int) -> Types -> Picture
getTile world board (x,y) (tile, blackPiece, whitePiece, movePiece) =
  case (findPiece board (x,y)) of --if a piece exists at these coordinates
    (Just piece) -> translate x' y' (getColour (blackPiece, whitePiece) piece)
    (Nothing) -> translate x' y' (checkValidMove board world x y (tile, movePiece))
  where pos' = getOffset (x,y)
        x' = fromIntegral (fst pos') --convert to float
        y' = fromIntegral (snd pos')

-- check if the tile at the given coordinates represents a potential valid move
-- so that a hint can be displayed (if active). Otherwise, just print an empty tile
checkValidMove :: Board -> World -> Int -> Int -> (Picture,Picture) -> Picture
checkValidMove board world x y (tile, moveTile)
  | any (==(x,y)) potentialMoves  && (hints world) = moveTile
  | otherwise = tile
  where potentialMoves = detectMoves board col (allPositions board)
        col = turn world



-----------------------------------------------------------------------------------
--Draw text functions


-- | draws the gameover screen
drawGameOver :: Board -> [Picture]
drawGameOver b = pics
  where background = (rectangleSolid (backgroundSize b) (backgroundSize b))
        gameOverMsg = color white (translate (0) (-550) (centreImg(text ("Game Over."))))
        continueMsg = color white (translate (-850) (-750) (text ("Press Space to Start Again")))
        pics = (background:[(scale sf sf gameOverMsg), (scale sf sf continueMsg)])

-- | draws the pause screen
drawPause :: Board -> [Picture]
drawPause b = pics
  where background = (rectangleSolid (backgroundSize b) (backgroundSize b))
        gameOverMsg = color white (translate (0) (-550) (centreImg(text ("Paused."))))
        continueMsg = color white (translate (-850) (-750) (text ("Press 'p' to Continue")))
        pics = (background:[(scale sf sf gameOverMsg), (scale sf sf continueMsg)])


-- | draws the menu screen
drawMenu :: Board -> Picture
drawMenu b = pictures (pics)
  where background = (rectangleSolid (backgroundSize b) (backgroundSize b))
        playMsg = color white (translate (250) (-200) (centreImg(text ("Play"))))
        aiMsg = color white (translate (1300) (375) (centreImg(text ("AI"))))
        aiEasyMsg = color white (translate (1300) (75) (centreImg(text ("Easy"))))
        aiMediumMsg = color white (translate (1300) (-175) (centreImg(text ("Medium"))))
        options = map (\pic -> scale sf sf pic) (aiMsg:[aiEasyMsg, aiMediumMsg])
        titleMsg = color white (translate (200) (550) (centreImg(text ("Othello"))))
        pics = (background:[playMsg, (scale 1.5 1.5 titleMsg)]) ++ options


-- | Display whose turn it is at the bottom of the board
drawTurn :: World -> Picture
drawTurn world = (color white pic)
  where pic = translate (centre) (0) (scale sf sf (centreImg (text ("Turn: " ++ show(col)))))
        col = turn world
        centre = (backgroundSize (gameboard world)) / 2 - fromIntegral(sizeOfTile) --x y from right

-- | Prints the Score of each colour to the right of the board
drawScore :: Board -> [Picture]
drawScore board = map (\pic -> (color white pic)) pics
  where scores = checkScore board
        len = backgroundSize board
        score1 = translate (len) (len-(0.25 *len)) (scale sf sf(text ("Black: " ++ show(fst scores))))
        score2 = translate (len) (len-(0.75*len)) (scale sf sf(text ("White: " ++ show(snd scores))))
        pics = (score1:[score2])

---------------------------------------------------------------
--Buttons
drawButtons :: Board -> [Picture]
drawButtons board = (undoButton board): [(hintsButton board)]

undoButton :: Board -> Picture
undoButton board = translate  (len-1.35*len) (len-(0.25 *len)) (scale sf sf (text ("UNDO")))
  where len = backgroundSize board

hintsButton :: Board -> Picture
hintsButton board = translate  (len-1.35*len) (len-(0.75 *len)) (scale sf sf (text ("HINTS")))
  where len = backgroundSize board


-------------------------------------------------------------
--Extents

undoExtent :: Board -> Extent
undoExtent b = makeExtent 7 6 (0) (-3)
  where len = backgroundSize b

hintsExtent :: Board -> Extent
hintsExtent b = makeExtent (3) (2) (0) (-3)
  where len = backgroundSize b

playExtent :: Board -> Extent
playExtent b = makeExtent (0) (-2) (5) (3)
  where len = backgroundSize b

aiExtent :: Board -> Extent
aiExtent b = makeExtent (5) (4) (10) (8)
  where len = backgroundSize b

aiEasyExtent :: Board -> Extent
aiEasyExtent b = makeExtent (3) (2) (10) (7)
  where len = backgroundSize b

aiMedExtent :: Board -> Extent
aiMedExtent b = makeExtent (2) (1) (11) (7)
  where len = backgroundSize b

-------------------------------------------------------------

-------------------------------------------------------------
-- | return correct image that corresponds with a piece's colour
getColour :: (Picture, Picture) -> Piece ->Picture
getColour (blackPiece, whitePiece) piece | (snd piece) == Black = blackPiece
                                         | (snd piece) == White = whitePiece

-- | Get the offset so each image is concatenated properly
getOffset :: Position -> (Int, Int)
getOffset (x,y) = (x*sizeOfTile, y*sizeOfTile)

-- | scale factor
sf :: Float
sf = 0.5

-- | size in pixels of each tile
sizeOfTile :: Int
sizeOfTile = 100

-- | size of composite image
backgroundSize :: Board -> Float
backgroundSize board = fromIntegral( sizeOfTile * (size board))

-- | centres image
centreImg :: Picture -> Picture
centreImg board= translate (-350.0) (-350.0) board


