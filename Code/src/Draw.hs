module Draw
(
 drawWorld
,menuExtent
,undoExtent
,hintsExtent
,playExtent
,aiExtent
,aiEasyExtent
,aiMedExtent
,pictureOrBlank
,Atlas(..)
) where

import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Board
import Game
import Data.List

data Atlas = Atlas { background :: Picture
                   , sidePanel :: Picture
                   , tile :: Picture
                   , blackPiece :: Picture
                   , whitePiece :: Picture
                   , hintPiece :: Picture }

-- This extracts the Board from the world state and draws it
-- as a grid plus pieces.
drawWorld :: Atlas -> World -> IO Picture
drawWorld atlas world | (gameState world) == Playing =
                          return (scale sf sf picture) --draw board if not game over
                      | (gameState world) == Menu = return (scale sf sf (drawMenu world))
                      | (gameState world) == Paused =
                          return (scale sf sf (pictures ((drawPause b) ++ (map (centreImg) score))))

                      | otherwise =
                          return (scale sf sf (pictures ((drawGameOver b) ++ (map (centreImg) score))))
  where b = gameboard world
        boardTiles = (getTiles b world atlas) --list of pictures representing the tiles
        score = drawScore world
        bg = translate (350) (350) (background atlas)
        sp = translate (350) (350) (sidePanel atlas)
        turnImg = drawTurn world
        picture = centreImg ( pictures ([bg, sp, turnImg] ++ score ++ boardTiles ++ (drawButtons b)) )

-- | Retrieves a list of each individual picture representing
-- | each tile
getTiles :: Board -> World -> Atlas -> [Picture]
getTiles board world atlas = [getTile world board (x,y) atlas | x <- [0..len], y <- [0..len]]
  where len = (size board) - 1


-- | for the particular tile location create the appropriate picture
getTile :: World -> Board -> (Int,Int) -> Atlas -> Picture
getTile world board (x,y) atlas = pictures [
           translate x' y' (checkValidMove board world x y (hintPiece atlas)),
           ( case (findPiece board (x,y)) of
               Just piece -> translate x' y' (getColour (blackPiece atlas, whitePiece atlas) piece)
               Nothing    -> Blank ),
           translate x' y' (tile atlas)]
  where pos' = getOffset (x,y)
        x' = fromIntegral (fst pos') --convert to float
        y' = fromIntegral (snd pos')

-- check if the tile at the given coordinates represents a potential valid move
-- so that a hint can be displayed (if active). Otherwise, just print an empty tile
checkValidMove :: Board -> World -> Int -> Int -> Picture -> Picture
checkValidMove board world x y moveTile
  | any (==(x,y)) potentialMoves  && (hints world) = moveTile
  | otherwise = Blank
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
drawMenu :: World -> Picture
drawMenu w | (ai w) && (difficulty w) == 1 = pictures (pics ++ [(scale sf sf (color white aiMediumMsg)),
             (color green (scale sf sf aiEasyMsg)), (color green (scale sf sf aiMsg))])
           | (ai w) && (difficulty w) == 2 = pictures (pics ++ [(scale sf sf (color white aiEasyMsg)),
             (color green (scale sf sf aiMediumMsg)), (color green (scale sf sf aiMsg))])
           | otherwise = pictures (pics ++ map (\pic -> color white (scale sf sf pic))
                                   [aiEasyMsg,aiMsg,aiMediumMsg])
  where b = gameboard w
        background = (rectangleSolid (backgroundSize b) (backgroundSize b))
        playMsg = color white (translate (250) (-200) (centreImg(text ("Play"))))
        aiMsg = (translate (1300) (375) (centreImg(text ("AI"))))
        aiEasyMsg = (translate (1300) (75) (centreImg(text ("Easy"))))
        aiMediumMsg = (translate (1300) (-175) (centreImg(text ("Medium"))))
        titleMsg = color white (translate (200) (550) (centreImg(text ("Othello"))))
        pics = (background:[playMsg, (scale 1.5 1.5 titleMsg)])


-- | Display whose turn it is at the bottom of the board
drawTurn :: World -> Picture
drawTurn world = (color white pic)
  where pic = translate (centre) (0) (scale sf sf (centreImg (text ("Turn: " ++ show(col)))))
        col = turn world
        centre = (backgroundSize (gameboard world)) / 2 - fromIntegral(sizeOfTile) --x y from right

-- | Prints the Score of each colour to the right of the board
drawScore :: World -> [Picture]
drawScore w | not (network w) = map (\pic -> (color white pic)) pics
            | otherwise = map (\pic -> (color white pic)) [score1,score2]
  where scores = checkScore (gameboard w)
        len = backgroundSize (gameboard w)
        score1 = translate (len) (len-(0.25 *len)) (scale sf sf(text ("Black: " ++ show(fst scores))))
        score2 = translate (len) (len-(0.5*len)) (scale sf sf(text ("White: " ++ show(snd scores))))
        timeLeft = translate (len) (len-(0.75*len)) (scale sf sf(text ("Time: " ++ show(truncate(time w)))))
        pics = [score1,score2, timeLeft]

---------------------------------------------------------------
--Buttons
drawButtons :: Board -> [Picture]
drawButtons board = [(undoButton board), (menuButton board), (hintsButton board)]

undoButton :: Board -> Picture
undoButton board = translate  (len-1.35*len) (len-(0.25 *len)) (scale sf sf (text ("UNDO")))
  where len = backgroundSize board


menuButton :: Board -> Picture
menuButton board = translate  (len-1.35*len) (len-(0.5 *len)) (scale sf sf (text ("MENU")))
  where len = backgroundSize board

hintsButton :: Board -> Picture
hintsButton board = translate  (len-1.35*len)  (len-(0.75 *len))(scale sf sf (text ("HINTS")))
  where len = backgroundSize board


-------------------------------------------------------------
--Extents

undoExtent ::Extent
undoExtent = makeExtent 7 6 0 (-3)

menuExtent :: Extent
menuExtent = makeExtent 5 4 0 (-3)

hintsExtent :: Extent
hintsExtent = makeExtent 3 2 0 (-3)

playExtent :: Extent
playExtent = makeExtent (0) (-2) (5) (3)

aiExtent :: Extent
aiExtent  = makeExtent (5) (4) (10) (8)

aiEasyExtent :: Extent
aiEasyExtent = makeExtent (3) (2) (10) (7)

aiMedExtent ::  Extent
aiMedExtent = makeExtent (2) (1) (11) (7)

-------------------------------------------------------------

-------------------------------------------------------------
-- | return correct image that corresponds with a piece's colour
getColour :: (Picture, Picture) -> Piece -> Picture
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

-- | return Just pic or a Blank Picture, depending on the passed Maybe Picture
pictureOrBlank :: Maybe Picture -> Picture
pictureOrBlank (Just pic) = pic
pictureOrBlank Nothing = Blank
