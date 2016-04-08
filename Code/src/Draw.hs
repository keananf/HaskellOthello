module Draw(drawWorld) where

import Graphics.Gloss
import Board
import Game

type Types = (Picture, Picture, Picture)

-- This extracts the Board from the world state and draws it
-- as a grid plus pieces.
drawWorld :: Types -> World -> Picture
drawWorld tiles world = scale 0.5 0.5 picture 
  where board = gameboard world
        picture = translate (-350.0) (-350.0) (getTiles board tiles)

-- | Retrieves the composite picture of each individual picture representing
-- | each tile
getTiles :: Board -> Types -> Picture
getTiles board types = pictures [getTile board (x,y) types | x <- [0..len-1], y <- [0..len-1]]
  where len = size board


-- | Figure out each type of tile and return the Picture that needs to be printed
getTile :: Board-> (Int,Int) -> Types -> Picture
getTile board (x,y) (tile, blackPiece, whitePiece) = case (findPiece board (x,y)) of
  (Just piece) -> translate x' y' (getColour (blackPiece, whitePiece) piece)
  (Nothing) -> translate x' y' tile
  where pos' = getOffset (x,y)
        x' = fromIntegral (fst pos')
        y' = fromIntegral (snd pos')

-- | if a space contains a piece, return the correct type of piece
getColour :: (Picture, Picture) -> Piece ->Picture
getColour (blackPiece, whitePiece) piece | (snd piece) == Black = blackPiece
                                         | (snd piece) == White = whitePiece

-- | Get the offset so each image is concatenated properly
getOffset :: Position -> (Int, Int)
getOffset (x,y) = (x*100, y*100)


