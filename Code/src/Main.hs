module Main where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import System.Environment
import Draw
import Input
import AI
import Game
-- 'play' starts up a graphics window and sets up handlers for dealing
-- with inputs and updating the world state.
--
-- 'drawWorld' converts the world state into a gloss Picture
--
-- 'handleInput' is called whenever there is an input event, and if it is
-- a human player's turn should update the board with the move indicated by
-- the event
--
-- 'updateWorld' is called 10 times per second (that's the "10" parameter)
-- and, if it is an AI's turn, should update the board with an AI generated
-- move

main :: IO ()
main = do sidePanel  <- loadJuicyPNG "../textures/side-panel.png"
          tile       <- loadJuicyPNG "../textures/tile-white.png"
          blackPiece <- loadJuicyPNG "../textures/piece-black.png"
          whitePiece <- loadJuicyPNG "../textures/piece-white.png"
          movePiece  <- loadJuicyPNG "../textures/piece-hint.png"
          args       <- getArgs
          playIO (InWindow "Othello" (750, 750) (10, 10)) black 10
            (initWorld args)
            (drawWorld (Atlas (pictureOrBlank sidePanel)
                              (pictureOrBlank tile)
                              (pictureOrBlank blackPiece)
                              (pictureOrBlank whitePiece)
                              (pictureOrBlank movePiece)) ) -- in Draw.hs
            handleInput -- in Input.hs
            updateWorld -- in AI.hs
