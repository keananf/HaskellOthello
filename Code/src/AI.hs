module AI where

import Board
import Game

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           nextMoves :: [(Position, GameTree)] }

-- Given a function to generate plausible moves (i.e. board positions)
-- for a player (Col) on a particular board, generate a (potentially)
-- infinite game tree.
--
-- (It's not actually infinite since the board is finite, but it's sufficiently
-- big that you might as well consider it infinite!)
--
-- An important part of the AI is the 'gen' function you pass in here.
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.
buildTree :: (Board -> Col -> [Position]) -- ^ Move generator
             -> Board -- ^ board state
             -> Col -- ^ player to play next
             -> GameTree
buildTree gen b c = let moves = gen b c in -- generated moves
                        GameTree b c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove b c pos of -- try making the suggested move
               Nothing -> mkNextStates xs -- not successful, no new state
               Just b' -> (pos, buildTree gen b' (other c)) : mkNextStates xs
                             -- successful, make move and build tree from 
                             -- here for opposite player


genAllMoves :: Board -> Col -> [Position]
genAllMoves board col = detectMoves board col (allPositions board)

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getBestMove i tree= evalLayer tree
  where eval :: GameTree -> Int
        eval tree = evaluate board col
          where board = (game_board tree)
                col = game_turn tree

        evalLayer :: GameTree -> Position
        evalLayer tree = fst (head bestPos)
          where scores = map (\(pos,gametree) -> (pos, eval (gametree))) (nextMoves tree)
                bestScore = maximum (map (snd) scores)
                bestPos = filter (\move -> (snd move) == bestScore) scores

-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> World
updateWorld t w | hasAI && aiColour == col = --ai v player
                    case makeMove board col pos of
                      (Just board') -> w {gameboard = board', turn = other col, oldworld = w}
                      (Nothing) -> w
                | otherwise = w --player v player
                where aiColour = aiCol w
                      col = turn w
                      hasAI = ai w
                      board = gameboard w
                      tree =(buildTree genAllMoves board col) --get entire gametree
                      pos = getBestMove 1 tree --get best move from gametree

{- Hint: 'updateWorld' is where the AI gets called. If the world state
 indicates that it is a computer player's turn, updateWorld should use
 'getBestMove' to find where the computer player should play, and update
 the board in the world state with that move.

 At first, it is reasonable for this to be a random valid move!

 If both players are human players, the simple version above will suffice,
 since it does nothing.

 In a complete implementation, 'updateWorld' should also check if either 
 player has won and display a message if so.
-}


