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
getBestMoveBadAI :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> (Position, GameTree)
getBestMoveBadAI i tree = head bestMove
  where bestScore = evalLayer tree --tuple of pos and score associated with pos
        bestMove = filter (\move -> (fst move) == (fst bestScore)) (nextMoves tree)
        eval :: GameTree -> Int
        eval tree = evaluate board col
          where board = (game_board tree)
                col = game_turn tree

        evalLayer :: GameTree -> (Position, Int)
        evalLayer tree = (head bestPos)
          where scores = map (\(pos,gametree) -> (pos, eval (gametree))) (nextMoves tree)
                bestScore = maximum (map (snd) scores)
                bestPos = filter (\move -> (snd move) == bestScore) scores

getBestMoveGoodAI :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> (Position, GameTree)
getBestMoveGoodAI i tree = head bestMove
  where bestScore = evalNested i tree --tuple of pos and score associated with pos
        bestMove = filter (\move -> (fst move) == (fst bestScore)) (nextMoves tree)
        eval :: GameTree -> Int
        eval tree = evaluate board col
          where board = (game_board tree)
                col = game_turn tree

        evalNested :: Int -> GameTree -> (Position, Int)
        evalNested i gt | i > 0 && length (nextMoves gt) > 0 = head bestPos
                        | length (nextMoves gt) > 0= evalLayer gt
                       -- | otherwise = ((-1,-1), 0)
          where results = (map (\t -> (fst t, snd (evalNested (i-1) (snd t)))) (nextMoves gt))
                bestScore = maximum (map (snd) results)
                bestPos = filter (\move -> (snd move) == bestScore) results

        evalLayer :: GameTree -> (Position, Int)
        evalLayer tree = (head bestPos)
          where scores = map (\(pos,gametree) -> (pos, eval (gametree))) (nextMoves tree)
                bestScore = maximum (map (snd) scores)
                bestPos = filter (\move -> (snd move) == bestScore) scores



-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> World
updateWorld t w | hasNetwork = w --read move from network, update world
                | hasAI && aiColour == col && length (nextMoves tree) > 0 = --ai v player
                    w {gameboard = newBoard, turn = other col, oldworld = w}
                | otherwise = w --player v player or ai has to pass
                where aiColour = aiCol w
                      col = turn w
                      hasAI = ai w
                      hasNetwork = network w
                      board = gameboard w
                      tree =(buildTree genAllMoves board col) --get entire gametree
                      (pos,newTree) = getBestMoveGoodAI 2 tree --get best move from gametree
                      newBoard = (game_board newTree) --get new board from the move associated with this tree

{- In a complete implementation, 'updateWorld' should also check if either 
 player has won and display a message if so.
-}


