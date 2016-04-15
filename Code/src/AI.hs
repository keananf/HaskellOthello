module AI where

import Board
import Game
import ClientMain
import System.IO.Unsafe


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
getBestMoveBadAI :: GameTree -> World -> (Position, GameTree)
getBestMoveBadAI tree w = head bestMove
  where bestScore = evalLayer tree w --tuple of pos and score associated with pos
        bestMove = filter (\move -> (fst move) == (fst bestScore)) (nextMoves tree)


getBestMoveGoodAI :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> World
               -> (Position, GameTree)
getBestMoveGoodAI i tree w = head bestMove
  where bestScore = evalNested i tree --tuple of pos and score associated with pos
        bestMove = filter (\move -> (fst move) == (fst bestScore)) (nextMoves tree)

        evalNested :: Int -> GameTree -> (Position, Int)
        evalNested i gt | i > 0 && length (nextMoves gt) > 0 = head bestPos
                        | length (nextMoves gt) > 0= evalLayer gt w
                        | otherwise = ((-1,-1), -100)
          where results = (map (\t -> (fst t, snd (evalNested (i-1) (snd t)))) (nextMoves gt))
                bestScore = maximum (map (snd) results)
                bestPos = filter (\move -> (snd move) == bestScore) results --get pos associated with score

-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update
            -> World -- ^ current world state
            -> IO World
updateWorld t w | gameOver board = return w {gameState=GameOver}
                | (time w) <= 0.0 = return w {turn = other (turn w), oldworld = w, time = 10.0}
                --read move from network, update world
                | (network w) && (userCol w) /= col && state == Playing =
                    moveFromNetwork w

                --ai v player
                | state==Playing && hasAI && aiColour == col && length (nextMoves tree) > 0 =
                    return w {gameboard = newBoard, turn = other col, oldworld = w, time=10.0}

                | state == Playing && hasAI && aiColour == col = --ai has to pass
                    return w {turn = other col, oldworld = w, time=10.0}
                | state == Menu || state == Paused = return w --game not started yet or paused

                | not (network w) = return w {time = (time w) - 0.1}
                | otherwise = return w--player v player
                where aiColour = aiCol w
                      col = turn w
                      hasAI = ai w
                      state = gameState w
                      board = gameboard w
                      tree =(buildTree genAllMoves board col) --get entire gametree
                      (pos,newTree) = chooseAI w tree --get best move from gametree
                      newBoard = (game_board newTree) --get new board from the move associated with this tree

chooseAI :: World -> GameTree-> (Position, GameTree)
chooseAI world tree | (difficulty world) == 1 = getBestMoveBadAI tree world --easy
                    | otherwise = getBestMoveGoodAI 3 tree world --medium

-- |Processes move received from network
moveFromNetwork :: World -> IO World
moveFromNetwork w = do hand <- (handle w)
                       (x,y) <- readAcrossNetwork hand
                       case (x,y) of
                         (-3, -3) -> return w {turn = col, oldworld = oldWorld,
                                               ai = True, aiCol = col, network = False}
                           --not a pass
                         (-1, -1) -> case makeMove board col (x,y) of
                           (Just newBoard') ->
                             return w {gameboard =  newBoard', turn = other col, oldworld = w}
                           (Nothing) -> return w {turn = other col, oldworld = w}
                         --pass
                         (_,_) -> return w {turn = other col, oldworld = w}
  where board = gameboard w
        col = turn w
        oldWorld = initWorld (args w)

-- | Evaluates a board for a given gametree
eval :: GameTree -> World -> Int
eval tree w = evaluate board col w
  where board = (game_board tree)
        col = game_turn tree

-- | Returns the best move for a single layer of the game tree
evalLayer :: GameTree -> World -> (Position, Int)
evalLayer tree w = (head bestPos)
  where scores = map (\(pos,gametree) -> (pos, eval (gametree) w)) (nextMoves tree)
        bestScore = maximum (map (snd) scores)
        bestPos = filter (\move -> (snd move) == bestScore) scores

