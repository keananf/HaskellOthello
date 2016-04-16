module AI where

import Board
import Game
import ClientMain
import System.IO


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

-- | This function calls detectMoves with a function that generates
-- | a list of all positions on the board. Every position gets checked to
-- | see if it is a valid move based on the rule set (either a piece is flipped or
-- | reversi is on), and these legal moves are then returned so the AI can analyse them.
genAllMoves :: Board -> Col -> [Position]
genAllMoves board col = detectMoves board col (allPositions board)


-- | The bad AI works by merely looking one layer down in the gametree.
-- | It calls a function evalLayer, which calls eval on each child gametree.
-- | The one that results in the highest lead for that turn is chosen.
getBestMoveBadAI :: GameTree -> World -> (Position, GameTree)
getBestMoveBadAI tree w = head bestMove
  where bestScore = evalLayer tree w --tuple of pos and score associated with pos
        bestMove = filter (\move -> (fst move) == (fst bestScore)) (nextMoves tree)

-- Get the best next move from a (possibly infinite) game tree. This
-- traverses the game tree up to a certain depth, and picks the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
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

-- | Update the world state every tenth of a second. This is used
-- | to check for moves from the AI, as well as from the network and updates
-- | the world accordingly. To do this, it checks if the current turn is that of
-- | the network player or of the AI. If so, the functions to receive a move or
-- | make one are called, and if not, then the time field in the world is decremented.
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

                | not (network w) = return w {time = (time w) - t}
                | otherwise = return w--player v player
                where aiColour = aiCol w
                      col = turn w
                      hasAI = ai w
                      state = gameState w
                      board = gameboard w
                      tree =(buildTree genAllMoves board col) --get entire gametree
                      (pos,newTree) = chooseAI w tree --get best move from gametree
                      newBoard = (game_board newTree) --get new board from the move associated with this tree

-- | This function looks at the difficulty field in the world passed into it
-- | and then calls the appropriate move function for either the easy or medium
-- | AI.
chooseAI :: World -> GameTree-> (Position, GameTree)
chooseAI world tree | (difficulty world) == 1 = getBestMoveBadAI tree world --easy
                    | otherwise = getBestMoveGoodAI 3 tree world --medium

-- | This function attempts to read a move from the pipe associated
-- | with the handle and connection. If so, it is returned.
readNetwork :: Handle -> IO (Int, Int)
readNetwork hand = do
  (x,y) <- readAcrossNetwork hand
  return (x,y)

-- | Processes move received from network. It checks to see if the move
-- | represents a disconnection or a pass, and if not, then makes the move.
moveFromNetwork :: World -> IO World
moveFromNetwork w = do hand <- (handle w)
                       (x,y) <- readNetwork hand
                       case (x,y) of
                         --disconnection
                         (-3, -3) -> return w {turn = col,
                                               ai = True, aiCol = col, network = False}
                         --pass
                         (-1, -1) -> return w {turn = other col, oldworld = w}
                         --not a pass
                         (_,_) -> case makeMove board col (x,y) of
                           (Just newBoard') ->
                             return w {gameboard =  newBoard', turn = other col, oldworld = w}
                           (Nothing) -> return w {turn = other col, oldworld = w}
  where board = gameboard w
        col = turn w

-- | Evaluates a board for a given gametreei.
-- | This extracts the board from the given gametree and calls
-- | the evaluate function in Board.hs
eval :: GameTree -> World -> Int
eval tree w = evaluate board col w
  where board = (game_board tree)
        col = game_turn tree

-- | Returns the best move for a single layer of the game tree
-- | This is used in both evalNested for the good AI and as the means
-- | of making a move for the bad AI. It calls eval on each gametree,
-- | and finds the maximum result the came from the eval function.
evalLayer :: GameTree -> World -> (Position, Int)
evalLayer tree w = (head bestPos)
  where scores = map (\(pos,gametree) -> (pos, eval (gametree) w)) (nextMoves tree)
        bestScore = maximum (map (snd) scores)
        bestPos = filter (\move -> (snd move) == bestScore) scores

