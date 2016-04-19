module Game where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Data.Maybe
import Data.List
import System.IO
import System.IO.Unsafe
import ClientMain
import Text.Regex.Posix
import Debug.Trace

data Col = Black | White
  deriving (Show, Eq, Read)

type Position = (Int, Int)
type Piece = (Position, Col)


data GameState = Menu | Paused | GameOver | Playing
  deriving Eq
-- A Board is a record containing the board size (a board is a square grid, n *
-- n), the number of consecutive passes, and a list of pairs of position and
-- the colour at that position.

data Board = Board { size :: Int,
                     passes :: Int,
                     reversi :: Bool,
                     pieces :: [Piece]
                   }

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (infomation for the AI, for example, such as where the
-- most recent moves were).
data World = World { gameboard :: Board,
                     gameState :: GameState,
                     oldworld :: World, --allows for undo
                     hints :: Bool, --if hints are on or not
                     ai :: Bool, --if ai is on
                     difficulty :: Int, --difficulty of ai
                     network :: Bool, --if network is on
                     host :: String,
                     handle :: IO Handle, --handle representing network connection
                     aiCol :: Col, --ai colour
                     userCol :: Col, --user colour
                     args :: [String], --all args passed in
                     time :: Float, --time remaining for this turn
                     turn :: Col,
                     background :: Picture,
                     tile :: Picture}

-- | Creates a new board of size 8. If reversi mode is active, then
-- | the board starts with no pieces
initBoard :: Bool -> Board
initBoard reversi | reversi== False = Board 8 0 reversi [((3,4), Black), ((4,4), White),
                                                         ((3,3), White), ((4,3), Black)]
                  | otherwise = Board 8 0 reversi []

-- | Creates a new world with all the different settings instantiated.
-- | A default old world is added, including which colour the user and ai are,
-- | as well as which options are active, such as ai difficulty and hints.
-- | The list of command line args are stored as well so that if the user wishes
-- | to play another game, they don't have to quit out and restart with the same options.
initWorld :: [String] -> World
initWorld args = (World board Menu oldWorld hints ai difficulty network host
                  (getHandle host) aiCol userCol args 10.0 Black background tile)
  where board = initBoard (isReversi args network)
        oldWorld = initWorld args
        hints = hasHints args
        ai = hasAI args
        network = hasNetwork args
        filePath = "/saveFile.txt"
        difficulty = aiDifficulty args
        userCol = userColour args
        aiCol = opp userCol
        host = hostName args
        background = loadBackground args
        tile = loadTile args

-- | load the correct background (wood default) given command line arguments if any
loadBackground :: [String] -> Picture
loadBackground args | length args >= 1 && any (isInfixOf "background=") args
                      = loadImage ("../textures/background-" ++ name ++ ".png")
                    | otherwise = loadImage "../textures/background-wood.png"
                    where validargs = filter (isInfixOf "background=") args
                          names = (validargs !! 0 =~ "background=([A-Za-z]*)")
                          name = names !! 0 !! 1

-- | load the correct tile image (white default) given command line arguments if any
loadTile :: [String] -> Picture
loadTile args | length args >= 1 && any (== "tile=black") args
               = loadImage ("../textures/tile-black.png")
             | otherwise = loadImage "../textures/tile-white.png"

-- | load an image from loadJuicyPNG, return a Blank if the Maybe picture is Nothing
loadImage :: String -> Picture
loadImage bgname = unsafePerformIO $ do
                                         img <- loadJuicyPNG bgname
                                         return (
                                                case img of
                                                    Just pic -> pic
                                                    Nothing  -> Blank)

-- | Attempts to connect to the remote server to play a game over the network.
getHandle :: String -> IO Handle
getHandle host = do
          handle <- client host 4024
          return handle

-- | Checks to see if the user passed in an argument detailing if they wish
-- | to play as white. If not, this function defaults to black.
userColour :: [String] -> Col
userColour arguments | length arguments >= 1 && any (=="user=white") arguments = White
                     | otherwise = Black

-- | Checks to see if the user passed in an argument detailing if they wish
-- | to play against the medium ai (2). If not, this function defaults to easy (1).
aiDifficulty :: [String] -> Int
aiDifficulty arguments | length arguments >= 1 && any (=="medium") arguments = 2
                       | otherwise = 1

-- | Checks to see if the user passed in an argument detailing if they wish to
-- | play againt an AI. Defaults to False.
hasAI :: [String] -> Bool
hasAI arguments | length arguments >= 1 && any (=="ai") arguments = True
                | otherwise = False

-- | Checks to see if the user passed in an argument detailing if they wish to
-- | play over the network. Defaults to False. If active, this takes precedence
-- | over ai, to avoid any confusion in the program as to where to look for moves from.
hasNetwork :: [String] -> Bool
hasNetwork arguments | length arguments >= 1 && any (=="network") arguments = True
                | otherwise = False

-- | Checks to see if the user passed in an IP address to connect to.
-- | This is ignored if the network option is inactive, and it defaults to localhost.
hostName :: [String] -> String
hostName arguments | length arguments >= 1 && length (findIPs arguments) > 0 = (findIPs arguments) !! 0
                   | otherwise = "localhost"

-- | Uses a regular expression to check if the user entered in an IP address.
findIPs :: [String] -> [String]
findIPs arguments = filter (=~ "(([0-9]{1,3}.){3}[0-9]{1,3})") arguments

-- | Check to see if the user wishes to play the game with the
-- | reversi rule set, that is, that the first two moves by each colour
-- | don't need to result in a flip.
isReversi :: [String] -> Bool -> Bool
isReversi arguments net| length arguments >= 1 && net == False && any (== "reversi") arguments = True
                       | otherwise = False

-- | Checks to see if the user requested to display hints. Defaults to
-- | False, and can also be set from in-game.
hasHints :: [String] -> Bool
hasHints arguments | length arguments >= 1 && any (=="hints") arguments = True
                   | otherwise = False

opp :: Col -> Col
opp Black = White
opp White = Black


-----------------------------------------------------------------
-- Check the current score
-- Returns a pair of the number of black pieces, and the number of
-- white pieces
checkScore :: Board -> (Int, Int)
checkScore board = countPieces (0,0) (colors)
                   where colors = map (snd) (pieces board)

-- | counts the numbers of each type of piece and returns a tuple
countPieces :: (Int, Int) -> [Col] -> (Int, Int)
countPieces (x,y) [] = (x,y)
countPieces (x,y) (z:zs) | Black == z = countPieces (x+1,y) zs
                         | otherwise = countPieces (x,y+1) zs
----------------------------------------------------------------

-- Return true if the game is complete (that is, either the board is
-- full or there have been two consecutive passes)
gameOver :: Board -> Bool
gameOver board | (passes board) == 2 = True
               | length (pieces board) == (size board) * (size board) = True
               | otherwise = False

-- | Attempts to undo a move given the particular type of game
-- | If the ai is active, an undo will take you back to your previous move
-- | so that it doesn't get caught in a loop of the AI making the same move
-- | again and again. Otherwise, it undoes one move for player to player.
-- | Given that there is a menu button, the undo won't work if the oldworld's
-- | game state is the Menu.
undoMove :: World -> IO World
undoMove world | ((gameState (oldworld world)) == Menu && not (ai world)) ||
                 ((gameState (oldworld (oldworld world)) == Menu) && (ai world)) = return world

               | not (ai world) = (return (oldworld world){time=10.0}) --undoes move for player v player
               | otherwise = return ((oldworld (oldworld world)){time=10.0}) --undoes for ai v player
               where networkHandle = handle world
                     userColour = userCol world
                     col = turn world
