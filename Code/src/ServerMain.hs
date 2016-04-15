module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import Network.Socket
import System.IO
import Control.Monad

main :: IO()
main = do
  setUpPort

type Msg = (Int, String)    --Message to be received / sent over network, int = protocol ID, String = message

setUpPort :: IO()
setUpPort = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock(SockAddrInet 4024 iNADDR_ANY)
  listen sock 2
  setUpGameConnections sock 0

setUpGameConnections :: Socket -> Int -> IO()
setUpGameConnections sock gameNum = do
  (sock1, _) <- accept sock   --Player 1
  handle1 <- socketToHandle sock1 ReadWriteMode
  hSetBuffering handle1 NoBuffering
  (sock2, _) <- accept sock   --Player 2
  handle2 <- socketToHandle sock2 ReadWriteMode
  hSetBuffering handle2 NoBuffering
  forkIO (setUpGame handle1 handle2 $ gameNum + 1)
  setUpGameConnections sock $ gameNum + 1

setUpGame :: Handle -> Handle -> Int -> IO()
setUpGame handle1 handle2 gameNum= do
  chanInput <- newChan
  chanOutputP1 <- newChan
  chanOutputP2 <- newChan
  dupInputChan1 <- dupChan chanInput
  dupInputChan2 <- dupChan chanInput
  dupOutputChanP1 <- dupChan chanOutputP1
  dupOutputChanP2 <- dupChan chanOutputP2

  forkIO (receiveMessageLoop handle1 gameNum 1 dupInputChan1)
  forkIO (receiveMessageLoop handle2 gameNum 2 dupInputChan2)
  forkIO (sendMessageLoop handle1 dupOutputChanP1)
  forkIO (sendMessageLoop handle2 dupOutputChanP2)

  gameLoop 2 chanInput chanOutputP1 chanOutputP2

gameLoop :: Int -> Chan Msg -> Chan Msg -> Chan Msg -> IO()
gameLoop idToPlay chanInput chanOutputP1 chanOutputP2 = do
  (playerID, message) <- waitForCorrectInputChan idToPlay chanInput

  --DO SOME COMPUTATION ON USER INPUT

  if idToPlay == 1
    then do
      writeChan chanOutputP2 (playerID, message)
      gameLoop 2 chanInput chanOutputP1 chanOutputP2
    else do
      writeChan chanOutputP1 (playerID, message)
      gameLoop 1 chanInput chanOutputP1 chanOutputP2


waitForCorrectInputChan :: Int -> Chan Msg -> IO(Msg)
waitForCorrectInputChan idToPlay chanInput = do
  (playerID, msg) <- readChan chanInput
  if msg == "-3,-3"
    then
      if playerID == 1
        then
          return (2, msg)
        else
          return (1, msg)
    else
      if playerID == idToPlay
        then return (playerID, msg)
        else do print $ "Message declined : from [" ++ show playerID ++ "] -> " ++ msg ++ " ID TO PLAY : " ++ show idToPlay
                waitForCorrectInputChan idToPlay chanInput

receiveMessageLoop :: Handle -> Int -> Int -> Chan Msg -> IO() --Put onto input chan
receiveMessageLoop handle gameNum playerID chanInput = do
  end <- hIsEOF handle
  if end
    then do
      writeChan chanInput (playerID, "-3,-3")
      print $ "GAME " ++ show gameNum ++ ", PLAYER " ++ show playerID ++ " DISCONNECTED"
    else do
      message <- hGetLine handle
      print $ "Game: " ++ show gameNum ++ " - "  ++ show playerID ++ " => "++ message
      writeChan chanInput (playerID, message)
      receiveMessageLoop handle gameNum playerID chanInput

sendMessageLoop :: Handle -> Chan Msg -> IO()
sendMessageLoop handle chanOutput = do
  (playerID, message) <- readChan chanOutput
  if message == "-3,-3"
    then do
      sendMessage handle message
      print "Game finished, one player disconnected, closing remaining handle"
      hClose handle
    else do
      sendMessage handle message
      sendMessageLoop handle chanOutput

sendMessage :: Handle -> String -> IO() --Read from output chan
sendMessage handle msg = do
  hPutStrLn handle msg
