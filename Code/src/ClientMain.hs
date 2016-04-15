module ClientMain where

import Network.Socket
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Data.List.Split

data Msg = String


client :: String -> Int -> IO Handle
client host port = do
  chan <- newChan
  addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  setSocketOption sock KeepAlive 1
  connect sock (addrAddress serverAddr)
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle NoBuffering
  return (handle)

runConn :: Handle -> Chan Msg -> IO()
runConn handle chan = do
  z <- getLine
  hPutStrLn handle z
  runConn handle chan

sendAcrossNetwork :: Handle -> Int -> Int -> IO()
sendAcrossNetwork handle x y = do
  hPutStrLn handle $ show x ++ "," ++ show y

readAcrossNetwork :: Handle -> IO (Int, Int)
readAcrossNetwork handle = do
    message1 <- hGetLine handle
    let [x,y] = splitOn "," message1
    return (read x, read y)
