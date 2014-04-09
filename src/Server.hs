module Server where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Network
import System.IO
import Text.Printf
import qualified Data.Map as Map
import Data.Char (isDigit, toLower)

import Core

handleInput :: String -> TVar Database -> STM Value
handleInput input m = do
  table <- readTVar m
  let (result, newTable) = processCommands (words input) table
  writeTVar m newTable
  return result

handler :: Handle -> TVar Database -> IO ()
handler h m = do
  hPutStrLn h ("Aclick ver 0")
  input <- (hGetLine h)
  print input
  result <- atomically $ (handleInput input m)
  hPutStr h ((show result) ++ "\n")

listen :: Socket -> TVar Database -> IO ()
listen sock m = do
  (handle, _, _) <- accept sock
  _ <- forkFinally (handler handle m) (\_ -> hClose handle)
  return ()

port :: Int
port = 6666

server :: IO ()
server = do
  m <- atomically $ newTVar Map.empty
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "AClick initialized on port %d\n" port
  forever (listen sock m)
