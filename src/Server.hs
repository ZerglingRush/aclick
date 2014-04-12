module Server where

import Control.Concurrent.STM
 ( TVar
  , STM
  , newTVar
  , readTVar
  , writeTVar
  , atomically
  )
import Control.Concurrent (forkFinally)
import Control.Monad (forever)
import Network (Socket, listenOn, PortID(..), accept)
import System.IO (Handle, hPutStrLn, hGetLine, hPutStr, hClose)
import Text.Printf (printf)

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
  m <- atomically $ newTVar database
  sock <- listenOn (PortNumber (fromIntegral port))
  _ <- printf "AClick initialized on port %d\n" port
  forever (listen sock m)
