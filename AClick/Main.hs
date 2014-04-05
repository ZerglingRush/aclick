module Main where

import Control.Concurrent
import Control.Monad
import Network
import System.IO
import Text.Printf

port :: Int
port = 6666

handler :: Handle -> IO ()
handler h = do
  hPutStr h "Go to Hell!!!!!!\n"
  input <- (hGetLine h)
  print input
  hPutStr h (input ++ "\n")

listen :: Socket -> IO ()
listen sock = do
  (handle, _, _) <- accept sock
  _ <- forkFinally (handler handle) (\_ -> hClose handle)
  return ()

main :: IO ()
main = do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "AClick initialized on port %d\n" port
  forever (listen sock)
