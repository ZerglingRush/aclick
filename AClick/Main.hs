module Main where

import Control.Concurrent
import Control.Monad
import Network
import System.IO
import Text.Printf
import qualified Data.Map as Map

port :: Int
port = 6666

properUsage :: String
properUsage = "Call 'set key value' or 'get key'"

-- Look at MVars as a way of storing things
-- and accessing them from different threads
-- Marlow has good examples
keyValue (k:v:_) m = (Map.insert k v m, "success")
getKey (k:_) m     = (m, m Map.! k)

processWords
    :: [String]
    -> Map.Map String String
    -> (Map.Map String String, String)
processWords [] m = (m, properUsage)
processWords (w: ws) m
    | w == "set" = keyValue ws m
    | w == "get" = getKey ws m
    | otherwise  = (m, properUsage)

handler :: Handle -> MVar (Map.Map String String) -> IO ()
handler h m = do
  hPutStr h ("Go to Hell!!!!!!\n")
  input <- (hGetLine h)
  r <- takeMVar m
  let (newM, t) = processWords (words input) r
  putMVar m newM
  hPutStr h (t ++ "\n")

listen :: Socket -> MVar (Map.Map String String) -> IO ()
listen sock m = do
  (handle, _, _) <- accept sock
  _ <- forkFinally (handler handle m) (\_ -> hClose handle)
  return ()

main :: IO ()
main = do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "AClick initialized on port %d\n" port
  m <- newMVar Map.empty
  forever (listen sock m)
