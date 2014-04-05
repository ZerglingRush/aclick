module Main where

import Control.Concurrent
import Control.Monad
import Network
import System.IO
import Text.Printf
import qualified Data.Map as Map
import Data.Char (isDigit, toLower)

port :: Int
port = 6666

properUsage :: Value
properUsage = StringValue "Use it like this 'set key value' asshole"

data Command = Set | Get | Incr

data Value = IntValue Int | StringValue String
  deriving (Show)

parseValue :: String -> Value
parseValue s
  | all isDigit s = IntValue $ read s
  | otherwise     = StringValue s

valueToString :: Value -> String
valueToString (IntValue v) = "(integer) " ++ show v
valueToString (StringValue v) = show v

fromString :: String -> Maybe Command
fromString s
  | map toLower s == "set"  = Just Set
  | map toLower s == "get"  = Just Get
  | map toLower s == "incr" = Just Incr
  | otherwise               = Nothing

keyValue (k:v:_) m = (Map.insert k (parseValue v) m, StringValue "success")
getKey (k:_) m     = (m, m Map.! k)
incrValue (k:_) m  = (Map.insert k newVal m, newVal)
  where
    addOne (IntValue v) = Just (IntValue (v + 1))
    newVal = addOne (m Map.! k)

processWords
  :: [String]
  -> Map.Map String Value
  -> (Map.Map String Value, Value)
processWords [] m = (m, properUsage)
processWords (w: ws) m = case fromString w of
  Just Set  -> keyValue  ws m
  Just Get  -> getKey    ws m
  Just Incr -> incrValue ws m
  Nothing   -> (m, properUsage)

handler :: Handle -> MVar (Map.Map String Value) -> IO ()
handler h m = do
  hPutStr h ("Go to Hell!!!!!!\n")
  input <- (hGetLine h)
  r <- takeMVar m
  let (newM, t) = processWords (words input) r
  putMVar m newM
  hPutStr h ((valueToString t) ++ "\n")

listen :: Socket -> MVar (Map.Map String Value) -> IO ()
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
