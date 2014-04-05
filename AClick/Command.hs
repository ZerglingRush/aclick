module AClick.Command where

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
properUsage = Error 1 "Use it like this 'set key value' asshole"

data Command = Set | Get | Incr

data Value = IntValue Int | StringValue String | Error Int String

instance Show Value where
  show (IntValue i) = show i
  show (StringValue s) = show s
  show (Error i e) = show "(error)" ++ show i ++ show e

parseValue :: String -> Value
parseValue s
  | all isDigit s = IntValue $ read s
  | otherwise     = StringValue s

valueToString :: Value -> String
valueToString (IntValue v) = "(integer) " ++ show v
valueToString (StringValue v) = show v

fromString :: String -> Maybe Command
fromString s
  | map toLower s == "set" = Just Set
  | map toLower s == "get" = Just Get
  | otherwise              = Nothing

keyValue (k:v:_) m = (Map.insert k (parseValue v) m, StringValue "success")
getKey (k:_) m     = (m, m Map.! k)

processWords
  :: [String]
  -> Map.Map String Value
  -> (Map.Map String Value, Value)
processWords [] m = (m, properUsage)
processWords (w: ws) m = case fromString w of
  Just Set -> keyValue ws m
  Just Get -> getKey   ws m
  Nothing  -> (m, properUsage)

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
