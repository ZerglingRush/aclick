module Command where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Base
import Control.Monad
import Network
import System.IO
import Text.Printf
import qualified Data.Map as Map
import Data.Char (isDigit, toLower)

invalidKey :: Value
invalidKey = Error 2 "No key found"

data Command = Set | Get | Incr

data Value = IntValue Int | StringValue String | Error Int String

instance Show Value where
  show (IntValue i) = "(integer) " ++ show i
  show (StringValue s) = show s
  show (Error i e) = show "(error #)" ++ show i ++ ":" ++ show e

parseValue :: String -> Value
parseValue s
  | all isDigit s = IntValue $ read s
  | otherwise     = StringValue s

fromString :: String -> Maybe Command
fromString s
  | map toLower s == "set"  = Just Set
  | map toLower s == "get"  = Just Get
  | map toLower s == "incr" = Just Incr
  | otherwise               = Nothing

getKey :: String ->
          (Map.Map String Value) ->
          (Map.Map String Value, Value)
getKey k m = (m, Map.findWithDefault invalidKey k m)

setKey :: String ->
          String ->
          (Map.Map String Value) ->
          (Map.Map String Value, Value)
setKey k v m = (Map.insert k (parseValue v) m, StringValue "success")

incrKey :: String ->
           (Map.Map String Value) ->
           (Map.Map String Value, Value)
incrKey k m = (Map.insert k newVal m, newVal)
  where
    addOne (IntValue v) = IntValue (v + 1)
    newVal = addOne $ Map.findWithDefault invalidKey k m

invalidCommand :: Map.Map String Value -> (Map.Map String Value, Value)
invalidCommand m = (m, Error 1 "Use it like this 'set key value' asshole")

processCommands
  :: [String]
  -> Map.Map String Value
  -> (Map.Map String Value, Value)
processCommands [] m = invalidCommand m
processCommands [x] m = invalidCommand m
processCommands (x:y:[]) m = case (fromString x) of
  Just Get  -> getKey y m
  Just Incr -> incrKey y m
  Nothing   -> invalidCommand m
processCommands (x:y:z:[]) m = case (fromString x) of
  Just Set -> setKey y z m
  Nothing  -> invalidCommand m

handleInput :: String -> TVar (Map.Map String Value)
               -> STM Value
handleInput input m = do
  table <- readTVar m
  let (newTable, result) = processCommands (words input) table
  writeTVar m newTable
  return result

handler :: Handle -> TVar (Map.Map String Value) -> IO ()
handler h m = do
  hPutStr h ("Go to Hell!!!!!!\n")
  input <- (hGetLine h)
  print input
  result <- atomically $ (handleInput input m)
  hPutStr h ((valueToString result) ++ "\n")

listen :: Socket -> TVar (Map.Map String Value) -> IO ()
listen sock m = do
  (handle, _, _) <- accept sock
  _ <- forkFinally (handler handle m) (\_ -> hClose handle)
  return ()
