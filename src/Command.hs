module Command where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Base
import Control.Monad
import Control.Monad.State
import Network
import System.IO
import Text.Printf
import qualified Data.Map as Map
import Data.Char (isDigit, toLower)

invalidKey :: Value a
invalidKey = Error 2 "No key found"

data Command = Set | Get | Incr

data Value a where
  IntValue :: Int -> Value a
  StringValue :: String -> Value a
  Error :: Int -> String -> Value a

instance Show (Value a) where
  show (IntValue i) = "(integer) " ++ show i
  show (StringValue s) = show s
  show (Error i e) = show "(error #)" ++ show i ++ ":" ++ show e

parseValue :: String -> Value a
parseValue s
  | all isDigit s = IntValue $ read s
  | otherwise     = StringValue s

parseCommand :: String -> Maybe Command
parseCommand s
  | map toLower s == "set"  = Just Set
  | map toLower s == "get"  = Just Get
  | map toLower s == "incr" = Just Incr
  | otherwise               = Nothing

getKey :: String ->
          (Map.Map String (Value a)) ->
          (Map.Map String (Value a), Value a)
getKey k m = (m, Map.findWithDefault invalidKey k m)

setKey :: String ->
          String ->
          (Map.Map String (Value a)) ->
          (Map.Map String (Value a), Value a)
setKey k v m = (Map.insert k (parseValue v) m, StringValue "success")

incrKey :: String ->
           (Map.Map String (Value a)) ->
           (Map.Map String (Value a), Value a)
incrKey k m = (Map.insert k newVal m, newVal)
  where
    addOne (IntValue v) = IntValue (v + 1)
    newVal = addOne $ Map.findWithDefault invalidKey k m

invalidCommand :: Map.Map String (Value a) -> (Map.Map String (Value a), Value a)
invalidCommand m = (m, Error 1 "Use it like this 'set key value' asshole")

processCommands
  :: [String]
  -> Map.Map String (Value a)
  -> (Map.Map String (Value a), Value a)
processCommands [] m = invalidCommand m
processCommands [x] m = invalidCommand m
processCommands (x:y:[]) m = case (parseCommand x) of
  Just Get  -> getKey y m
  Just Incr -> incrKey y m
  Nothing   -> invalidCommand m
processCommands (x:y:z:[]) m = case (parseCommand x) of
  Just Set -> setKey y z m
  Nothing  -> invalidCommand m

handleInput :: String -> TVar (Map.Map String (Value a))
               -> STM (Value a)
handleInput input m = do
  table <- readTVar m
  let (newTable, result) = processCommands (words input) table
  writeTVar m newTable
  return result

handler :: Handle -> TVar (Map.Map String (Value a)) -> IO ()
handler h m = do
  hPutStr h ("Go to Hell!!!!!!\n")
  input <- (hGetLine h)
  print input
  result <- atomically $ (handleInput input m)
  hPutStr h ((show result) ++ "\n")

listen :: Socket -> TVar (Map.Map String (Value a)) -> IO ()
listen sock m = do
  (handle, _, _) <- accept sock
  _ <- forkFinally (handler handle m) (\_ -> hClose handle)
  return ()
