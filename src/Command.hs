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
import Data.List (isPrefixOf)

data Command = Set | Get | Incr

data Value = IntValue Int | StringValue String | Nil

type Database = Map.Map String Value

instance Show Value where
  show (IntValue i)    = "(integer) " ++ show i
  show (StringValue s) = "(string) "  ++ show s
  show (Nil)           = "(nil)"

parseValue :: String -> Value
parseValue s
  -- Allows integers to be entered as strings by prefixing them with \s
  | "\\s" `isPrefixOf` s = StringValue $ drop 2 s
  | all isDigit s      = IntValue $ read s
  | otherwise          = StringValue s

parseCommand :: String -> Maybe Command
parseCommand s
  | map toLower s == "set"  = Just Set
  | map toLower s == "get"  = Just Get
  | map toLower s == "incr" = Just Incr
  | otherwise               = Nothing

getKey :: String -> Database -> (Database, Value)
getKey k m = (m, Map.findWithDefault Nil k m)

setKey :: String -> String -> Database -> (Database, Value)
setKey k v m = (Map.insert k value m, value)
  where value = parseValue v

incrKey :: String -> Database -> (Database, Value)
incrKey k m = (Map.insert k newVal m, newVal)
  where
    addOne (IntValue v) = IntValue (v + 1)
    newVal = addOne $ Map.findWithDefault Nil k m

processCommands :: [String] -> Database -> (Database, Value)
processCommands [] m = (m, Nil)
processCommands [x] m = (m, Nil)
processCommands (x:y:[]) m = case (parseCommand x) of
  Just Get  -> getKey y m
  Just Incr -> incrKey y m
  Nothing   -> (m, Nil)
processCommands (x:y:z:[]) m = case (parseCommand x) of
  Just Set -> setKey y z m
  Nothing  -> (m, Nil)

handleInput :: String -> TVar Database -> STM Value
handleInput input m = do
  table <- readTVar m
  let (newTable, result) = processCommands (words input) table
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
