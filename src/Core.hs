module Core where

import qualified Data.Map as Map (Map, findWithDefault, insert)
import Data.List (isPrefixOf)
import Data.Char (isDigit, toLower)

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
  | all isDigit s        = IntValue $ read s
  | otherwise            = StringValue s

parseCommand :: String -> Maybe Command
parseCommand s
  | map toLower s == "set"  = Just Set
  | map toLower s == "get"  = Just Get
  | map toLower s == "incr" = Just Incr
  | otherwise               = Nothing

getKey :: String -> Database -> (Value, Database)
getKey k m = (Map.findWithDefault Nil k m, m)

setKey :: String -> String -> Database -> (Value, Database)
setKey k v m = (value, Map.insert k value m)
  where value = parseValue v

incrKey :: String -> Database -> (Value, Database)
incrKey k m = (newVal, Map.insert k newVal m)
  where
    addOne (IntValue v) = IntValue (v + 1)
    newVal = addOne $ Map.findWithDefault Nil k m

processCommands :: [String] -> Database -> (Value, Database)
processCommands [] m = (Nil, m)
processCommands [_] m = (Nil, m)
processCommands (x:y:[]) m = case (parseCommand x) of
  Just Get  -> getKey y m
  Just Incr -> incrKey y m
  Nothing   -> (Nil, m)
processCommands (x:y:z:[]) m = case (parseCommand x) of
  Just Set -> setKey y z m
  Nothing  -> (Nil, m)
