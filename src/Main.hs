module Main where

import Control.Concurrent.STM
import Control.Monad
import Network
import System.IO
import Text.Printf
import qualified Data.Map as Map
import Data.Char (isDigit, toLower)

import Command

port :: Int
port = 6666

main :: IO ()
main = do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "AClick initialized on port %d\n" port
  m <- atomically $ newTVar Map.empty
  forever (listen sock m)
