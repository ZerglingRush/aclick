module Main where

import Control.Concurrent
import Control.Monad
import Network
import System.IO
import Text.Printf
import qualified Data.Map as Map
import Data.Char (isDigit, toLower)

import Command

main :: IO ()
main = do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "AClick initialized on port %d\n" port
  m <- newMVar Map.empty
  forever (listen sock m)
