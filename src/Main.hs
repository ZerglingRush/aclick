module Main where

import Control.Concurrent.STM
import Control.Monad
import Network
import System.IO
import Text.Printf
import qualified Data.Map as Map
import Data.Char (isDigit, toLower)

import Server
import Core

main :: IO ()
main = server
