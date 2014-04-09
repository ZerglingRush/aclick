module Main where

import Control.Concurrent
import Control.Monad
import Network
import System.IO

main = do
  putStrLn "aclick client"
  forever $ do
    h <- connectTo "localhost" (PortNumber (fromIntegral 6666))
    hGetLine h
    putStrLn "aclick> "
    getLine >>= hPutStrLn h
    hGetLine h >>= putStrLn
    hClose h
