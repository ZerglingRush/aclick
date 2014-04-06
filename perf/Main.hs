import Control.Concurrent
import Control.Monad
import Network
import System.IO

setNum :: Integer -> IO ()
setNum i = do
  h <- connectTo "localhost" (PortNumber (fromIntegral 6666))
  _ <- hGetLine h
  hPutStr h ("set " ++ (show i) ++ " " ++ (show i) ++ "\n")
  hClose h

setNumThread :: Integer -> IO ()
setNumThread i = void $ (forkIO $ setNum i)

main =sequence $ map setNumThread [1..10000]
