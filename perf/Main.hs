import Control.Concurrent
import Network
import System.IO

setNum :: Integer -> IO ()
setNum i = do
  h <- connectTo "localhost" (PortNumber (fromIntegral 6666))
  hPutStr h ("set " ++ (show i) ++ " " ++ (show i) ++ "\n")
  hClose h

main = setNum 1
