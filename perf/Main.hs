import Control.Concurrent
import Network
import System.IO

setNum :: Integer -> IO ()
setNum i = (forkIO $ do
  h <- connectTo "localhost" (PortNumber (fromIntegral 6666))
  hPutStr h ("set " ++ (show i) ++ " " ++ (show i) ++ "\n")
  hClose h) >>= (\_ -> return ())

main :: IO [()]
main =  mapM setNum [1]
