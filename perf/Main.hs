import Control.Concurrent
import Network
import System.IO

main =sequence $ map (\i -> do
  print "herp"
  h <- connectTo "localhost" (PortNumber (fromIntegral 6666))
  derp <- hGetLine h
  print derp
  hPutStr h ("set " ++ (show i) ++ " " ++ (show i) ++ "\n")
  hClose h) [1..100]
