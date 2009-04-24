module System.PersistentSystem (main) where

import Network.Socket.Server
import Network

import Control.Concurrent
import System.Environment
import System.IO

main :: IO ()
main = do
  socketFile : _ <- getArgs
  print =<< runThreadedServer handleProcess (return False) =<< listenOn (UnixSocket socketFile)

handleProcess :: AcceptanceInfo -> ThreadId -> IO ()
handleProcess (handle, _, _) _ = do
  contents <- hGetContents handle
  putStr contents
