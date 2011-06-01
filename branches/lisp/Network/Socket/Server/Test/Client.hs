module Network.Socket.Server.Test.Client (main) where

import Network

import Control.Concurrent

import IO
import Random
import Control.Monad

main :: IO ()
main = do
  connections <- replicateM 100 $ connectTo "localhost" $ UnixSocket "test.sock"
  mapM_ (\ connection -> forkIO $ do hSetBuffering connection NoBuffering
                                     mapM_ (\ c -> randomRIO (0, 1000) >>= threadDelay >> hPutChar connection c) ['A'..'Z']) connections
