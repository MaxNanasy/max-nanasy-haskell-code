module Network.Socket.Server.Test.Server (main) where

import Network.Socket.Server

import Network

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import IO
import Control.Monad
import Control.Monad.Fix
import Directory
--import Data.Function

main :: IO ()
main = do
  channel <- atomically newTChan
  doesFileExist "test.sock" >>= flip when
       (removeFile "test.sock")
  server  <- listenOn $ UnixSocket "test.sock"
  forkIO $ runAccumulatingServer f (atomically . writeTChan channel) (return False) server >> return ()
  putStrLn "Server running."
  hSetBuffering stdout NoBuffering
  mapChannel_ putChar channel
--  channelToInfiniteList channel >>= putStrLn

channelToFiniteList channel = unfoldM (atomically $ isEmptyTChan channel) (atomically $ readTChan channel)
channelToInfiniteList channel = fix $ liftM2 (:) (putStrLn "Reading..." >> (atomically $ readTChan channel))

mapChannel_ :: (a -> IO b) -> TChan a -> IO c
mapChannel_ f channel = fix ((atomically $ readTChan channel) >>= f >>) where fix f = f $ fix f

f ""       = Nothing
f (c : cs) = Just (c, cs)
