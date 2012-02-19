module Utility where

import Control.Concurrent(mergeIO)
import Control.Exception(bracket)
import Control.Monad(liftM2)

import Data.List(find)

import Directory(removeFile)

import Network

import System.IO(Handle, hGetContents)
import System.IO.Unsafe(unsafeInterleaveIO)

withListenOn :: PortID -> (Socket -> IO a) -> IO a
withListenOn portID = bracket (listenOn portID) sCleanup where
    sCleanup socket = do
      sClose socket
      case portID of
        UnixSocket fileName -> removeFile fileName
        _                   -> return ()

sMergeServerConnectionResults :: ((Handle, HostName, PortNumber) -> IO [a]) -> Socket -> IO [a]
sMergeServerConnectionResults f socket = unsafeInterleaveIO $ do
  connection <- accept socket
  x <- unsafeInterleaveIO $ f connection
  xs <- sMergeServerConnectionResults f socket
  mergeIO x xs

sGetServerConnections :: Socket -> IO [(Handle, HostName, PortNumber)]
sGetServerConnections = unsafeRepeatIO . accept

sGetServerContentses :: Socket -> IO [String]
sGetServerContentses socket = sGetServerConnections socket >>= unsafeMapIO (\ (handle, _, _) -> hGetContents handle)

readMaybe :: Read a => String -> Maybe a
readMaybe s = fmap fst $ find (\ (_, s') -> null s') $ reads s

repeatM :: Monad m => m a -> m [a]
repeatM = sequence . repeat

unsafeMapIO :: (a -> IO b) -> [a] -> IO [b]
unsafeMapIO f xs = unsafeInterleaveIO $ case xs of
  x : xs' -> liftM2 (:) (f x) (unsafeMapIO f xs')
  []      -> return []

unsafeRepeatIO :: IO a -> IO [a]
unsafeRepeatIO action = unsafeInterleaveIO $ liftM2 (:) action (unsafeRepeatIO action)
