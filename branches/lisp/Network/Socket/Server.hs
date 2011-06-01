module Network.Socket.Server where

import Network
import IO

import Control.Concurrent

import Control.Monad.State.Lazy

import Control.Monad
import Control.Monad.Fix
import Data.List

type AcceptanceInfo = (Handle, HostName, PortNumber)

runServer :: (AcceptanceInfo -> IO a) -> IO Bool -> Socket -> IO [a]
runServer handleConnection done server = unfoldM done (accept server >>= handleConnection)

unfoldM :: Monad m => m Bool -> m a -> m [a]
unfoldM done action = loop where
    loop = do
      isDone <- done
      if isDone
        then
          return []
        else
          liftM2 (:) action loop

forkIOwID :: (ThreadId -> IO ()) -> IO ThreadId
forkIOwID = mfix . (forkIO .)

runThreadedServer :: (AcceptanceInfo -> ThreadId -> IO ()) -> IO Bool -> Socket -> IO [ThreadId]
runThreadedServer = runServer . (forkIOwID .)

runAccumulatingServer :: (AcceptanceInfo -> ThreadId -> [a]) -> (a -> IO ()) -> IO Bool -> Socket -> IO [ThreadId]
runAccumulatingServer f g = runThreadedServer $ (mapM_ g .) . f

stateToUnfolder :: State s (Maybe a) -> (s -> Maybe (a, s))
stateToUnfolder sc s = case runState sc s of
                         (Just x , s') -> Just (x, s')
                         (Nothing, _ ) -> Nothing

unfoldState :: State s (Maybe a) -> s -> [a]
unfoldState = unfoldr . stateToUnfolder

runStatefulAccumulatingServer :: (AcceptanceInfo -> ThreadId -> s) -> State s (Maybe a) -> (a -> IO ()) -> IO Bool -> Socket -> IO [ThreadId]
runStatefulAccumulatingServer f sc = runAccumulatingServer $ \ ai ti -> unfoldState sc $ f ai ti
