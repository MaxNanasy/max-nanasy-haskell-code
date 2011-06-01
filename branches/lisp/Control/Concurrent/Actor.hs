{-# LANGUAGE ExistentialQuantification, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Control.Concurrent.Actor (Behavior, Acting, Actor, sendIO
                                , MonadActing, become, send, spawn
                                , spawnIO, spawnIOActorIO) where

import Control.Concurrent
import Control.Monad.Writer.Lazy
import Data.Maybe

type Behavior msg = msg -> Acting msg ()

newtype Acting  msg a = Acting  (WriterT (Last (Behavior  msg)) IO a)

instance Monad (Acting msg) where
    Acting x >>= f = Acting $ x >>= (\ y -> case f y of Acting w -> w)
    return = Acting . return

class MonadActing actor m | m -> actor where
    become :: (msg  -> m msg  ())         -> m msg ()
    send   :: actor msg'          -> msg' -> m msg ()
    spawn  :: (msg' -> m msg' ())         -> m msg (actor msg')

instance MonadActing Actor Acting where
    become = Acting . tell . Last . Just
    send = ((Acting . liftIO) .) . sendIO
    spawn = Acting . liftIO . spawnIO

newtype Actor msg = Actor { sendIO :: msg -> IO () }

spawnIO :: Behavior msg -> IO (Actor msg)
spawnIO originalBehavior = do
  mailbox <- newChan
  forkIO $ let loop behavior = do
                              action <- liftM behavior $ readChan mailbox
                              newBehavior <- execAction action behavior
                              loop newBehavior
           in loop originalBehavior
  return . Actor $ writeChan mailbox

spawnIOActorIO :: (msg -> IO ()) -> IO (Actor msg)
spawnIOActorIO a = do
  mailbox <- newChan
  forkIO . forever $ a =<< readChan mailbox
  return . Actor $ writeChan mailbox

execAction :: Acting msg () -> Behavior msg -> IO (Behavior msg)
execAction (Acting action) oldBehavior = do
  (_, Last maybeNewBehavior) <- runWriterT action
  return $ fromMaybe oldBehavior maybeNewBehavior

test :: IO (Actor ())
test = do
  printer <- spawnIOActorIO print
  let print :: Integer -> Acting msg ()
      print = send printer
      printerTriggererB () = do
         numberPrinterTriggerer <- spawn numberPrinterTriggererB
         send numberPrinterTriggerer 0
         let counterSenderB n () = do
                               send numberPrinterTriggerer n
                               become $ counterSenderB $ n + 1
         become $ counterSenderB 1
      numberPrinterTriggererB = print
  printerTriggerer <- spawnIO printerTriggererB
  return printerTriggerer
