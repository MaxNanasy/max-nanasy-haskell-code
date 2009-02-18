{-# LANGUAGE ExistentialQuantification #-}

module Control.Concurrent.Actor (Behavior, Result, Acting, Actor, sendIO, become, send, newActor, actor) where

import Control.Concurrent

import Control.Monad.Writer.Lazy

import Data.Maybe

type Behavior msg = msg -> Acting msg ()

data Result msg = Result [Communication] (Behavior msg)
                | New (Behavior msg) (Actor msg -> Result msg)
data Communication = forall msg. Communication (Actor msg) msg

newtype Acting msg a = Acting (Writer ([Communication], Last (Behavior msg)) a) -- deriving (Monad)

instance Monad (Acting msg) where
    Acting x >>= f = Acting $ x >>= (\ y -> case f y of Acting w -> w)
    return = Acting . return

newtype Actor msg = Actor { sendIO :: msg -> IO () }

actionToResult :: Acting msg () -> Behavior msg -> Result msg
actionToResult (Acting (Writer (_, (communications, Last maybeNewBehavior)))) oldBehavior = Result communications (fromMaybe oldBehavior maybeNewBehavior)

become :: Behavior msg -> Acting msg ()
become = Acting . tell . curry id [] . Last . Just

send :: Actor msg' -> msg' -> Acting msg ()
send = ((Acting . tell . flip (curry id) (Last Nothing) . (:[])) .) . Communication

newActor :: Behavior msg -> IO (Actor msg)
newActor b = do
  mailbox <- newChan
  forkIO $ let loop b = do
                 action <- liftM b $ readChan mailbox
                 let Result communications b' = actionToResult action b
                 mapM_ (\ (Communication a m) -> sendIO a m) communications
                 loop b'
           in loop b
  return . Actor $ writeChan mailbox

actor :: (msg -> IO ()) -> IO (Actor msg)
actor a = do
  mailbox <- newChan
  forkIO . forever $ a =<< readChan mailbox
  return . Actor $ writeChan mailbox
