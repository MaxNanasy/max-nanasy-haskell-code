{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}

module Control.Concurrent.Actor where

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

actor = Actor

actionToResult :: Acting msg () -> Behavior msg -> Result msg
actionToResult (Acting (Writer (_, (communications, Last maybeNewBehavior)))) oldBehavior = Result communications (fromMaybe oldBehavior maybeNewBehavior)

become :: Behavior msg -> Acting msg ()
become = Acting . tell . curry id [] . Last . Just

send :: Actor msg' -> msg' -> Acting msg ()
send = ((Acting . tell . flip (curry id) (Last Nothing) . (:[])) .) . Communication

