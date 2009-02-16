{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}

module Control.Concurrent.Actor where

import Control.Monad.Writer.Lazy

import Data.Maybe

newtype Behavior msg = Behavior (msg -> Result msg)
data Result msg = Result [Communication] (Behavior msg)
                | New (Behavior msg) (Actor msg -> Result msg)
data Communication = forall msg. Communication (Actor msg) msg
newtype Actor msg = Actor (msg -> IO ())

newtype Acting msg a = Acting (Writer ([Communication], Last (Behavior msg)) a) deriving (Monad)

actingToResult :: Acting msg () -> Behavior msg -> Result msg
actingToResult (Acting w) oldBehavior = case execWriter w of (communications, newBehavior) -> Result communications (fromMaybe oldBehavior (getLast newBehavior))
