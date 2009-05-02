{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Consumer where

import Data.Stream

import Control.Monad.State.Lazy

newtype Consumer a = Consumer (State (Stream a)) deriving (Monad)


