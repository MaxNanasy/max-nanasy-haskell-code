{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Control.Monad.Consumer (consume, Consumer, MonadConsumer, evalConsumer, execConsumer) where

import Data.Stream

import Control.Monad.State.Lazy

newtype Consumer e a = Consumer (State (Stream e) a) deriving (Monad)

evalConsumer (Consumer s) = evalState s
execConsumer (Consumer s) = execState s

class Monad m => MonadConsumer e m where
    consume :: m e

instance MonadConsumer e (Consumer e) where
    consume = Consumer $ do
                Stream x xs <- get
                put xs
                return x

instance (MonadTrans t, Monad (t m), MonadConsumer e m) => MonadConsumer e (t m) where
    consume = lift consume
