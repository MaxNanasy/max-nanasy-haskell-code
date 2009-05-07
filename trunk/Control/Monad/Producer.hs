{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GeneralizedNewtypeDeriving, UndecidableInstances #-}

module Control.Monad.Producer where

import Control.Monad.Writer.Lazy

import Control.Applicative

class Monad m => MonadProducer e m | m -> e where
    produce :: e -> m ()
instance (MonadTrans t, Monad (t m), MonadProducer e m) => MonadProducer e (t m) where
    produce = lift . produce

newtype ProducingWriterT m a = ProducingWriterT { runProducingWriterT :: m a } deriving Monad
instance MonadTrans ProducingWriterT where
    lift = ProducingWriterT
instance MonadWriter e m => MonadProducer e (ProducingWriterT m)  where
    produce = ProducingWriterT . tell

newtype PurifyingProducerT m a = PurifyingProducerT { runPurifyingProducerT :: m a } deriving Monad
instance MonadTrans PurifyingProducerT where
    lift = PurifyingProducerT
instance (MonadProducer (f e) m, Applicative f) => MonadProducer e (PurifyingProducerT m) where
    produce = PurifyingProducerT . produce . pure

newtype PureProducerT w m a = PureProducerT (PurifyingProducerT (ProducingWriterT (WriterT w m)) a) deriving Monad
execPureProducerT :: Monad m => PureProducerT w m a -> m w
execPureProducerT (PureProducerT x) = execWriterT . runProducingWriterT $ runPurifyingProducerT x
instance MonadProducer (f e) m => MonadProducer e (PureProducerT (f e) m) where
    produce = PureProducerT . produce
