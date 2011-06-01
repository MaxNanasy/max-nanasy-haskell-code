{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module Language.Brainfuck (runBrainfuck) where

import Prelude hiding (until)

import Control.Monad.State
import Control.Monad.Consumer
import Control.Monad.Producer
import Control.Monad.Writer

import Control.Applicative

import Language.Brainfuck.Class

import Data.Stream.Iterator

type Array = StreamIterator

class    (Eq e, Enum e, MonadState (Array e) m, MonadConsumer e m, MonadProducer e m) => BC e m
instance (Eq e, Enum e, MonadState (Array e) m, MonadConsumer e m, MonadProducer e m) => BC e m

newtype Brainfuck m = BF (MonadAsMonoid (m ()))
    deriving(Monoid)

modCell :: BC e m => (e -> e) -> m ()
modCell = modify . applyToCurrent

setCell :: BC e m => e -> m ()
setCell = modify . setCurrent

getCell :: BC e m => m e
getCell = gets current

getsCell :: BC e m => (e -> a) -> m a
getsCell f = getCell >>= return . f

cellIsZero :: BC e m => m Bool
cellIsZero = getsCell (zero ==)

while :: Monad m => m Bool -> m a -> m ()
while test body = let loop = do
                        test' <- test
                        when test' $
                             body >> loop
                  in loop

until :: Monad m => m Bool -> m a -> m ()
until = while . liftM not

zero :: Enum a => a
zero = toEnum 0

instance BC e m => MonoidBF (Brainfuck m) where
    incCell                       = BF $ MaM $ modCell succ
    decCell                       = BF $ MaM $ modCell pred
    incPointer                    = BF $ MaM $ modify  next
    decPointer                    = BF $ MaM $ modify  prev
    inputCell                     = BF $ MaM $ consume >>= setCell
    outputCell                    = BF $ MaM $ getCell >>= produce
    loopUntilZero (BF (MaM body)) = BF $ MaM $ until cellIsZero body

runBrainfuck :: (Enum e, Monoid w) => Brainfuck (StateT (Array e) (PureProducerT w (Consumer e))) -> Stream e -> w
runBrainfuck (BF (MaM program)) = evalConsumer . execPureProducerT . evalStateT program $ finiteListToStreamIterator [zero]
