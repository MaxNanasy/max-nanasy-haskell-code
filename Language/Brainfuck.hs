{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module Language.Brainfuck (runBrainfuck) where

import Prelude hiding (until)

import Control.Monad.State
import Control.Monad.Consumer
import Control.Monad.Writer

import Control.Applicative

import Language.Brainfuck.Class

import Data.Stream.Iterator

type Array = StreamIterator

class    (Eq e, Enum e, MonadState (Array e) m, MonadConsumer e m, MonadWriter (f e) m, Applicative f) => BC e f m
instance (Eq e, Enum e, MonadState (Array e) m, MonadConsumer e m, MonadWriter (f e) m, Applicative f) => BC e f m

newtype Brainfuck m = BF (MonadAsMonoid (m ()))
    deriving(Monoid)

modCell :: BC e f m => (e -> e) -> m ()
modCell = modify . applyToCurrent

setCell :: BC e f m => e -> m ()
setCell = modify . setCurrent

getCell :: BC e f m => m e
getCell = gets current

getsCell :: BC e f m => (e -> a) -> m a
getsCell f = getCell >>= return . f

cellIsZero :: BC e f m => m Bool
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

instance BC e f m => MonoidBF (Brainfuck m) where
    incCell                       = BF $ MaM $ modCell succ
    decCell                       = BF $ MaM $ modCell pred
    incPointer                    = BF $ MaM $ modify  next
    decPointer                    = BF $ MaM $ modify  prev
    inputCell                     = BF $ MaM $ consume >>= setCell
    outputCell                    = BF $ MaM $ getCell >>= tell . pure
    loopUntilZero (BF (MaM body)) = BF $ MaM $ until cellIsZero body

runBrainfuck :: (Enum e, Monoid (f e)) => Brainfuck (StateT (Array e) (WriterT (f e) (Consumer e))) -> Stream e -> f e
runBrainfuck (BF (MaM program)) = evalConsumer . execWriterT . evalStateT program $ finiteListToStreamIterator [zero]
