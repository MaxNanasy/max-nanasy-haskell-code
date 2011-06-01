{-# LANGUAGE ExistentialQuantification, Rank2Types, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, FlexibleContexts, FunctionalDependencies, EmptyDataDecls #-}
{-# OPTIONS_GHC -Wall #-}

module Language.BlooP(loop, cell, (<--), fromInteger, (>>), ($), (+), (*), Function, Predicate, yes, no, output) where

import Number.Peano

import Control.Monad.ST.Lazy
import Data.STRef.Lazy

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader

import Control.Monad.Cont

import Data.List

import           Prelude hiding((*), (+), fromInteger)
import qualified Prelude

default(T)

yes, no :: BlooP s o Bool
yes = return True
no  = return False

type Function  = forall s. BlooP s T    T -> BlooP s T    ()
type Predicate = forall s. BlooP s Bool T -> BlooP s Bool ()
type BlooP s o = ReaderT (RefStream s T) (StateT o (ErrorT BlooPError (ST s)))
type Cell s = STRef s (Maybe T)
type BlooPError = String

data RefStream s a = Cons (STRef s (Maybe a)) (ST s (RefStream s a))

data Output
output :: BlooP s o Output
output = return undefined

runBlooP :: BlooPType o => (forall s. BlooP s o ()) -> Either BlooPError o
runBlooP b = runST $ runErrorT $ execStateT (do
               cells <- lift $ lift refStream
               runReaderT b cells)
             outputDefault

class BlooPType a where
    outputDefault :: a

infix 0 <--

(<--) :: (Settable s o a c, Valued s o b c) => BlooP s o a -> BlooP s o b -> BlooP s o ()
xRef' <-- x' = do
  xRef <- xRef'
  x    <- x'
  valX <- val x
  set xRef valX

class Valued s o v a where
    val :: v -> BlooP s o a

instance Valued s o (Cell s) T where
    val xRef = do
      x <- lift . lift . lift $ readSTRef xRef
      maybe (throwError "No value in cell") return x

instance Valued s o a a where
    val = return

instance Valued s o Output o where
    val = const get

class Settable s o p a | o p -> a where
    set :: p -> a -> BlooP s o ()

instance Settable s o (Cell s) T where
    set xRef x = lift . lift . lift $ writeSTRef xRef $ Just x

instance Settable s o Output o where
    set = const put

binOp :: (Valued s o a' a, Valued s o b' b) => (a -> b -> c) -> BlooP s o a' -> BlooP s o b' -> BlooP s o c
binOp op x' y' = do
  x    <- x'
  y    <- y'
  valX <- val x
  valY <- val y
  return $ op valX valY

(*), (+) :: (Valued s o a T, Valued s o b T) => BlooP s o a -> BlooP s o b -> BlooP s o T
(*) = binOp (Prelude.*)
(+) = binOp (Prelude.+)

fromInteger :: Integer -> BlooP s o T
fromInteger = return . Prelude.fromInteger

instance BlooPType Bool where outputDefault = False
instance BlooPType T    where outputDefault = Zero

cell :: BlooP s o T -> BlooP s o (Cell s)
cell n' = do
  n <- n'
  ref <- asks $ index n
  lift . lift $ lift ref

index :: T -> RefStream s a -> ST s (STRef s (Maybe a))
index  Zero    (Cons r _ ) = return r
index (Succ n) (Cons _ rs) = do
  rs'  <- rs
  index n rs'

refStream :: ST s (RefStream s a)
refStream = do
  ref  <- newSTRef Nothing
  return $ Cons ref refStream

genericReplicateM_ :: (Monad m, Integral i) => i -> m a -> m ()
genericReplicateM_ = (sequence_ .) . genericReplicate

loop :: Valued s o a T => BlooP s o a -> BlooP s o () -> BlooP s o ()
loop n' b = do
  n    <- n'
  valN <- val n
  genericReplicateM_ (valN :: T) b
