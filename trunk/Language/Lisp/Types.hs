{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Lisp.Types where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.ST
import Data.STRef

data Object s = SpecialOperator (SpecialOperator s)
              | Macro           (Macro           s)
              | Function        (Function        s)
              | Cons (Cell s) (Cell s)
              | Nil
              | Symbol String
              | Char Char

newtype Lisp s a = Lisp { unLisp :: StateT Stream (WriterT Stream (ST s)) (ReaderT (Cell s) Identity a) }

instance Monad (Lisp s) where
    f 

type Stream = [Char]

type Function        s = Object s -> Lisp s (Object s)
type Macro           s = Function s
type SpecialOperator s = Environment s -> Function s

type Environment s = [(Object s, Object s)]

newtype Cell s = Cell (STRef s (Object s))
