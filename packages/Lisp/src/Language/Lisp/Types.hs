{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Lisp.Types where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Cont
import Data.IORef
import System.IO

data Idd a = Idd a !Identifier

instance Eq (Idd a) where
    (Idd _ id0) == (Idd _ id1) = id0 == id1

data Object = SpecialOperator SpecialOperator
            | Macro           Macro
            | Function        Function
            | Cons            Cell Cell
            | Nil
            | Symbol          (Idd Object)
            | Char            Char
            | Stream          Stream
            | NewType         Object Object
              deriving Eq

newtype Lisp a = Lisp { unLisp :: ReaderT Environment (ReaderT Cell (StateT Identifier (ContT Object IO))) a } deriving (Monad, MonadIO, MonadCont)

type Stream = Handle

type Function        = Idd (Object -> Lisp Object)
type Macro           = Function
type SpecialOperator = Idd (Environment -> Object -> Lisp Object)

type Identifier = Integer

type Environment = Object

newtype Cell = Cell (Idd (IORef Object)) deriving (Eq)
