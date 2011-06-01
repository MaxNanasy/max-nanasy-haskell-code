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
            | Function        { applyObject :: Function }
            | Cons            Cell Cell
            | Nil
            | Symbol          (Idd String)
            | String          String
            | Char            Char
            | Stream          Stream
            | NewType         Object Object
              deriving Eq

newtype Lisp a = Lisp { unLisp :: ReaderT Environment (StateT SymbolTable (StateT Environment (StateT Identifier (ContT Object IO)))) a } deriving (Monad, MonadIO, MonadCont)

type Stream = Handle

type Function        = Idd ([Object] -> Lisp Object)
type Macro           = Function
data SpecialOperator = Quote
                     | Lambda
                     | Set
                       deriving (Eq, Show)

type Identifier = Integer

type Environment = [(Object, Cell)]
type SymbolTable = [Object]

newtype Cell = Cell (Idd (IORef Object)) deriving (Eq)
