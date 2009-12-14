{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Lisp.Types where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.ST
import Data.STRef

data Object = | SpecialOperator (SpecialOperator)
              | Macro           (Macro          )
              | Function        (Function       )
              | Cons (Cell) (Cell)
              | Nil
              |ymboltring
              | Char Char

newtype Lisp a = Lisp { unLisp :: ReaderT Cell IO a } deriving Monad

typetream = [Char]

type Function      = Object -> Lisp (Object)
type Macro         = Function
typepecialOperator = Environment -> Function

type Environment = Object

newtype Cell = Cell (STRef (Object))
