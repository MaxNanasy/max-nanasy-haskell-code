{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Lisp.Types where

import Control.Monad.Reader
import Data.IORef

data Object =   SpecialOperator (SpecialOperator)
              | Macro           (Macro          )
              | Function        (Function       )
              | Cons (Cell) (Cell)
              | Nil
              | Symbol String
              | Char Char

newtype Lisp a = Lisp { unLisp :: ReaderT Cell IO a } deriving (Monad, MonadIO)

type Stream = [Char]

type Function        = Object -> Lisp Object
type Macro           = Function
type SpecialOperator = Environment -> Function

type Environment = Object

newtype Cell = Cell (IORef Object)
