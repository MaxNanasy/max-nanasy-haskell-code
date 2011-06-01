> {-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
> {-# OPTIONS_GHC -Wall #-}
>
> module Language.Functional.Compiler.Types where
>

This is only to bring into scope the Pretty instance and the Expression type, for
prettying and casting, respectively.

> import qualified Language.Functional.Parser.Types as T
>
> import Text.PrettyPrint.Leijen
> 
> import Data.Dynamic hiding(cast)
> 
> import Prelude hiding(exp)
> 
> type Var = String
>

The untyped Lambda Calculus augmented with literal Haskell-based dynamic values.

> data Term = Lambda  Var  Term
>           | Apply   Term Term
>           | Var     Var
>             deriving(Show)
> 

A superfluous class.  instance Castable a b => A value of type a can be
converted to a value of type b.  The converse is not necessarily true.

> class Castable a b where
>     cast :: a -> b
> 
> instance Castable Term T.Expression where

I don't care enough to convert to a real multivariable Lambda.

>     cast (Lambda  var exp) = T.Lambda  [var]    (cast exp)
>     cast (Apply   f   x  ) = T.Apply   (cast f) (cast x)
>     cast (Var     var    ) = T.JustID   var
> 
> instance Castable T.Expression Term where
>     cast (T.Lambda  vars exp) = foldr Lambda (cast exp) vars
>     cast (T.Apply   f    x  ) = Apply (cast f) (cast x)
>     cast (T.JustID  var     ) = Var var
> 
> instance Pretty Term where
>     pretty = pretty . (cast :: Term -> T.Expression)
> 

The compiled form of a Term.
A function or a native Haskell value.

> type Value = Dynamic
>

An Environment is an AList mapping variables to bindings.

> type Environment = [(Var, Value)]

A Module consists of an Environment.

> newtype Module = Module { environment :: Environment }
>
