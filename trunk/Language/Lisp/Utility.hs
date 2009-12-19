module Language.Lisp.Utility (mapLlist, append, zipLlists, lookupSymbol) where

import Language.Lisp.Types
import Language.Lisp.Monad

import Control.Monad

cons :: Object -> Object -> Lisp Object
cons a d = liftM2 Cons (newCell a) (newCell d)

mapLlist :: Object -> Object -> Lisp Object
mapLlist fn@(Function (Idd f _)) (Cons aC dC) = do
  a <- f =<< readCell aC
  d <- mapLlist fn =<< readCell dC
  cons a d
mapLlist _ Nil = return Nil
mapLlist _ _   = error "mapLlist: Not a list or not a function."

append :: Object -> Object -> Lisp Object
append Nil           ys = return ys
append (Cons xC xsC) ys = do
  x  <- readCell xC
  xs <- readCell xsC
  append xs ys >>= cons x
append _             _  = error "append: Not a list."

zipLlists :: Object -> Object -> Lisp Object
zipLlists Nil           _             = return Nil
zipLlists _             Nil           = return Nil
zipLlists (Cons xC xsC) (Cons yC ysC) = do
  x  <- readCell xC
  y  <- readCell yC
  xs <- readCell xsC
  ys <- readCell ysC
  pair <- cons x y
  rest <- zipLlists xs ys
  cons pair rest
zipLlists _             _             = error "zipLlists: Not a list."

lookupSymbol :: Environment -> Object -> Lisp (Maybe Cell)
lookupSymbol env (Symbol name) = let
    lookupSymbol' :: Environment -> Lisp (Maybe Cell)
    lookupSymbol' Nil                 = return Nothing
    lookupSymbol' (Cons entryC restC) = do
                                   Cons keyC valueC <- readCell entryC
                                   Symbol name'     <- readCell keyC
                                   if name == name' then return $ Just valueC else readCell restC >>= lookupSymbol'
    lookupSymbol' _                   = error "lookupSymbol: Environment is not a list."
  in do gEnv <- getGlobalEnvironment >>= readCell
        valueCM <- lookupSymbol' env
        case valueCM of
          Nothing -> lookupSymbol' gEnv
          Just _  -> return valueCM
lookupSymbol _   _             = error "lookupSymbol: Not a symbol."
