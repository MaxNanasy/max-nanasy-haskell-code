module Language.Lisp.Utility (mapLlist, append, zipLlists, lookupSymbol, lookupByName) where

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

listEqual :: Object -> Object -> Lisp Bool
Nil         `listEqual` Nil       = return True
Cons xC xsC `listEqual` Cons yC ysC = do
  x  <- readCell xC
  y  <- readCell yC
  xs <- readCell xsC
  ys <- readCell ysC
  liftM (x == y &&) (xs `listEqual` ys)
_           `listEqual` _         = return False

lookupByName :: Environment -> Object -> Lisp (Maybe Cell)
lookupByName Nil                      _                 = return Nothing
lookupByName (Cons entryC restC) name@(NewType _ name') = do
  Cons keyC valueC <- readCell entryC
  Symbol (NewType _ name'') <- readCell keyC
  found <- name' `listEqual` name''
  if found then return $ Just valueC else readCell restC >>= flip lookupByName name
lookupByName _                   NewType {}       = error "lookupByName: Environment is not a list."
lookupByName _                   _                = error "lookupByName: Not a string."

lookupSymbol :: Environment -> Object -> Lisp (Maybe Cell)
lookupSymbol env symbol = let
    lookupSymbol' :: Environment -> Lisp (Maybe Cell)
    lookupSymbol' Nil                 = return Nothing
    lookupSymbol' (Cons entryC restC) = do
                            Cons keyC valueC <- readCell entryC
                            key              <- readCell keyC
                            if symbol == key then return $ Just valueC else readCell restC >>= lookupSymbol'
    lookupSymbol' _                   = error "lookupSymbol: Environment is not a list."
  in do
    gEnv <- getGlobalEnvironment >>= readCell
    valueCM <- lookupSymbol' env
    case valueCM of
      Nothing -> lookupSymbol' gEnv
      Just _  -> return valueCM
