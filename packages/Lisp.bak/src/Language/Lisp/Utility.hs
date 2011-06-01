module Language.Lisp.Utility (mapLlist, append, zipLlists, lookupSymbol, lookupSymbolLexically) where

import Language.Lisp.Monad

cons :: Object -> Object -> Lisp Object
cons a d = liftM2 Cons (newCell a) (newCell d)

mapLlist :: Object -> Object -> Lisp Object
mapLlist fn@(Function (Idd f _)) (Cons aC dC) = do
  a <- f . (: []) =<< readCell aC
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

lookupSymbol :: Environment -> Object -> Maybe Cell
lookupSymbol = flip lookup

data EnvType = Global | Lexical

lookupSymbolLexically :: Environment -> Object -> Lisp (Maybe (Cell, EnvType))
lookupSymbolLexically env symbol = do
  case lookupSymbol env symbol of
    Nothing -> do
      gEnv <- getGlobalEnvironment
      return $ fmap (\ vC -> (vC, Global)) $ lookupSymbol gEnv symbol
    Just vC -> return $ Just (vC, Lexical)
