module Language.Lisp.List where

import Language.Lisp.Monad

cons :: Object -> Object -> Lisp Object
cons x xs = liftM2 Cons (newCell x) (newCell xs)

llistToList :: Object -> Lisp ([Object], Object)
llistToList (Cons xC xsC) = do
  x  <- readCell xC
  xs <- readCell xsC
  (list, dot) <- llistToList xs
  return (x : list, dot)
llistToList x             = return ([], x)
