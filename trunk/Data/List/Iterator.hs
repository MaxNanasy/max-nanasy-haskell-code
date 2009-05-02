{-# OPTIONS_GHC -Wall #-}

module Data.List.Iterator where

data ListIterator a = LI [a] [a] deriving(Show)

iterator :: [a] -> ListIterator a
iterator   xs       = LI [] xs
deiterator :: ListIterator a -> [a]
deiterator (LI p n) = reverse p ++ n

current :: ListIterator a -> Maybe a
current (LI      _  (x : _ )) = Just x
current (LI      _       [] ) = Nothing

next :: ListIterator a -> Maybe (ListIterator a)
next    (LI      p  (x : n )) = Just $ LI (x : p)     n
next    (LI      _       [] ) = Nothing

prev :: ListIterator a -> Maybe (ListIterator a)
prev    (LI (x : p ) n  )     = Just $ LI      p (x : n)
prev    (LI      []  _  )     = Nothing

setCurrent :: a -> ListIterator a -> Maybe (ListIterator a)
setCurrent x (LI p (_ : n )) = Just $ LI p (x : n)
setCurrent _ (LI _      [] ) = Nothing

applyToCurrent :: (a -> a) -> ListIterator a -> Maybe (ListIterator a)
applyToCurrent f li = do
  x <- current li
  setCurrent (f x) li

circularListIterator :: [a] -> ListIterator a
circularListIterator l = LI (cycle $ reverse l) (cycle l)
