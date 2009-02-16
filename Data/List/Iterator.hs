{-# OPTIONS_GHC -Wall #-}

module Data.List.Iterator where

newtype ListIterator a = LI ([a],[a]) deriving(Show)

iterator :: [a] -> ListIterator a
iterator   x          = LI ([],x)
deiterator :: ListIterator a -> [a]
deiterator (LI (p,n)) = reverse p ++ n

now :: ListIterator a -> a
now        (LI (_  , x:_))               = x
now        (LI (_  , [] ))               = error "Brainfuck.now: now    of empty LI"

next :: ListIterator a -> ListIterator a
next       (LI (p  , x:n))               = LI (x:p,n)
next       (LI (_  , [] ))               = error "Brainfuck.next: next   of empty LI"

prev :: ListIterator a -> ListIterator a
prev       (LI (x:p, n  ))               = LI (p,x:n)
prev       (LI ([] , _  ))               = error "Brainfuck.prev: prev   of empty LI"

setNow :: a -> ListIterator a -> ListIterator a
setNow     x               (LI (p, _:n)) = LI (p,x:n)
setNow     _               (LI (_, [] )) = error "Brainfuck.setNow: setNow of empty LI"

applyToNow :: (a -> a) -> ListIterator a -> ListIterator a
applyToNow f               l             = setNow (f $ now l) l

circularListIterator :: [a] -> ListIterator a
circularListIterator l = LI $ (cycle $ reverse l, cycle l)
