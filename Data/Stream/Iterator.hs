module Data.Stream.Iterator ( module Data.Stream
                            , current
                            , next
                            , prev
                            , setCurrent
                            , applyToCurrent
                            , finiteListToStreamIterator
                            , StreamIterator (SI)
                            ) where

import Data.Stream

data StreamIterator a = SI (Stream a) (Stream a)

current :: StreamIterator a -> a
current (SI  _           (Stream x _ )) = x

next    :: StreamIterator a -> StreamIterator a
next    (SI  p           (Stream x n )) = SI (Stream x p) n

prev    :: StreamIterator a -> StreamIterator a
prev    (SI (Stream x p)  n           ) = SI p (Stream x n)

setCurrent :: a -> StreamIterator a -> StreamIterator a
setCurrent x (SI p (Stream _ n ))  = SI p (Stream x n)

applyToCurrent :: (a -> a) -> StreamIterator a -> StreamIterator a
applyToCurrent f i = setCurrent (f $ current i) i

finiteListToStreamIterator :: [a] -> StreamIterator a
finiteListToStreamIterator xs = SI (listToStream $ reverse xs) (listToStream xs)
