module Data.Stream where

data Stream a = Stream a (Stream a)

listToStream :: [a] -> Stream a
listToStream xs = foldr Stream (listToStream xs) xs

instance Functor Stream where
    fmap f (Stream x xs) = Stream (f x) (fmap f xs)
