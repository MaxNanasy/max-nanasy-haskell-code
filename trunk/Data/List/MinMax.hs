{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Data.List.MinMax(minMax) where

import Control.Monad.Writer.Lazy

mapToPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapToPair f g (x, y) = (f x, g y)

minMax :: Ord a => [a] -> (a, a)
minMax = mapToPair minimum maximum . segregateList

prop_minMax :: Ord a => [a] -> Bool
prop_minMax xs = null xs || minMax xs == (minimum xs, maximum xs)

segregateList :: Ord a => [a] -> ([a], [a])
segregateList = segregateList' . partitionList

segregateList' :: Ord a => PartitionedList a -> ([a], [a])
segregateList' (Just x , ([]  , []  )) = ([x], [x])
segregateList' (Just x , (y:ys, z:zs)) = segregateList'' (minMax3 x y z) ys zs
segregateList' (Nothing, (xs  , ys  )) = unzip $ zipWith minMax2 xs ys

segregateList'' :: Ord a => (a, a) -> [a] -> [a] -> ([a], [a])
segregateList'' (x, y) xs ys = mapToPair (x:) (y:) $ segregateList' (Nothing, (xs, ys))

type PartitionedList a = (Maybe a, ([a], [a]))

partitionList :: [a] -> PartitionedList a
partitionList (x : y  : zs) = partitionList' x y $ partitionList zs
partitionList (x : []     ) = (Just x , ([], []))
partitionList []            = (Nothing, ([], []))

partitionList' :: a -> a -> PartitionedList a -> PartitionedList a
partitionList' x y (m, (xs, ys)) = (m, (x:xs, y:ys))

minMax2M :: (Num w, MonadWriter (Sum w) m, Ord a) => a -> a -> m (a, a)
minMax2M x y = do
  let xLess = x < y
  tell (Sum 1)
  return $ if xLess
           then (x, y)
           else (y, x)

minMax2 :: Ord a => a -> a -> (a, a)
minMax2 x y = if x < y then (x, y) else (y, x)

minMax3 :: Ord a => a -> a -> a -> (a, a)
minMax3 x y z = if x < y
                then
                    if y < z
                    then
                        (x, z)
                    else
                        (min x z, y)
                else
                    if x < z
                    then
                        (y, z)
                    else
                        (min y z, x)
