{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

module Data.FDTS where

type DTime = Int -- The input for discrete-time signals is represented by Int for efficiency with List functions.
type Range a = (a, a)
type DTR = Range DTime

data FDTS e = FDTS { values :: [e], range :: DTR, defalt :: e} -- A Finite-valued Discrete-Time Signal.
              deriving (Show)                                  -- Unlike an actual FDTS, the default value may be nonzero.
-- values : consecutive values from low to high inputs in finite range
-- range  : bounds of finite range; default = (low, low + length values) = (low, high); low-inclusive, high-exclusive
-- defalt: value for inputs outside of finite range

instance Functor FDTS where
    fmap f x@(FDTS { values = xs, defalt = def }) = x { values = map f xs, defalt = (f def) }

makeFDTS :: a -> DTR -> [a] -> FDTS a
makeFDTS def r@(low, _high) xs = FDTS (select r (FDTS xs (low, low + length xs) def)) r def

makeFDTS' :: Num a => DTime -> [a] -> FDTS a
makeFDTS' low xs = FDTS xs (low, low + length xs) 0

select :: DTR -> FDTS a -> [a]
select (low', high') (FDTS xs (low, high) def) = (if low' < low then (replicate (low - low') def ++) else drop (low' - low)) $ (if high' > high then (++ replicate (high' - high) def) else take (high' - low)) xs

shift :: DTime -> FDTS a -> FDTS a
shift n x@(FDTS { range = (low, high) }) = x { range = (low + n, high + n) }

fold :: FDTS a -> FDTS a
fold x@FDTS { values = xs, range = (low, high) } = x { values = reverse xs, range = (-high, -low) }

instance Num a => Num (FDTS a) where
    (+)           = combine (+)
    (*)           = combine (*)
    negate        = fmap negate
    abs           = fmap abs
    signum        = fmap signum
    fromInteger n = FDTS [] (0, 0) (fromInteger n)

instance Eq a => Eq (FDTS a) where
    x0 == x1 = normalize x0 === normalize x1

(===) :: Eq a => FDTS a -> FDTS a -> Bool
FDTS xs0 r0 def0 === FDTS xs1 r1 def1 = xs0 == xs1 && r0 == r1 && def0 == def1

nir :: Ord a => Range a -> Range a -> Range a
nir r0@(low0, high0) r1@(low1, high1) | low0 == high0 = r1
                                      | low1 == high1 = r0
                                      | otherwise     = (min low0 low1, max high0 high1)

combine :: (a -> b -> c) -> FDTS a -> FDTS b -> FDTS c
combine f x0@(FDTS { range = r0, defalt = def0 }) x1@(FDTS { range = r1, defalt = def1 }) = let r = nir r0 r1 in FDTS (zipWith f (select r x0) (select r x1)) r (f def0 def1)

enumRange :: Enum a => Range a -> [a]
enumRange = init . uncurry enumFromTo

(~*~) :: Num a => FDTS a -> FDTS a -> Maybe (FDTS a)
FDTS xs0 r0@(low0, _) 0 ~*~ x1 = Just $ sum $ map (\ k -> fmap (xs0 !! (k - low0) *) $ shift k x1) $ enumRange r0
_ ~*~ _ = Nothing

normalize :: Eq a => FDTS a -> FDTS a
normalize = normalizeRange . normalizeValues where
    normalizeValues = fold . normalizeInit . fold . normalizeInit
    normalizeInit x@(FDTS (e:xs) (low, high) def) | e == def = normalizeInit $ x { values = xs, range = (low + 1, high) }
    normalizeInit x = x
    normalizeRange x@(FDTS { range = (low, high) }) | low == high = x { range = (0, 0) }
    normalizeRange x = x

data SymExpr = Symbol String
             | SymExpr :+: SymExpr
             | SymExpr :*: SymExpr
             | Number Int
               deriving (Eq)

instance Show SymExpr where
    show (Symbol x) = x
    show (x :+: y)  = show x ++ "+" ++ show y
    show (x :*: y)  = show x ++ "*" ++ show y
    show (Number n) = show n

instance Num SymExpr where
    (+)         = (:+:)
    (*)         = (:*:)
    abs         = undefined
    signum      = undefined
    fromInteger = Number . fromInteger
