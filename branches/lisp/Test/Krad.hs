module Test.Krad where

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Monad (Cont r) where
    return = Cont . flip ($)
    (Cont c) >>= f = Cont $ \ k -> c $ \ x -> runCont (f x) k
