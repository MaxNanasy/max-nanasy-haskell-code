module Test.Test where

type A m a = m a

class Dual a where
    dual :: a -> a

instance Dual Bool where
    dual = not

instance (Dual a, Dual b) => Dual (a -> b) where
    dual f = dual . f . dual

instance Dual a => Dual [a] where
    dual = reverse . map dual

instance Dual Integer where
    dual = negate
