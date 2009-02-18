module Control.Concurrent.Actor.Test where

import Control.Concurrent.Actor

import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
  counterA <- newActor $ counter 0
  showerA <- newActor shower
  outputA <- actor putStrLn
  kradA <- newActor $ krad showerA outputA
  replicateM_ 23 $ sendIO counterA kradA

counter :: Integer -> Behavior (Actor Integer)
counter n customer = do
  send customer n
  become . counter $ n + 1

shower :: Show a => Behavior (a, Actor String)
shower (x, customer) = send customer (show x)

krad :: Actor (a, Actor b) -> Actor b -> Behavior a
krad producer consumer x = send producer (x, consumer)
