module Control.Concurrent.Actor.Test where

import Control.Concurrent.Actor

import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
  counterA <- newActor $ counter 0
  showerA <- newActor shower
  kradA <- newActor $ krad showerA output
  replicateM_ 23 $ sendIO counterA kradA

counter :: Integer -> Behavior (Actor Integer)
counter n customer = do
  send customer n
  become $ counter $ n + 1

newActor :: Behavior msg -> IO (Actor msg)
newActor b = do
  mailbox <- newChan
  forkIO $ let loop b = do
                 action <- liftM b $ readChan mailbox
                 let Result communications b' = actionToResult action b
                 mapM_ (\ (Communication a m) -> forkIO $ sendIO a m) communications
                 loop b'
           in loop b
  return $ actor $ writeChan mailbox

shower :: Show a => Behavior (a, Actor String)
shower (x, customer) = send customer (show x)

krad :: Actor (a, Actor b) -> Actor b -> Behavior a
krad producer consumer x = send producer (x, consumer)

output :: Actor String
output = actor putStrLn
