module Language.Lisp.Monad (module Language.Lisp.Types, module Control.Monad, runLisp, liftContinuation, liftIdentifierCounter, liftGlobalEnv, liftInternTable, liftDynamicEnv, readCell, writeCell, newCell, getGlobalEnvironment, augmentGlobalEnvironment, getDynamicEnvironment, withAugmentedDynamicEnvironment, incrementIdCounter, callWithCurrentContinuation) where

import Language.Lisp.Types

import Control.Monad

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Cont
import Data.IORef

import System.IO

runLisp :: Lisp Object -> IO Object
runLisp c = runContT (evalStateT (evalStateT (evalStateT (runReaderT (unLisp c) []) []) []) 0) return

liftContinuation      c = Lisp (lift (lift (lift (lift c))))
liftIdentifierCounter c = Lisp       (lift (lift (lift c)))
liftGlobalEnv         c = Lisp             (lift (lift c))
liftInternTable       c = Lisp                   (lift c)      
liftDynamicEnv        c = Lisp                         c

unliftDynamicEnv (Lisp c) = c

incrementIdCounter :: Lisp Identifier
incrementIdCounter = do
  n <- liftIdentifierCounter get
  liftIdentifierCounter $ put $ n + 1
  return n

readCell :: Cell -> Lisp Object
readCell  (Cell (Idd c _)) = liftIO . readIORef $ c

writeCell :: Cell -> Object -> Lisp ()
writeCell (Cell (Idd c _)) = liftIO . writeIORef c

newCell :: Object -> Lisp Cell
newCell x = do
  xC <- liftIO $ newIORef x
  n <- incrementIdCounter
  return $ Cell (Idd xC n)

getGlobalEnvironment :: Lisp Environment
getGlobalEnvironment = liftGlobalEnv get

augmentGlobalEnvironment :: Environment -> Lisp ()
augmentGlobalEnvironment = liftGlobalEnv . modify . (++)

getDynamicEnvironment :: Lisp Environment
getDynamicEnvironment = liftDynamicEnv ask

withAugmentedDynamicEnvironment :: Environment -> Lisp a -> Lisp a
withAugmentedDynamicEnvironment env c = liftDynamicEnv $ local (env ++) $ unliftDynamicEnv c

callWithCurrentContinuation :: Object -> Lisp Object
callWithCurrentContinuation (Function (Idd f _)) = do
  n <- incrementIdCounter
  callCC $ \ k -> f [Function (Idd (\ [arg] -> k arg) n)]
callWithCurrentContinuation _                    = error "call/cc: Not a function."
