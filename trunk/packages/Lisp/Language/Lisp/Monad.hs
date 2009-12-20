module Language.Lisp.Monad (runLisp, readCell, writeCell, newCell, readCharacter, peekCharacter, writeCharacter, writeString, getGlobalEnvironment, getDynamicEnvironment, withAugmentedDynamicEnvironment, incrementIdCounter, callWithCurrentContinuation) where

import Language.Lisp.Types

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Cont
import Data.IORef

import System.IO

runLisp :: Lisp Object -> IO Object
runLisp c = do
  envR <- newIORef Nil
  let envC = Cell (Idd envR 0)
  runContT (evalStateT (runReaderT (runReaderT (unLisp c) Nil) envC) 1) return

_liftContinuation     c = Lisp (lift (lift (lift c)))
liftIdentifierCounter c = Lisp       (lift (lift c))
liftGlobalEnv         c = Lisp             (lift c)
liftDynamicEnv        c = Lisp                   c

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

readCharacter :: Handle -> Lisp (Maybe Char)
readCharacter stream = liftIO $ do
                         eof <- hIsEOF stream
                         if eof then return Nothing else liftM Just $ hGetChar stream

peekCharacter :: Handle -> Lisp (Maybe Char)
peekCharacter stream = liftIO $ do
                         eof <- hIsEOF stream
                         if eof then return Nothing else liftM Just $ hLookAhead stream

writeCharacter :: Handle -> Char -> Lisp ()
writeCharacter = (liftIO .) . hPutChar

writeString :: Handle -> String -> Lisp ()
writeString = (liftIO .) . hPutStr

getGlobalEnvironment :: Lisp Cell
getGlobalEnvironment = liftGlobalEnv ask

getDynamicEnvironment :: Lisp Environment
getDynamicEnvironment = liftDynamicEnv ask

cons :: Object -> Object -> Lisp Object
cons x xs = do
  xC  <- newCell x
  xsC <- newCell xs
  return $ Cons xC xsC

append :: Object -> Object -> Lisp Object
append Nil           ys = return ys
append (Cons xC xsC) ys = do
  x  <- readCell xC
  xs <- readCell xsC
  append xs ys >>= cons x
append _             _  = error "append: Not a list."

withAugmentedDynamicEnvironment :: Environment -> Lisp a -> Lisp a
withAugmentedDynamicEnvironment env' c = do
  env <- getDynamicEnvironment
  env'' <- append env' env
  liftDynamicEnv $ local (const env'') $ unliftDynamicEnv c

car :: Object -> Lisp Object
car (Cons aC _ ) = readCell aC
car _            = error "car: Not a cons."

callWithCurrentContinuation :: Object -> Lisp Object
callWithCurrentContinuation (Function (Idd f _)) = do
  n <- incrementIdCounter
  callCC $ \ k -> cons (Function (Idd (\ args -> car args >>= k) n)) Nil >>= f
callWithCurrentContinuation _                    = error "call/cc: Not a function."
