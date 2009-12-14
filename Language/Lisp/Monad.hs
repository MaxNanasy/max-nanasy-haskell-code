{-# LANGUAGE Rank2Types #-}

module Language.Lisp.Monad where

import Language.Lisp.Types

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.ST
import Data.STRef

runLisp :: (forall s. Lisp s a) -> Stream -> (a, Stream)
runLisp c s = runST (do
                      envC <- liftM Cell $ newSTRef Nil
                      runReaderT (runWriterT (evalStateT (unLisp c) s)) envC)

liftST        x = Lisp (lift (lift (lift x)))
liftGlobalEnv x = Lisp       (lift (lift x))
liftStdout    x = Lisp             (lift x)
liftStdin     x = Lisp                   x

readCell :: Cell s -> Lisp s (Object s)
readCell (Cell r) = liftST . readSTRef $ r

writeCell :: Cell s -> Object s -> Lisp s ()
writeCell (Cell r) = liftST . writeSTRef r

newCell :: Object s -> Lisp s (Cell s)
newCell x = do
  xR <- liftST $ newSTRef x
  return $ Cell xR

readCharacter :: Lisp s Char
readCharacter = do 
  (c : cs) <- liftStdin get
  liftStdin $ put cs
  return c

peekCharacter :: Lisp s Char
peekCharacter = liftStdin $ gets head

writeCharacter :: Char -> Lisp s ()
writeCharacter = liftStdout . tell . (:[])

writeString :: String -> Lisp s ()
writeString = liftStdout . tell

getGlobalEnvironment :: Lisp s (Cell s)
getGlobalEnvironment = liftGlobalEnv ask
