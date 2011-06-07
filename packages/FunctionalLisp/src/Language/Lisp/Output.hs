module Language.Lisp.Output (write, writeChar) where

import Language.Lisp.Monad
import Language.Lisp.List

import System.IO

import Control.Monad.Trans

writeChar :: Stream -> Char -> Lisp ()
writeChar = (liftIO .) . hPutChar

writeString :: Stream -> String -> Lisp ()
writeString = (liftIO .) . hPutStr

write :: Stream -> Object -> Lisp ()
write stream x = do
  case x of
    Function (Idd _ n)        -> do
                   writeString' "#<function "
                   writeString' $ show n
                   writeChar' '>'
    Macro    (Idd _ n)        -> do
                   writeString' "#<macro "
                   writeString' $ show n
                   writeChar' '>'
    SpecialOperator so        -> do
                   writeString' "#<special-operator "
                   writeString' $ show so
                   writeChar' '>'
    Cons yC ysC               -> do
                   y  <- readCell yC
                   ys <- readCell ysC
                   (list, dot) <- llistToList ys
                   writeChar' '('
                   write' y
                   mapM_ (\ z -> writeChar' ' ' >> write' z) list
                   case dot of
                     Nil -> return ()
                     _   -> writeString' " . " >> write' dot
                   writeChar' ')'
    Nil                       -> writeString' "()"
    Symbol (Idd name _)       -> writeString' name
    Char char                 -> writeString' "#\\" >> writeChar' char
    Stream _                  -> writeString' "#<stream>"
    NewType t y               -> do
                   writeChar' '@'
                   writeChar' ' '
                   write' t
                   writeChar' ' '
                   write' y
    String string             -> do
                   writeChar' '"'
                   writeString' string
                   writeChar' '"'
    where
      writeString' = writeString stream
      writeChar'   = writeChar   stream
      write'       = write       stream
