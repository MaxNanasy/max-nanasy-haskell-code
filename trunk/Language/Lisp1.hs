{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lisp (main) where

import Prelude hiding (read, print)

import Control.Monad.Identity
import Control.Monad.Reader

import Data.List
import Data.Maybe

import System.IO hiding (print)

data Object = SpecialOperator SpecialOperator
            | Macro Macro
            | Function Function
            | Cons Object Object
            | Nil
            | Symbol String
            | Char Object
            | Stream Object

instance Show Object where
  show (Cons a d)    = "(" ++ show a ++ "." ++ show d ++ ")"
  show Nil           = "()"
  show (Symbol name) = name
  show _             = "#<>"

newtype Lisp a = Lisp (Identity a) deriving Monad

runLisp :: Lisp a -> a
runLisp (Lisp (Identity x)) = x

type Function = Object -> Lisp Object
type Macro = Function
type SpecialOperator = Environment -> Function

type Environment = [(Object, Object)]

mapLlist :: Function
mapLlist (Cons (Function f) (Cons (Cons x xs) Nil)) = do
  a <- f $ Cons x Nil
  d <- mapLlist (Cons (Function f) (Cons xs Nil))
  return $ Cons a d
mapLlist (Cons (Function _) (Cons Nil         Nil)) = return Nil

eval :: SpecialOperator
eval env (Cons (Symbol name) Nil) = return . snd . fromMaybe (error $ name ++ " not found in " ++ show env) . find (\ ((Symbol name'), _) -> name == name') $ env
eval env (Cons (Cons   f d ) Nil) = do 
  fun <- eval env (Cons f Nil)
  case fun of
    SpecialOperator operator -> operator env d
    Function        function -> function =<< mapLlist (Cons (Function $ eval env) $ Cons d Nil)
    Macro           macro    -> macro d >>= \ form -> eval env (Cons form Nil)
eval _   (Cons x             Nil) = return x

quote :: SpecialOperator
quote _ (Cons form Nil) = return form

lambda :: SpecialOperator
lambda env (Cons params (Cons body Nil)) = case llistToList params of
  (pars, Nil        ) -> return $ Function $ \ args -> eval (zip pars (fst $ llistToList args) ++ env) (Cons body Nil)
  ([]  , Symbol name) -> return $ Function $ \ args -> eval ((Symbol name, args) : env)                (Cons body Nil)
  a                   -> error $ "lambda: " ++ show a

car, cdr :: Function
car (Cons (Cons a _) Nil) = return a
cdr (Cons (Cons _ d) Nil) = return d

macro :: Function
macro (Cons (Function f) Nil) = return $ Macro f

apply :: Function
apply (Cons (Function f) (Cons args Nil)) = f args

macroexpand1 :: Function
macroexpand1 (Cons (Macro macro) args) = macro args

{-quasiquote :: Macro
quasiquote (Cons (Cons (Symbol "comma") (Cons form Nil)) Nil) = return form
quasiquote (Cons (Cons form rest) Nil)                        = do
  a <- quasiquote $ Cons form Nil
  d <- quasiquote $ Cons rest Nil
  return $ Cons a d
quasiquote (Cons form Nil)                                    = return $ Cons (Symbol "quote") $ Cons form Nil-}

llistToList :: Object -> ([Object], Object)
llistToList (Cons a d) = case llistToList d of (xs, x) -> (a : xs, x)
llistToList x          = ([], x)

typeOf :: Function
typeOf (Cons (Function        {}) Nil) = return $ Symbol "function"
typeOf (Cons (Cons            {}) Nil) = return $ Symbol "cons"
typeOf (Cons (Nil               ) Nil) = return $ Symbol "nil"
typeOf (Cons (SpecialOperator {}) Nil) = return $ Symbol "special-operator"
typeOf (Cons (Symbol          {}) Nil) = return $ Symbol "symbol"
typeOf (Cons (Macro           {}) Nil) = return $ Symbol "macro"

ifFunction :: Function
ifFunction (Cons Nil (Cons (Function _) (Cons (Function e) Nil))) = e Nil
ifFunction (Cons _   (Cons (Function t) (Cons (Function _) Nil))) = t Nil

initialEnvironment :: Environment
initialEnvironment =
    [ (Symbol "quote"  , SpecialOperator quote )
    , (Symbol "lambda" , SpecialOperator lambda)
    , (Symbol "macro"  , Function macro)
    , (Symbol "car"    , Function car                           )
    , (Symbol "cdr"    , Function cdr                           )
    , (Symbol "cons"   , Function $ \ (Cons a (Cons d Nil)) -> return $ Cons a d)
--    , (Symbol "nil"    , Nil                                    )
    , (Symbol "type-of", Function typeOf                        )
    , (Symbol "eval"   , Function $ eval initialEnvironment     )
--    , (Symbol "append" , Function append                        )
    , (Symbol "macroexpand-1", Function macroexpand1            )
--    , (Symbol "list"         , Function list                    )
--    , (Symbol "quasiquote", Macro quasiquote                    )
    , (Symbol "if-function", Function ifFunction                           )
    , (Symbol "apply"      , Function apply                     )
    ]

main :: IO ()
main = runReaderT repl stdin

repl :: REPL ()
repl = do
  form <- read
  print $ runLisp $ eval initialEnvironment (Cons form Nil)
  writeCharacter '\n'
  repl

type REPL = ReaderT Handle IO

readCharacter :: REPL Char
readCharacter = ask >>= liftIO . hGetChar

peekCharacter :: REPL Char
peekCharacter = ask >>= liftIO . hLookAhead

isWhitespace :: Char -> Bool
isWhitespace = (`elem` " \t\n")

read :: REPL Object
read = do
  c <- readCharacter
  case c of
      '\'' -> do 
        object <- read
        return $ Cons (Symbol "quote") $ Cons object Nil
      '(' -> do
        readDelimitedList ')' '.'
      '`' -> do
        object <- read
        return $ Cons (Symbol "quasiquote") $ Cons object Nil
      ',' -> do
        c' <- peekCharacter
        symbolName <- case c' of
          '@' -> readCharacter >> return "comma-splice"
          _   ->                  return "comma"
        form <- read
        return $ Cons (Symbol symbolName) $ Cons form Nil
      _ | isWhitespace      c -> read
        | isSymbolCharacter c -> do token <- readToken
                                    return $ Symbol $ c : token

{-interpolateForm :: Object -> Object
interpolateForm form = interpolateForm' form Nil where
  interpolateForm' :: Object -> Object -> Object
  interpolateForm' (Cons (Symbol "comma"       ) (Cons form Nil)) k = Cons (Cons (Symbol "cons"  ) $ Cons form k) Nil
  interpolateForm' (Cons (Symbol "comma-splice") (Cons form Nil)) k = Cons (Cons (Symbol "append") $ Cons form k) Nil
  interpolateForm' (Cons a                       d              ) k = interpolateForm' a $ interpolateForm' d k
  interpolateForm' form                                           k = Cons (Cons (Symbol "cons"  ) $ Cons (Cons (Symbol "quote") $ Cons form Nil) k) Nil-}

skipWhitespace :: REPL ()
skipWhitespace = do
  c <- peekCharacter
  when (isWhitespace c) (readCharacter >> skipWhitespace)

isSymbolCharacter :: Char -> Bool
isSymbolCharacter = (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "+-*/")

readToken :: REPL String
readToken = do
  c <- peekCharacter
  if isSymbolCharacter c
    then do readCharacter
            token <- readToken
            return $ c : token
    else return ""

readDelimitedList :: Char -> Char -> REPL Object
readDelimitedList c d = do
  skipWhitespace
  c' <- peekCharacter
  if c' == c
    then do readCharacter
            return Nil
    else if c' == d
         then do readCharacter
                 (Cons object _) <- readDelimitedList ')' '.'
                 return object
         else do object <- read
                 rest <- readDelimitedList c d
                 return $ Cons object rest

writeCharacter :: Char -> REPL ()
writeCharacter = liftIO . putChar

writeString :: String -> REPL ()
writeString = liftIO . putStr

print :: Object -> REPL ()
print (Function _) = writeString "#<function>"
print (Macro    _) = writeString "#<macro>"
print (Cons (Symbol "quote") (Cons form Nil)) = do
  writeCharacter '\''
  print form
print (Cons (Symbol "quasiquote") (Cons form Nil)) = do
  writeCharacter '`'
  print form
print (Cons (Symbol "comma") (Cons form Nil)) = do
  writeCharacter ','
  print form
print (Cons (Symbol "comma-splice") (Cons form Nil)) = do
  writeString ",@"
  print form
print (Cons a d) = do
  writeCharacter '('
  print a
  let (list, dot) = llistToList d
  mapM (\ x -> writeCharacter ' ' >> print x) list
  case dot of
    Nil -> return ()
    _   -> writeString " . " >> print dot
  writeCharacter ')'
print Nil = writeString "()"
print (SpecialOperator _) = writeString "#<special operator>"
print (Symbol name) = writeString name
