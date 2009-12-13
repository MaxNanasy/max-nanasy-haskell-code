module Lisp where

import Prelude hiding (read, print)

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

type Function = Object -> Object
type Macro = Function
type SpecialOperator = Environment -> Function

type Environment = [(Object, Object)]

type Stream = [Char]
type REPL = IO

list :: Function
list = id

mapLlist :: Function
mapLlist (Cons (Function f) (Cons (Cons x xs) Nil)) = Cons (f (Cons x Nil)) (mapLlist (Cons (Function f) (Cons xs Nil)))
mapLlist (Cons (Function _) (Cons Nil         Nil)) = Nil

eval :: Environment -> Object -> Object
eval env (Cons (Symbol name) Nil) = snd . fromJust . find (\ ((Symbol name'), _) -> name == name') $ env
eval env (Cons (Cons   f d ) Nil) = case eval env (Cons f Nil) of
  SpecialOperator operator -> operator env d
  Function        function -> function $ mapLlist (Cons (Function $ eval env) $ Cons d Nil)
  Macro           macro    -> eval env (Cons (macro d) Nil)
eval _   (Cons x             Nil) = x

quote :: SpecialOperator
quote _ (Cons form Nil) = form

lambda :: SpecialOperator
lambda env (Cons params (Cons body Nil)) = Function $ \ args -> eval (zip (fst $ llistToList params) (fst $ llistToList args) ++ env) (Cons body Nil)

car, cdr :: Function
car (Cons (Cons a _) Nil) = a
cdr (Cons (Cons _ d) Nil) = d

--letMacro :: Macro
--letMacro [bindings, body] = let (llistToList bindings

macro :: Function
macro (Cons (Function f) Nil) = Macro f

macroexpand1 :: Function
macroexpand1 (Cons (Macro macro) args) = macro args

quasiquote :: Macro
quasiquote (Cons (Cons (Symbol "comma") (Cons form Nil)) Nil) = form
quasiquote (Cons (Cons form rest) Nil)                        = Cons (quasiquote (Cons form Nil)) (quasiquote (Cons rest Nil))
quasiquote (Cons form Nil)                                    = Cons (Symbol "quote") $ Cons form Nil

llistToList :: Object -> ([Object], Object)
llistToList (Cons a d) = case llistToList d of (xs, x) -> (a : xs, x)
llistToList x          = ([], x)

listToLlist :: Object -> [Object] -> Object
listToLlist = foldr Cons

typeOf :: Function
typeOf (Cons (Function        {}) Nil) = Symbol "function"
typeOf (Cons (Cons            {}) Nil) = Symbol "cons"
typeOf (Cons (Nil               ) Nil) = Symbol "nil"
typeOf (Cons (SpecialOperator {}) Nil) = Symbol "special-operator"
typeOf (Cons (Symbol          {}) Nil) = Symbol "symbol"
typeOf (Cons (Macro           {}) Nil) = Symbol "macro"

initialEnvironment :: Environment
initialEnvironment =
    [ (Symbol "quote"  , SpecialOperator quote )
    , (Symbol "lambda" , SpecialOperator lambda)
    , (Symbol "macro"  , Function macro)
    , (Symbol "car"    , Function car                           )
    , (Symbol "cdr"    , Function cdr                           )
    , (Symbol "cons"   , Function $ \ (Cons a (Cons d Nil)) -> Cons a d)
    , (Symbol "nil"    , Nil                                    )
    , (Symbol "type-of", Function typeOf                        )
    , (Symbol "eval"   , Function $ eval initialEnvironment     )
    , (Symbol "append" , Function append                        )
    , (Symbol "macroexpand-1", Function macroexpand1            )
    , (Symbol "list"         , Function list                    )
    , (Symbol "map"          , Function mapLlist                )
    , (Symbol "quasiquote", Macro quasiquote                    )
    ]

append :: Function
append (Cons x (Cons y Nil)) = listToLlist Nil $ fst (llistToList x) ++ fst (llistToList y)

main :: IO ()
main = repl

repl :: REPL ()
repl = do
  form <- read
  print $ eval initialEnvironment (Cons form Nil)
  writeCharacter '\n'
  repl

readCharacter :: REPL Char
readCharacter = getChar

peekCharacter :: REPL Char
peekCharacter = hLookAhead stdin

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
writeCharacter = putChar

writeString :: String -> REPL ()
writeString = putStr

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
