module Language.Lisp (main) where

import Language.Lisp.Monad
import Language.Lisp.Types

import Prelude hiding (read, print)

import Control.Monad

import Data.List
import Data.Maybe

import System.IO hiding (print)

mapLlist f (Cons aC dC) = do
  a <- f =<< readCell aC
  d <- mapLlist f =<< readCell dC
  cons a d
mapLlist _ Nil = return Nil
mapLlist _ _   = error "mapLlist: Not a list."

eval env (Symbol name )      = return . snd . fromMaybe (error $ name ++ " not found.") . find (\ ((Symbol name'), _) -> name == name') $ env
eval env (Cons   funC argsC) = do
  fun <- eval env =<< readCell funC
  args <- readCell argsC
  case fun of
    SpecialOperator operator -> operator env args
    Function        function -> function =<< mapLlist (eval env) args
    Macro           macro    -> macro args >>= eval env
    _                        -> error "eval: Not a functional value."
eval _   x                   = return x

quote _ form = return form

lambda :: Environment s -> Object s -> Object s -> Lisp s (Object s)
lambda env params body = do 
  (params', rest) <- llistToList params
  let f args = do
        (args', Nil) <- llistToList args
        eval ((case rest of Nil -> zip params' args' ; _ -> [(rest, args)]) ++ env) body
  return (Function f)

--defineFunction symbol value = 

car (Cons aC _ ) = readCell aC
car _            = error "car: Not a cons."
cdr (Cons _  dC) = readCell dC
cdr _            = error "cdr: Not a cons."

cons a d = liftM2 Cons (newCell a) (newCell d)

macro (Function f) = return $ Macro f
macro _            = error "macro: Not a function."

apply (Function f) args = f args
apply _            _    = error "apply: Not a function."

macroexpand1 (Macro macro) args = macro args
macroexpand1 _             _    = error "macroexpand-1: Not a macro."

llistToList :: Object s -> Lisp s ([Object s], Object s)
llistToList (Cons aC dC) = do
  a <- readCell aC
  d <- readCell dC
  (xs, x)  <- llistToList d
  return (a : xs, x)
llistToList x            = return ([], x)

typeOf Function        {} = return $ Symbol "function"
typeOf Cons            {} = return $ Symbol "cons"
typeOf Nil                = return $ Symbol "nil"
typeOf SpecialOperator {} = return $ Symbol "special-operator"
typeOf Symbol          {} = return $ Symbol "symbol"
typeOf Macro           {} = return $ Symbol "macro"
typeOf Char            {} = return $ Symbol "char"

ifFunction Nil (Function _) (Function e) = e Nil
ifFunction _   (Function t) (Function _) = t Nil
ifFunction _   _            _            = error "ifFunction: Not a function."

type OneParam s = Object s -> Lisp s (Object s)
oneParam :: String -> OneParam s -> Function s
oneParam fname f args =
  case args of
    Cons arg0C rest0C -> do
      rest0 <- readCell rest0C
      case rest0 of
        Nil -> do
          arg0 <- readCell arg0C
          f arg0
        _ -> error $ fname ++ ": Expected 1 argument; received more."
    _ -> error $ fname ++ ": Expected 1 argument; received 0."

type TwoParam s = Object s -> Object s -> Lisp s (Object s)
twoParam :: String -> TwoParam s -> Function s
twoParam fname f args =
  case args of
    Cons arg0C rest0C -> do
      rest0 <- readCell rest0C
      case rest0 of
        Cons arg1C rest1C -> do
          rest1 <- readCell rest1C
          case rest1 of
            Nil -> do
              arg0 <- readCell arg0C
              arg1 <- readCell arg1C
              f arg0 arg1
            _   -> error $ fname ++ ": Expected 2 arguments; received more."
        _ -> error $ fname ++ ": Expected 2 arguments; received 1."
    _ -> error $ fname ++ ": Expected 2 arguments; received 0."

type ThreeParam s = Object s -> Object s -> Object s -> Lisp s (Object s)
threeParam :: String -> ThreeParam s -> Function s
threeParam fname f args =
  case args of
    Cons arg0C rest0C -> do
      rest0 <- readCell rest0C
      case rest0 of
        Cons arg1C rest1C -> do
          rest1 <- readCell rest1C
          case rest1 of
            Cons arg2C rest2C -> do
              rest2 <- readCell rest2C
              case rest2 of
                Nil -> do
                  arg0 <- readCell arg0C
                  arg1 <- readCell arg1C
                  arg2 <- readCell arg2C
                  f arg0 arg1 arg2
                _ -> error $ fname ++ ": Expected 3 arguments; received more."
            _ -> error $ fname ++ ": Expected 3 arguments; received 2."
        _ -> error $ fname ++ ": Expected 2 arguments; received 1."
    _ -> error $ fname ++ ": Expected 2 arguments; received 0."

initialEnvironment :: Environment s
initialEnvironment =
    [ (Symbol "quote"        , SpecialOperator $ \ env -> oneParam "quote"  (quote  env)      )
    , (Symbol "lambda"       , SpecialOperator $ \ env -> twoParam "lambda" (lambda env)      )
    , (Symbol "macro"        , Function $ oneParam   "macro"         macro                    )
    , (Symbol "car"          , Function $ oneParam   "car"           car                      )
    , (Symbol "cdr"          , Function $ oneParam   "cdr"           cdr                      )
    , (Symbol "cons"         , Function $ twoParam   "cons"          cons                     )
    , (Symbol "type-of"      , Function $ oneParam   "type-of"       typeOf                   )
    , (Symbol "eval"         , Function $ oneParam   "eval"          (eval initialEnvironment))
    , (Symbol "macroexpand-1", Function $ twoParam   "macroexpand-1" macroexpand1             )
    , (Symbol "if-function"  , Function $ threeParam "if-function"   ifFunction               )
    , (Symbol "apply"        , Function $ twoParam   "apply"         apply                    )
    ]

main :: IO ()
main = do
  ((), output) <- liftM (runLisp repl) getContents
  putStr output

repl :: Lisp s ()
repl = do
  form <- read
  result <- eval initialEnvironment form
  print result
  writeCharacter '\n'
  repl

isWhitespace :: Char -> Bool
isWhitespace = (`elem` " \t\n")

read :: Lisp s (Object s)
read = do
  c <- readCharacter
  case c of
      '\'' -> do 
        object <- read
        cons (Symbol "quote") =<< cons object Nil
      '(' -> do
        readDelimitedList ')' '.'
      '`' -> do
        object <- read
        cons (Symbol "quasiquote") =<< cons object Nil
      ',' -> do
        c' <- peekCharacter
        symbolName <- case c' of
          '@' -> readCharacter >> return "comma-splice"
          _   ->                  return "comma"
        form <- read
        cons (Symbol symbolName) =<< cons form Nil
      _ | isWhitespace      c -> read
        | isSymbolCharacter c -> do token <- readToken
                                    return $ Symbol $ c : token
        | otherwise           -> error $ "RAPE\nby" ++ c : []

{-interpolateForm :: Object -> Object
interpolateForm form = interpolateForm' form Nil where
  interpolateForm' :: Object -> Object -> Object
  interpolateForm' (Cons (Symbol "comma"       ) (Cons form Nil)) k = Cons (Cons (Symbol "cons"  ) $ Cons form k) Nil
  interpolateForm' (Cons (Symbol "comma-splice") (Cons form Nil)) k = Cons (Cons (Symbol "append") $ Cons form k) Nil
  interpolateForm' (Cons a                       d              ) k = interpolateForm' a $ interpolateForm' d k
  interpolateForm' form                                           k = Cons (Cons (Symbol "cons"  ) $ Cons (Cons (Symbol "quote") $ Cons form Nil) k) Nil-}

skipWhitespace :: Lisp s ()
skipWhitespace = do
  c <- peekCharacter
  when (isWhitespace c) (readCharacter >> skipWhitespace)

isSymbolCharacter :: Char -> Bool
isSymbolCharacter = (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "+-*/")

readToken :: Lisp s String
readToken = do
  c <- peekCharacter
  if isSymbolCharacter c
    then do readCharacter
            token <- readToken
            return $ c : token
    else return ""

readDelimitedList :: Char -> Char -> Lisp s (Object s)
readDelimitedList c d = do
  skipWhitespace
  c' <- peekCharacter
  if c' == c
    then do readCharacter
            return Nil
    else if c' == d
         then do readCharacter
                 (Cons objectC _) <- readDelimitedList ')' '.'
                 readCell objectC
         else do object <- read
                 rest <- readDelimitedList c d
                 cons object rest

print :: Object s -> Lisp s ()
print (Function _) = writeString "#<function>"
print (Macro    _) = writeString "#<macro>"
print (Cons aC dC) = do
  a <- readCell aC
  d <- readCell dC
  let printNormally = do
                       (list, dot) <- llistToList d
                       mapM (\ x -> writeCharacter ' ' >> print x) list
                       case dot of
                         Nil -> return ()
                         _   -> writeString " . " >> print dot
{-        writeCharacter '('
        print a
        writeString " . "
        print d
        writeCharacter ')' -}
  case d of
    Cons formC nilC -> do
      nil <- readCell nilC
      case nil of
        Nil -> do
          form <- readCell formC
          case a of
            Symbol "quote"        -> do
              writeCharacter '\''
              print form
            Symbol "quasiquote"   -> do
              writeCharacter '`'
              print form
            Symbol "comma"        -> do
              writeCharacter ','
              print form
            Symbol "comma-splice" -> do
              writeString ",@"
              print form
            _ -> printNormally
        _  -> printNormally
    _ -> printNormally
print Nil = writeString "()"
print (SpecialOperator _) = writeString "#<special operator>"
print (Symbol name) = writeString name
print (Char char) = writeString "#\\" >> writeCharacter char
