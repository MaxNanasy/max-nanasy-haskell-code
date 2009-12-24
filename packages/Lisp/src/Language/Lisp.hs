module Language.Lisp (run) where

import Language.Lisp.Types
import Language.Lisp.Monad
import Language.Lisp.Utility
import Language.Lisp.Symbol
import Language.Lisp.List
import Language.Lisp.Input
import Language.Lisp.Output

import           Prelude hiding (read, error)
import qualified Prelude as P

import Control.Monad.Trans
import Control.Monad

import Data.List
import Data.Maybe

import           System.IO hiding (openFile)
import qualified System.IO as SI
import System.Exit

dummyID :: Identifier
dummyID = -1

eval :: Environment -> OneParam
eval env symbol@(Symbol (Idd name _)) = lookupSymbolLexically env symbol >>= maybe (error name) readCell
eval env  _form@(Cons funC argsC)     = do
  fun <- eval env =<< readCell funC
  (args, Nil) <- readCell argsC >>= llistToList
  case fun of
    SpecialOperator (Idd operator _) -> operator env args
    Function        (Idd function _) -> function =<< mapM (eval env) args
    Macro           (Idd macro    _) -> macro args >>= eval env
    _                                -> error "eval: Not a functional value."
eval _          x                     = return x

quote :: Lisp Object
quote  = incrementIdCounter >>= return . SpecialOperator . Idd (\ _ [x] -> return x)

lambda :: Lisp Object
lambda = incrementIdCounter >>= return . SpecialOperator . Idd (\ env [params, body] -> do
                                                                  (params', dot) <- llistToList params
                                                                  let f args = do
                                                                        env' <- case dot of
                                                                                    Nil -> zipWithM (\ param arg -> newCell arg >>= \ argC -> return (param, argC)) params' args >>= return . (++ env)
                                                                                    _   -> listToLlist args >>= newCell >>= \ argsC -> return $ (dot, argsC) : env
                                                                        eval env' body
                                                                  liftM (Function . Idd f) incrementIdCounter)

set :: Lisp Object
set    = incrementIdCounter >>= return . SpecialOperator . Idd (\ env [s@(Symbol (Idd name _)), vF] -> case lookupSymbol env s of
                                                                                                       Just xC -> do
                                                                                                         v <- eval env vF
                                                                                                         writeCell xC v
                                                                                                         return v
                                                                                                       Nothing -> error name)

lookupSymbolDynamically :: Object -> Lisp (Maybe Cell)
lookupSymbolDynamically symbol = do
  env <- getDynamicEnvironment
  lookupSymbolLexically env symbol

withDynamicBindings :: ThreeParam
withDynamicBindings symbols values (Function (Idd f _)) = zipLlists symbols values >>= flip withAugmentedDynamicEnvironment (f Nil)
withDynamicBindings _       _      _                    = error "with-dynamic-bindings: Not a function."

dynamicValue :: OneParam
dynamicValue symbol@(Symbol (Idd name _)) = lookupSymbolDynamically symbol >>= maybe (error name) readCell
dynamicValue        x                     = return x

defineSymbol :: TwoParam
defineSymbol symbol value = do
  valueC <- newCell value
  augmentGlobalEnvironment [(symbol, valueC)]
  return value

error :: String -> Lisp b
error = P.error

car, cdr :: OneParam
car (Cons aC _ ) = readCell aC
car _x           = error "car: Not a cons."
cdr (Cons _  dC) = readCell dC
cdr _x           = error "cdr: Not a cons."

macroToFunction :: OneParam
macroToFunction (Macro f) = return $ Function f
macroToFunction _         = error "macro-function: Not a macro."

apply :: TwoParam
apply (Function (Idd f _)) args = do
  (args', Nil) <- llistToList args
  f args'
apply _                    _    = error "apply: Not a function."

functionToMacro :: OneParam
functionToMacro (Function f) = return $ Macro f
functionToMacro _            = error "macro: Not a function."

deconstructLlist :: Object -> Lisp (Object, Object)
deconstructLlist (Cons aC dC) = do
  a <- readCell aC
  d <- readCell dC
  (xs, x)  <- deconstructLlist d
  d' <- cons a xs
  return (d', x)
deconstructLlist x            = return (Nil, x)

newType :: TwoParam
newType = (return .) . NewType

unNewType :: OneParam
unNewType (NewType _ x) = return x
unNewType _             = error "un-new-type: Not a new-type."

typeOf :: OneParam
typeOf Function        {} = intern "function"
typeOf Cons            {} = intern "cons"
typeOf Nil                = intern "nil"
typeOf SpecialOperator {} = intern "special-operator"
typeOf Symbol          {} = intern "symbol"
typeOf Macro           {} = intern "macro"
typeOf Char            {} = intern "char"
typeOf Stream          {} = intern "stream"
typeOf String          {} = intern "string"
typeOf (NewType t _)      = return t

eq :: TwoParam
x `eq` y = if x == y then intern "true" else return Nil

ifFunction :: ThreeParam
ifFunction Nil (Function _)         (Function (Idd e _)) = e []
ifFunction _   (Function (Idd t _)) (Function _)         = t []
ifFunction _   _                    _                    = error "if-function: Not a function."

type ZeroParam = Lisp Object
zeroParam :: String -> ZeroParam -> Lisp Object
zeroParam fname f = incrementIdCounter >>=
                    return . Function . (Idd $ \ args -> case args of
                                                           Nil -> f
                                                           _   -> error $ fname ++ ": Expected 0 arguments; received more.")

type OneParam = Object -> Lisp Object
oneParam :: String -> OneParam -> Lisp Object
oneParam fname f = incrementIdCounter >>=
                   return . Function . (Idd $ \ args ->
                             case args of
                               Cons arg0C rest0C -> do
                                         rest0 <- readCell rest0C
                                         case rest0 of
                                           Nil -> do
                                             arg0 <- readCell arg0C
                                             f arg0
                                           _ -> error $ fname ++ ": Expected 1 argument; received more."
                               _ -> error $ fname ++ ": Expected 1 argument; received 0.")

type TwoParam = Object -> Object -> Lisp Object
twoParam :: String -> TwoParam -> Lisp Object
twoParam fname f = incrementIdCounter >>=
                   return . Function . (Idd $ \ args ->
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
                               _ -> error $ fname ++ ": Expected 2 arguments; received 0.")

type ThreeParam = Object -> Object -> Object -> Lisp Object
threeParam :: String -> ThreeParam -> Lisp Object
threeParam fname f = incrementIdCounter >>=
                     return . Function . (Idd $ \ args ->
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
                                            _ -> error $ fname ++ ": Expected 2 arguments; received 0.")

push :: Cell -> OneParam
push xsC x = do
  xs  <- readCell xsC
  xs' <- cons x xs
  writeCell xsC xs'
  return xs'

quit :: ZeroParam
quit = liftIO exitSuccess

initializeGlobalEnvironment :: Stream -> Stream -> Stream -> Lisp ()
initializeGlobalEnvironment stdIn stdOut stdErr = do
  addListToEnv       ([ ("quote"         , quote                                      )
                     , ("lambda"        , lambda                                     )
                     , ("set"           , set                                        )
                     , ("macro"         , oneParam   "macro"          functionToMacro)
                     , ("car"           , oneParam   "car"            car            )
                     , ("cdr"           , oneParam   "cdr"            cdr            )
                     , ("cons"          , twoParam   "cons"           cons           )
                     , ("type-of"       , oneParam   "type-of"        typeOf         )
                     , ("new-type"      , twoParam   "new-type"       newType        )
                     , ("un-new-type"   , oneParam   "un-new-type"    unNewType      )
                     , ("eval"          , oneParam   "eval"           (eval [])     )
                     , ("macro-function", oneParam   "macro-function" macroToFunction)
                     , ("if-function"   , threeParam "if-function"   ifFunction      )
                     , ("apply"         , twoParam   "apply"         apply           )
                     , ("define-symbol" , twoParam   "define-symbol" defineSymbol    )
                     , ("dynamic-value" , oneParam   "dynamic-value" dynamicValue    )
                     , ("with-dynamic-bindings", threeParam   "with-dynamic-bindings" withDynamicBindings)
--                     , ("write-char"    , twoParam   "write-char"    writeChar       )
--                     , ("read-char"     , oneParam   "read-char"     readChar        )
--                     , ("peek-char"     , oneParam   "peek-char"     peekChar        )
--                     , ("write"         , twoParam   "write"         write           )
--                     , ("read"          , oneParam   "read"          read            )
                     , ("open-file"     , oneParam   "open-file"     openFile        )
                     , ("eq"            , twoParam   "eq"            eq              )
                     , ("call/cc"       , oneParam   "call/cc"       callWithCurrentContinuation)
                     , ("quit"          , zeroParam  "quit"          quit            )
--                     , ("intern"        , oneParam   "intern"        intern          )
--                     , ("symbol-name"   , oneParam   "symbol-name"   symbolName      )
                     ] ++ initialStreams stdIn stdOut stdErr)

listToLlist :: [Object] -> Lisp Object
listToLlist = foldM (flip cons) Nil . reverse

addListToEnv :: [(String, Lisp Object)] -> Lisp ()
addListToEnv = augmentGlobalEnvironment <=< mapM (\ (name, valueM) -> do
                                                    symbol <- intern name
                                                    valueC <- valueM >>= newCell
                                                    return $ (symbol, valueC))

initialStreams :: Stream -> Stream -> Stream -> [(String, Lisp Object)]
initialStreams stdIn stdOut stdErr = [ ("*standard-input*" , return $ Stream stdIn )
                                     , ("*standard-output*", return $ Stream stdOut)
                                     , ("*standard-error*" , return $ Stream stdErr)
                                     ]

run :: Stream -> Stream -> Stream -> Stream -> IO ()
run replStream stdIn stdOut stdErr = do
  runLisp $ do
    initializeGlobalEnvironment stdIn stdOut stdErr
    repl replStream
  return ()

repl :: Stream -> Lisp Object
repl stream = forever $ read stream >>= eval []

openFile :: OneParam
openFile (String name) = liftIO $ liftM Stream $ flip SI.openFile ReadWriteMode name
