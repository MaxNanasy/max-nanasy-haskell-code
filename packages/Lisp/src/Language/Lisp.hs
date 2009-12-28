module Language.Lisp (run) where

import Language.Lisp.Monad
import Language.Lisp.Utility
import qualified Language.Lisp.Symbol as LSym
import Language.Lisp.List
import qualified Language.Lisp.Input  as LIn
import qualified Language.Lisp.Output as LOut
import Language.Lisp.Compile

import           Prelude hiding (read, error)
import qualified Prelude as P

import Control.Monad.Trans
import Control.Monad

import Data.List
import Data.Maybe

import           System.IO hiding (openFile)
import qualified System.IO as SI
import System.Exit

lookupSymbolDynamically :: Object -> Lisp (Maybe Cell)
lookupSymbolDynamically symbol = do
  env <- getDynamicEnvironment
  undefined lookupSymbolLexically env symbol

withDynamicBindings :: ThreeParam
withDynamicBindings symbols values (Function (Idd f _)) = do
  (symbolList, Nil) <- llistToList symbols
  (valueList , Nil) <- llistToList values
  valueCList <- mapM newCell valueList
  withAugmentedDynamicEnvironment (zip symbolList valueCList) (f [])
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

newType :: TwoParam
newType = (return .) . NewType

unNewType :: OneParam
unNewType (NewType _ x) = return x
unNewType _             = error "un-new-type: Not a new-type."

typeOf :: OneParam
typeOf Function        {} = LSym.intern "function"
typeOf Cons            {} = LSym.intern "cons"
typeOf Nil                = LSym.intern "nil"
typeOf SpecialOperator {} = LSym.intern "special-operator"
typeOf Symbol          {} = LSym.intern "symbol"
typeOf Macro           {} = LSym.intern "macro"
typeOf Char            {} = LSym.intern "char"
typeOf Stream          {} = LSym.intern "stream"
typeOf String          {} = LSym.intern "string"
typeOf (NewType t _)      = return t

listToString :: OneParam
listToString cs = do
  (chars, Nil) <- llistToList cs
  return $ String $ map (\ (Char c) -> c) chars

eq :: TwoParam
x `eq` y = if x == y then LSym.intern "true" else return Nil

ifFunction :: ThreeParam
ifFunction Nil (Function _)         (Function (Idd e _)) = e []
ifFunction _   (Function (Idd t _)) (Function _)         = t []
ifFunction _   _                    _                    = error "if-function: Not a function."

compile :: OneParam
compile x = liftM (String . show) $ compileForm [] x

type ZeroParam = Lisp Object
zeroParam :: String -> ZeroParam -> Lisp Object
zeroParam fname f = incrementIdCounter >>=
                    return . Function . (Idd $ \ args ->
                                             case args of
                                               []                  -> f
                                               _                   -> error $ fname ++ ": Expected 0 arguments; received " ++ show (length args) ++ ".")

type OneParam = Object -> Lisp Object
oneParam :: String -> OneParam -> Lisp Object
oneParam fname f = incrementIdCounter >>=
                   return . Function . (Idd $ \ args ->
                                            case args of
                                              [arg0]               -> f arg0
                                              _                    -> error $ fname ++ ": Expected 1 argument; received " ++ show (length args) ++ ".")

type TwoParam = Object -> Object -> Lisp Object
twoParam :: String -> TwoParam -> Lisp Object
twoParam fname f = incrementIdCounter >>=
                   return . Function . (Idd $ \ args ->
                                            case args of
                                              [arg0, arg1]         -> f arg0 arg1
                                              _                    -> error $ fname ++ ": Expected 2 arguments; received " ++ show (length args) ++ ".")

type ThreeParam = Object -> Object -> Object -> Lisp Object
threeParam :: String -> ThreeParam -> Lisp Object
threeParam fname f = incrementIdCounter >>=
                     return . Function . (Idd $ \ args ->
                                              case args of
                                                [arg0, arg1, arg2] -> f arg0 arg1 arg2
                                                _                  -> error $ fname ++ ": Expected 3 arguments; received " ++ show (length args) ++ ".")

quit :: ZeroParam
quit = liftIO exitSuccess

openFile :: OneParam
openFile (String name) = liftIO $ liftM Stream $ flip SI.openFile ReadWriteMode name
openFile _             = error "open-file: Not a string."

writeChar :: TwoParam
writeChar ch@(Char c) (Stream stream) = LOut.writeChar stream c >> return ch
writeChar    Char {}  _               = error "write-char: Not a char."
writeChar    _        _               = error "write-char: Not a stream."

write :: TwoParam
write x (Stream stream) = LOut.write stream x >> return x
write _ _               = error "write: Not a stream."

readChar :: OneParam
readChar (Stream stream) = liftM (maybe Nil Char) $ LIn.readChar stream
readChar _               = error "read-char: Not a stream."

peekChar :: OneParam
peekChar (Stream stream) = liftM (maybe Nil Char) $ LIn.peekChar stream
peekChar _               = error "peek-char: Not a stream."

read :: OneParam
read (Stream stream) = LIn.read stream
read _               = error "read: Not a stream."

intern :: OneParam
intern (String name) = LSym.intern name
intern _             = error "intern: Not a string."

symbolName :: OneParam
symbolName = return . String . LSym.symbolName

initializeGlobalEnvironment :: Stream -> Stream -> Stream -> Lisp ()
initializeGlobalEnvironment stdIn stdOut stdErr = do
  addListToEnv ([ ("quote"         , return $ SpecialOperator Quote                               )
                , ("lambda"        , return $ SpecialOperator Lambda                              )
                , ("set"           , return $ SpecialOperator Set                                 )
                , ("macro"         , oneParam   "macro"          functionToMacro)
                , ("car"           , oneParam   "car"            car            )
                , ("cdr"           , oneParam   "cdr"            cdr            )
                , ("cons"          , twoParam   "cons"           cons           )
                , ("type-of"       , oneParam   "type-of"        typeOf         )
                , ("new-type"      , twoParam   "new-type"       newType        )
                , ("un-new-type"   , oneParam   "un-new-type"    unNewType      )
                , ("macro-function", oneParam   "macro-function" macroToFunction)
                , ("if-function"   , threeParam "if-function"    ifFunction      )
                , ("apply"         , twoParam   "apply"          apply           )
                , ("define-symbol" , twoParam   "define-symbol"  defineSymbol    )
                , ("dynamic-value" , oneParam   "dynamic-value"  dynamicValue    )
                , ("with-dynamic-bindings", threeParam   "with-dynamic-bindings" withDynamicBindings)
                , ("write-char"    , twoParam   "write-char"     writeChar       )
                , ("read-char"     , oneParam   "read-char"      readChar        )
                , ("peek-char"     , oneParam   "peek-char"      peekChar        )
                , ("write"         , twoParam   "write"          write           )
                , ("default-read"  , oneParam   "default-read"   read            )
                , ("open-file"     , oneParam   "open-file"      openFile        )
                , ("eq"            , twoParam   "eq"             eq              )
                , ("call/cc"       , oneParam   "call/cc"        callWithCurrentContinuation)
                , ("quit"          , zeroParam  "quit"           quit            )
                , ("intern"        , oneParam   "intern"         intern          )
                , ("symbol-name"   , oneParam   "symbol-name"    symbolName      )
                , ("list-to-string", oneParam   "list-to-string" listToString    )
                , ("compile"       , oneParam   "compile"        compile         )
                ] ++ initialStreams stdIn stdOut stdErr)

listToLlist :: [Object] -> Lisp Object
listToLlist = foldM (flip cons) Nil . reverse

addListToEnv :: [(String, Lisp Object)] -> Lisp ()
addListToEnv = augmentGlobalEnvironment <=< mapM (\ (name, valueM) -> do
                                                    symbol <- LSym.intern name
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
repl stream = forever $ LIn.read stream >>= compileForm [] >>= liftIO . putStrLn . generateEvalString
