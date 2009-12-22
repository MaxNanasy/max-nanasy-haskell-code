module Language.Lisp (run) where

import Language.Lisp.Types
import           Language.Lisp.Monad hiding (writeString)
import qualified Language.Lisp.Monad as LLM
import Language.Lisp.Utility

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

{-type Index = Int

data CompiledForm = Literal             Object
                  | LexicalReference    Index
                  | GlobalReference     Object
                  | FunctionApplication CompiledForm [CompiledForm]

compileForm :: Environment -> Object -> Lisp CompiledForm
compileForm env symbol@(Symbol _name) = return $ LexicalReference $ undefined lookupSymbol env symbol
compileForm _env        (Cons _f _xs) = undefined-}

eval :: Environment -> OneParam
eval env symbol@(Symbol name)     = lookupSymbol env symbol >>= maybe (lstringToString name >>= error) readCell
eval env  _form@(Cons funC argsC) = do
  fun <- eval env =<< readCell funC
  args <- readCell argsC
  case fun of
    SpecialOperator (Idd operator _) -> operator env args
    Function        (Idd function _) -> function =<< mapLlist (Function (Idd (eval env) dummyID)) args
    Macro           (Idd macro    _) -> macro args >>= eval env
    _                                -> error "eval: Not a functional value."
{-                                      writeString "eval: "
                                      write fun
                                      writeString " is not a functional value in "
                                      write form
                                      writeString " ."-}
eval _          x                 = return x

quote :: Lisp Object
quote  = incrementIdCounter >>= return . SpecialOperator . Idd (\ _ (Cons xC _) -> readCell xC)

lambda :: Lisp Object
lambda = incrementIdCounter >>= return . SpecialOperator . Idd (\ env (Cons paramsC restC) -> do
                                                                  params <- readCell paramsC
                                                                  (params', rest) <- deconstructLlist params
                                                                  Cons bodyC _ <- readCell restC
                                                                  body <- readCell bodyC
                                                                  let f args = do
                                                                        (args', Nil) <- deconstructLlist args
                                                                        env' <- case rest of
                                                                                  Nil -> zipLlists params' args' >>= (`append` env)
                                                                                  _   -> cons rest args          >>= (`cons`   env)
                                                                        eval env' body
                                                                  liftM (Function . Idd f) incrementIdCounter)

set :: Lisp Object
set    = incrementIdCounter >>= return . SpecialOperator . Idd (\ env (Cons sC restC) -> do
                                                              s@(Symbol _name) <- readCell sC
                                                              Just xC <- lookupSymbol env s
                                                              Cons vC _ <- readCell restC
                                                              v <- readCell vC >>= eval env
                                                              writeCell xC v
                                                              return v)

withDynamicBindings :: ThreeParam
withDynamicBindings symbols values (Function (Idd f _)) = zipLlists symbols values >>= flip withAugmentedDynamicEnvironment (f Nil)
withDynamicBindings _       _      _                    = error "with-dynamic-bindings: Not a function."

dynamicValue :: OneParam
dynamicValue symbol@(Symbol name) = lookupSymbolDynamically symbol >>= maybe (lstringToString name >>= error) readCell
dynamicValue        x             = return x

defineSymbol :: TwoParam
defineSymbol symbol value = do
  envC <- getGlobalEnvironment
  cons symbol value >>= push envC
  return value

error :: String -> Lisp b
error message =
{-  stdErr <- dynamicValue =<< intern =<< stringToLstring "*standard-error*"
  writeString "ERROR: " stdErr >> writeString message stdErr >> writeChar (Char '\n') stdErr-}
  P.error message

car, cdr :: OneParam
car (Cons aC _ ) = readCell aC
car _x           = error "car: Not a cons."
cdr (Cons _  dC) = readCell dC
cdr _x           = error "cdr: Not a cons."

cons :: TwoParam
cons a d = liftM2 Cons (newCell a) (newCell d)

macroToFunction :: OneParam
macroToFunction (Macro f) = return $ Function f
macroToFunction _         = error "macro-function: Not a macro."

apply :: TwoParam
apply (Function (Idd f _)) args = f args
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
typeOf Function        {} = intern =<< stringToLstring "function"
typeOf Cons            {} = intern =<< stringToLstring "cons"
typeOf Nil                = intern =<< stringToLstring "nil"
typeOf SpecialOperator {} = intern =<< stringToLstring "special-operator"
typeOf Symbol          {} = intern =<< stringToLstring "symbol"
typeOf Macro           {} = intern =<< stringToLstring "macro"
typeOf Char            {} = intern =<< stringToLstring "char"
typeOf Stream          {} = intern =<< stringToLstring "stream"
typeOf (NewType t _)      = return t

eq :: TwoParam
x `eq` y = if x == y then intern =<< stringToLstring "true" else return Nil

ifFunction :: ThreeParam
ifFunction Nil (Function _)         (Function (Idd e _)) = e Nil
ifFunction _   (Function (Idd t _)) (Function _)         = t Nil
ifFunction _   _                    _                    = error "if-function: Not a function."

intern :: OneParam
intern name = do
  env <- getGlobalEnvironment >>= readCell
  Just tableC <- stringToLstring "*intern-table*" >>= lookupByName env
  table <- readCell tableC
  symbolCM <- lookupByName table name
  case symbolCM of
    Just symbolC -> readCell symbolC
    Nothing      -> cons (Symbol name) (Symbol name) >>= push tableC >> return (Symbol name)

symbolName :: OneParam
symbolName (Symbol name) = return name
symbolName _             = error "symbol-name: Not a symbol."

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
  envC <- getGlobalEnvironment
  internSymbol <- liftM Symbol $ stringToLstring "*intern-table*"
  internValue <- cons internSymbol internSymbol >>= flip cons Nil
  internEntry <- cons internSymbol internValue
  push envC internEntry
  addListToEnv envC ([ ("quote"         , quote                                      )
                     , ("lambda"        , lambda                                     )
                     , ("set"           , set                                        )
                     , ("macro"         , oneParam   "macro"          functionToMacro)
                     , ("car"           , oneParam   "car"            car            )
                     , ("cdr"           , oneParam   "cdr"            cdr            )
                     , ("cons"          , twoParam   "cons"           cons           )
                     , ("type-of"       , oneParam   "type-of"        typeOf         )
                     , ("new-type"      , twoParam   "new-type"       newType        )
                     , ("un-new-type"   , oneParam   "un-new-type"    unNewType      )
                     , ("eval"          , oneParam   "eval"           (eval Nil)     )
                     , ("macro-function", oneParam   "macro-function" macroToFunction)
                     , ("if-function"   , threeParam "if-function"   ifFunction      )
                     , ("apply"         , twoParam   "apply"         apply           )
                     , ("define-symbol" , twoParam   "define-symbol" defineSymbol    )
                     , ("dynamic-value" , oneParam   "dynamic-value" dynamicValue    )
                     , ("with-dynamic-bindings", threeParam   "with-dynamic-bindings" withDynamicBindings)
                     , ("write-char"    , twoParam   "write-char"    writeChar       )
                     , ("read-char"     , oneParam   "read-char"     readChar        )
                     , ("peek-char"     , oneParam   "peek-char"     peekChar        )
                     , ("write"         , twoParam   "write"         defaultWrite    )
                     , ("read"          , oneParam   "read"          defaultRead     )
                     , ("open-file"     , oneParam   "open-file"     openFile        )
                     , ("eq"            , twoParam   "eq"            eq              )
                     , ("call/cc"       , oneParam   "call/cc"       callWithCurrentContinuation)
                     , ("quit"          , zeroParam  "quit"          quit            )
                     , ("intern"        , oneParam   "intern"        intern          )
                     , ("symbol-name"   , oneParam   "symbol-name"   symbolName      )
                     ] ++ initialStreams stdIn stdOut stdErr)

listToLlist :: [Object] -> Lisp Object
listToLlist = foldM (flip cons) Nil . reverse

stringToLstring :: String -> Lisp Object
stringToLstring string = liftM2 NewType (return Nil) (listToLlist $ map Char string)

llistToList :: Object -> Lisp ([Object], Object)
llistToList (Cons xC xsC) = do
  x  <- readCell xC
  xs <- readCell xsC
  (list, dot) <- llistToList xs
  return (x : list, dot)
llistToList x             = return ([], x)

lstringToString :: Object -> Lisp String
lstringToString (NewType _ string) = liftM (map (\ (Char c) -> c) . fst) $ llistToList string
lstringToString _                  = error "lstringToString: Not a string."

addListToEnv :: Cell -> [(String, Lisp Object)] -> Lisp ()
addListToEnv envC = mapM_ (\ (name, valueM) -> do
                             symbol <- intern =<< stringToLstring name
                             value  <- valueM
                             entry  <- cons symbol value
                             push envC entry)

initialStreams :: Stream -> Stream -> Stream -> [(String, Lisp Object)]
initialStreams stdIn stdOut stdErr = [ ("*standard-input*" , return $ Stream stdIn )
                                     , ("*standard-output*", return $ Stream stdOut)
                                     , ("*standard-error*" , return $ Stream stdErr)
                                     ]

run :: Stream -> Stream -> Stream -> Stream -> IO ()
run replStream stdIn stdOut stdErr = do
  runLisp $ do
    initializeGlobalEnvironment stdIn stdOut stdErr
    repl $ Stream replStream
  return ()

repl :: Object -> Lisp Object
repl stream = forever $ read stream >>= eval Nil

peekChar :: OneParam
peekChar (Stream stream) = liftM (maybe Nil Char) $ peekCharacter stream
peekChar _               = error "peek-char: Not a stream."

readChar :: OneParam
readChar (Stream stream) = liftM (maybe Nil Char) $ readCharacter stream
readChar _               = error "read-char: Not a stream."

isWhitespace :: Char -> Bool
isWhitespace = (`elem` " \t\n")

read :: OneParam
read stream = do
  read' <- dynamicValue =<< intern =<< stringToLstring "read"
  case read' of
    Function (Idd f _) -> f =<< cons stream Nil
    _                  -> defaultRead stream

defaultRead :: OneParam
defaultRead stream = do
  cc <- readChar stream
  case cc of
    Nil -> return Nil
    Char c -> case c of
      '(' -> do
        readDelimitedList ')' '.' stream
      '#' -> do
           c' <- readChar stream
           case c' of
             Char '\\' -> readChar stream
             Char c''  -> error $ c'' : " RAPE"
             _         -> error $ "EOF RAPE"
      _ | isWhitespace      c -> read stream
        | isSymbolCharacter c -> do token <- readToken stream
                                    intern =<< stringToLstring (c : token)
        | otherwise           -> error $ "RAPE\nby #\\" ++ c : []
    _ -> undefined

skipWhitespace :: Object -> Lisp ()
skipWhitespace stream = do
  cc <- peekChar stream
  case cc of
    Char c | isWhitespace c -> readChar stream >> skipWhitespace stream
    _                       -> return ()

isSymbolCharacter :: Char -> Bool
isSymbolCharacter = (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "+-*/")

readToken :: Object -> Lisp String
readToken stream = do
  cc <- peekChar stream
  case cc of
    Char c | isSymbolCharacter c -> do readChar stream
                                       token <- readToken stream
                                       return $ c : token
    _                            -> return ""

readDelimitedList :: Char -> Char -> Object -> Lisp Object
readDelimitedList c d stream = do
  skipWhitespace stream
  Char c' <- peekChar stream
  if c' == c
    then do readChar stream
            return Nil
    else if c' == d
         then do readChar stream
                 Cons objectC _ <- readDelimitedList c d stream
                 readCell objectC
         else do object <- read stream
                 rest <- readDelimitedList c d stream
                 cons object rest

openFile :: OneParam
openFile name = liftIO . liftM Stream . flip SI.openFile ReadWriteMode =<< lstringToString name

lookupSymbolDynamically :: Object -> Lisp (Maybe Cell)
lookupSymbolDynamically symbol = do
  env <- getDynamicEnvironment
  lookupSymbol env symbol

writeChar :: TwoParam
writeChar ch@(Char c) (Stream stream)  = do
  writeCharacter stream c
  return ch
writeChar _           _                = error "write-char: Not a character or not a stream."

writeString :: TwoParam
writeString string (Stream stream) = lstringToString string >>= LLM.writeString stream >> return string
writeString _      _               = error "write-string: Not a stream."

write :: TwoParam
write x stream = do
  write' <- dynamicValue =<< intern =<< stringToLstring "write"
  case write' of
    Function (Idd f _) -> f =<< cons x =<< cons stream Nil
    _                  -> defaultWrite x stream

defaultWrite :: TwoParam
defaultWrite x stream = do
  case x of
    Function (Idd _ n) -> do
                   writeString' =<< stringToLstring "#<function "
                   writeString' =<< stringToLstring (show n)
                   writeChar' $ Char '>'
    Macro    (Idd _ n) -> do
                   writeString' =<< stringToLstring "#<macro "
                   writeString' =<< stringToLstring (show n)
                   writeChar' $ Char '>'
    SpecialOperator (Idd _ n) -> do
                   writeString' =<< stringToLstring "#<special-operator "
                   writeString' =<< stringToLstring (show n)
                   writeChar' $ Char '>'
    Cons aC dC -> do
           a <- readCell aC
           d <- readCell dC
           let writeNormally = do
                    (list, dot) <- deconstructLlist d
                    writeChar' $ Char '('
                    write' a
                    mapLlist (Function (Idd (\ z -> writeChar' (Char ' ') >> write' z) dummyID)) list
                    case dot of
                      Nil -> return ()
                      _   -> stringToLstring " . " >>= writeString' >> write' dot >> return ()
                    writeChar' $ Char ')'
           writeNormally
           {- case d of
             Cons formC nilC -> do
                     nil <- readCell nilC
                     case nil of
                       Nil -> do
                         form <- readCell formC
                         case a of
                           Symbol "quote"        -> do
                                  writeChar' $ Char '\''
                                  write' form
                           Symbol "quasiquote"   -> do
                                  writeChar' $ Char '`'
                                  write' form
                           Symbol "comma"        -> do
                                  writeChar' $ Char ','
                                  write' form
                           Symbol "comma-splice" -> do
                                  writeString' ",@"
                                  write' form
                           _ -> writeNormally >> return a
                       _  -> writeNormally
             _ -> writeNormally-}
    Nil           -> stringToLstring "()" >>= writeString' >> return Nil
    Symbol name   -> writeString' name >> return Nil
    Char char     -> stringToLstring "#\\" >>= writeString' >> writeChar' (Char char)
    Stream _      -> stringToLstring "#<stream>" >>= writeString' >> return Nil
    NewType t y   -> do
           writeChar' $ Char '@'
           writeChar' $ Char ' '
           write' t
           writeChar' $ Char ' '
           write' y
  return x where
      writeString' = flip writeString stream
      writeChar'   = flip writeChar   stream
      write'       = flip write       stream
