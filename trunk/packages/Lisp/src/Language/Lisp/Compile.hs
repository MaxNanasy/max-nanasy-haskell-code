module Language.Lisp.Compile (compileForm, CompiledCode, generateEvalString) where

import Language.Lisp.Monad hiding (Macro, SpecialOperator)
import Language.Lisp.Monad        (Macro, SpecialOperator)
import qualified Language.Lisp.Monad as LLM
import Language.Lisp.List
import Language.Lisp.Symbol

import Data.List

data CompiledCode = LiteralObject       Object
                  | LexicalReference    LexicalIdentifier
                  | GlobalReference     Object
                  | LexicalSet          LexicalIdentifier CompiledCode
                  | FunctionApplication CompiledCode      [CompiledCode]
                  | FunctionAbstraction [Object]          CompiledCode

instance Show CompiledCode where
    show (LiteralObject _) = "<<LITERAL>>"
    show (LexicalReference (Symbol (Idd name _)))   = concat ["<<LID "   , name, ">>"]
    show (GlobalReference  (Symbol (Idd name _)))   = concat ["<<GLOBAL ", name, ">>"]
    show (LexicalSet       (Symbol (Idd name _)) c) = concat ["<<SET (LID ", name, ") ", show c, ">>"]
    show (FunctionApplication c  cs)                = concat ["(", show c, " ", unwords $ map show cs, ")"]
    show (FunctionAbstraction xs c )                = concat ["\\ ", unwords $ map (show . symbolName) xs, " . ", show c]

generateEvalString :: CompiledCode -> String
generateEvalString (LexicalReference    (Symbol (Idd name _))      ) = concat ["(readCell ", name, ")"]
generateEvalString (LexicalSet          (Symbol (Idd name _)) c    ) = concat ["(", generateEvalString c, " >>= writeCell ", name]
generateEvalString (FunctionApplication fC                    argCs) = concat ["(", generateEvalString fC, " =<< sequence ", mapShowList generateEvalString argCs, ")"]
generateEvalString (FunctionAbstraction params                bodyC) = concat ["(Function (\\ ", mapShowList symbolName params, " -> ", generateEvalString bodyC, "))"]

mapShowList :: (a -> String) -> [a] -> String
mapShowList f xs = concat ["[", concat $ intersperse ", " $ map f xs, "]"]

data LexicalDatum = Reference       LexicalIdentifier
                  | Macro           Macro
                  | SpecialOperator SpecialOperator

type LexicalEnvironment = [(Object, LexicalDatum)]

type LexicalIdentifier = Object

objectToLexicalDatum :: Object -> Object -> LexicalDatum
objectToLexicalDatum _      (LLM.Macro           macro          ) = Macro           macro
objectToLexicalDatum _      (LLM.SpecialOperator specialOperator) = SpecialOperator specialOperator
objectToLexicalDatum symbol _                                     = Reference symbol

data EnvType = Global | Lexical

lookupSymbolLexically :: LexicalEnvironment -> Object -> Lisp (Maybe (LexicalDatum, EnvType))
lookupSymbolLexically env symbol = do
  case lookup symbol env of
    Nothing -> do
      gEnv <- getGlobalEnvironment
      case lookup symbol gEnv of
        Nothing -> return Nothing
        Just vC -> do
                 v <- readCell vC
                 return $ Just (objectToLexicalDatum symbol v, Global)
    Just vC -> return $ Just (vC, Lexical)

compileForm :: LexicalEnvironment -> Object -> Lisp CompiledCode
compileForm env symbol@(Symbol {}) = do
  xxx <- lookupSymbolLexically env symbol
  case xxx of
    Nothing                       -> return $ GlobalReference  symbol
    Just (Reference lid, Lexical) -> return $ LexicalReference lid
    Just (Reference lid, Global ) -> return $ GlobalReference  lid
    Just {}                       -> error "SYNTAX NOOOOOOOOOOOOOOO"
compileForm env (Cons funC argsC)  = do
  fun <- readCell funC
  (args, Nil) <- readCell argsC >>= llistToList
  let function = liftM2 FunctionApplication (compileForm env fun) (mapM (compileForm env) args)
  case fun of
    Symbol {} -> do
             xxx <- lookupSymbolLexically env fun
             case xxx of
               Nothing                      -> function
               Just (Macro macro       , _) -> undefined
               Just (SpecialOperator so, _) ->
                   case so of
                     Quote  -> return $ LiteralObject $ head args
                     Lambda ->
                         do
                           let [paramsL, body] = args
                           (params, Nil) <- llistToList paramsL
                           let env' = map (\ p -> (p, Reference p)) params ++ env
                           liftM (FunctionAbstraction params) $ compileForm env' body
                     Set    -> undefined
               Just (Reference       {}, _) -> function
    _         -> function
compileForm _   x                  = return $ LiteralObject x
