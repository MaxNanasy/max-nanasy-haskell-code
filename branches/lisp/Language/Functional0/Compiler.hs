{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

module Language.Functional.Compiler where

import           Language.Functional.Parser.Types hiding(Lambda, Apply)
import qualified Language.Functional.Parser.Types as PT

import Text.PrettyPrint.Leijen

import qualified Data.List as DL

import Prelude hiding(exp, length, replicate)

length :: [a] -> Integer
length = DL.genericLength

replicate :: Integer -> a -> [a]
replicate = DL.genericReplicate

type Value = Term
type Var   = Identifier

data Term = Var    Var
          | Apply  Term Term
          | Lambda Var  Term
            deriving(Show)

instance Pretty Term where
    pretty = pretty . (cast :: Term -> Expression)

class Castable a b where
    cast :: a -> b

instance Castable Term Expression where
    cast (Var    name  ) = JustID    name
    cast (Apply  f    x) = PT.Apply  (cast f) (cast x)
    cast (Lambda x    e) = PT.Lambda [x] (cast e)

type Environment = [(Var, Value)]

compileModule :: Declaration -> Environment
compileModule (ModuleDecl _ decls) = concatMap compileDecl decls

compileDecl :: Declaration -> [(Var, Value)]
compileDecl (DataDecl True  _name _args cons) = zipWith (compileCodataCons $ length cons) [0 .. length cons - 1] cons
compileDecl (DataDecl False _name _args cons) = undefined cons --[(name, compileData args cons)]
compileDecl (VariableDecl   name exp       ) = [(name, compileExp exp)]

compileCodataCons :: Integer -> Integer -> ConsDecl -> (Var, Value)
compileCodataCons total position (ConsDecl name args) = (name, multiLambda (map (ID False False [] . ('x':) . show) [0 .. length args - 1]) (multiLambda (replicate position (ID False False [] "_") ++ (ID False False [] "f") : replicate (total - position - 1) "_") (multiApply (Var "f") (map (Var . ('x' :) . show) [0 .. length args - 1]))))

compileData :: [Identifier] -> [ConsDecl] -> Value
compileData = undefined

unitTerm :: Term
unitTerm = Lambda "f" (Var "f")

compileExp :: Expression -> Value
compileExp (PT.Lambda names exp) = multiLambda names (compileExp exp)
compileExp (JustID name) = Var $ name
compileExp (PT.Apply f x) = Apply (compileExp f) (compileExp x)
compileExp (InfixApp f x y) = Apply (Apply (compileExp f) (compileExp x)) (compileExp y)
compileExp Unit = unitTerm

multiLambda :: [Var] -> Term -> Term
multiLambda = flip $ foldr Lambda

multiApply :: Term -> [Term] -> Term
multiApply = foldl Apply
