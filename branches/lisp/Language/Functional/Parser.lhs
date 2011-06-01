> {-# OPTIONS_GHC -Wall #-}
>

Parts of this module implement a subset of Chapter 9 of the Haskell 98 Report.

> module Language.Functional.Parser(declaration) where
>
> import Language.Functional.Parser.Types
> import Language.Functional.Parser.Token
> 
> import Text.ParserCombinators.Parsec
>
> import Control.Monad
> 
> import Prelude hiding(exp)
> 

This is technically for Applicative Functors, but I like the way it looks and
am not using any AFs.

> {-(<*) :: Monad m => m a -> m b -> m a
> x <* y = do
>   x' <- x
>   y
>   return x'-}
>
> declaration, importDecl, moduleDecl, variableDecl, dataDecl :: CharParser st Declaration
> declaration = moduleDecl <|> importDecl <|> variableDecl <|> dataDecl <?> "declaration"
>
> moduleDecl = do
>   reserved "module"
>   name <- identifier
>   reserved "where"

No layout rule yet. :-(

>   decls <- braces $ sepBy declaration semi
>   return $ ModuleDecl name decls
> 
> importDecl = reserved "import" >> liftM ImportDecl identifier
>
> variableDecl = do

Case insensitive for now.

>   var <- identifier
>   reservedOp "="
>   exp <- expression
>   return $ VariableDecl var exp
>
> dataDecl = do
>   co <- (reserved "codata" >> return True) <|> (reserved "data" >> return False)
>   name <- identifier
>   args <- many identifier
>   reservedOp "="
>   cons <- sepBy consDecl (reserved "|")
>   return $ DataDecl co name args cons
>
> consDecl :: CharParser st ConsDecl
> consDecl = liftM2 ConsDecl identifier (many identifier)
>   
> expression, simpleExp,
>  lambdaExp, justIDExp :: CharParser st Expression 
> 

The distinction between simpleExp and expression is needed to avoid infinite
recursion.

> expression = liftM (foldl1 Apply) (many1 simpleExp) <?> "expression"
>
> simpleExp = parens expression
>         <|> lambdaExp
>         <|> justIDExp
>         <?> "simple expression"
> 
> justIDExp = liftM JustID identifier
> 
> lambdaExp = do
>   reservedOp "\\"
>   vars <- many identifier
>   reservedOp "->"
>   exp <- expression
>   return $ Lambda vars exp
> 
