> {-# OPTIONS_GHC -Wall #-}
> 
> module Language.Functional.Parser.LexicalSyntax where
> 

This module implements a subset of the lexical syntax of section 9.2 of the Haskell 98 report.

> import Text.ParserCombinators.Parsec
> 
> moduleDecl = many (lexeme <|> whitespace)
> lexeme = qvarid <|> 
> 
