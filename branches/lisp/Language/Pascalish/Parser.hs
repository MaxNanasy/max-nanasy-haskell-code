module Language.Pascalish.Parser(pascalishProgram) where

import Language.Pascalish.Parser.Types

import Language.Pascalish.Parser.Token
import Text.ParserCombinators.Parsec

import Control.Monad

import Prelude hiding(id, read)

type PascalishParser = Parser

pascalishProgram :: PascalishParser PascalishProgram
pascalishProgram = do
  reserved "program"
  name   <- id
  vDecls <- varDecls
  pDecls <- procDecls
  reserved "begin"
  stmts  <- statementList
  reserved "end"
  return $ PascalishProgram name vDecls pDecls stmts
  
varDecls = many varDecl
varDecl :: PascalishParser VarDecl
varDecl = do
  reserved "var"
  ids <- idList
  colon
  typ <- pType
  semi
  return $ VarDecl ids typ

id :: PascalishParser Id
id = identifier

idList :: PascalishParser [Id]
idList = commaSep id

pType :: PascalishParser Type
pType = (reserved "integer" >> return PInteger)
    <|> (reserved "short"   >> return PShort  )
    <|> (reserved "char"    >> return PChar   )
    <|> (reserved "anytype" >> return AnyType )

procDecls = many procDecl
procDecl :: PascalishParser ProcDecl
procDecl = do
  reserved "procedure"
  name   <- id
  vDecls <- varDecls
  reserved "begin"
  stmts  <- statementList
  reserved "end"
  return $ ProcDecl name vDecls stmts

idOrNumber :: PascalishParser IdOrNumber
idOrNumber = liftM Number integer
         <|> liftM Id     id

statementList = many statement
statement :: PascalishParser Statement
statement = do
  stmt <- read <|> write <|> procCall <|> assign
  semi
  return stmt

read, write, procCall, assign :: PascalishParser Statement

read = do
  reserved "read"
  liftM Read id

write = do
  reserved "write"
  liftM Write idOrNumber

procCall = do
  reserved "call"
  liftM ProcCall id

assign = do
  var <- id
  reservedOp "="
  liftM (Assign var) expr

expr :: PascalishParser Expr
expr = do
  ion <- idOrNumber
  liftM2 (flip CompoundExpr ion) operator expr <|> return (SimpleExpr ion)

operator :: PascalishParser Operator
operator = (reservedOp "+" >> return Add)
       <|> (reservedOp "-" >> return Sub)
       <|> (reservedOp "*" >> return Mul)
       <|> (reservedOp "/" >> return Div)
