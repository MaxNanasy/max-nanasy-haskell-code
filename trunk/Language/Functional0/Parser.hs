--{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module Language.Functional.Parser(declaration) where

import           Language.Functional.Parser.Types
import           Language.Functional.Parser.Token hiding(identifier)
import qualified Language.Functional.Parser.Token as PT(identifier)

import Text.ParserCombinators.Parsec

import Control.Monad

import Debug.Trace

to :: Monad m => m a -> m b -> m a
x `to` y = x >>= ((y >>) . return)

--stringParser :: 

identifier :: Bool -> CharParser st Identifier
identifier o = return ID `ap` return False
                         `ap` return o
                         `ap` many (try (uppercaseID `to` dot))
                         `ap` PT.identifier

lowercaseID, uppercaseID :: CharParser st String
uppercaseID = notFollowedBy lower >> PT.identifier
lowercaseID = notFollowedBy upper >> PT.identifier

moduleDecl :: CharParser st Declaration
moduleDecl = return ModuleDecl `ap` moduleHeader
                               `ap` (braces $ many declaration)

moduleHeader :: CharParser st Identifier
moduleHeader = reserved "module" >> identifier False `to` reserved "where" <?> "module header"

declaration :: CharParser st Declaration
declaration = (moduleDecl <|> importDecl <|> dataDecl <|> variableDecl ) `to` semi <?> "declaration"

importDecl :: CharParser st Declaration
importDecl = liftM ImportDecl (reserved "import" >> identifier False)

dataDecl, variableDecl :: CharParser st Declaration
dataDecl =
  return DataDecl `ap` ((reserved "codata" >> return True ) <|>
                        (reserved   "data" >> return False))
                  `ap` identifier False
                  `ap` many (identifier False)
                  `ap` (reservedOp "=" >> consDecl `sepBy` reservedOp "|")

consDecl :: CharParser st ConsDecl
consDecl = return ConsDecl `ap` identifier False `ap` many (identifier False)

variableDecl = return VariableDecl `ap` identifier False `ap` (reservedOp "=" >> expression)

expression, simpleExp, parensExp :: CharParser st Expression
expression = try infixAppExp
         <|> multiApplyExp
         <?> "expression"

simpleExp = lambdaExp
        <|> justIDExp
        <|> parensExp
        <|> constantExp
        <?> "simple expression"

constantExp :: CharParser st Expression
constantExp = liftM Literal natural

parensExp = parens (unitExp <|> {- sectionExp <|> -} expression) <?> "parenthesized expression"

unitExp :: CharParser st Expression
unitExp = notFollowedBy (noneOf ")") >> return Unit <?> "unit expression"

infixOpExp :: CharParser st Expression
infixOpExp = between (symbol "`") (symbol "`") expression
             <?> "infix operator expression"

infixAppExp :: CharParser st Expression
infixAppExp = (return . flip) InfixApp `ap` multiApplyExp `ap` infixOpExp `ap` expression
          <?> "infix application expression"

{-sectionExp, leftSectionExp, rightSectionExp :: CharParser st Expression
sectionExp = rightSectionExp <|>
             (do
               ls@(LSection f x) <- leftSectionExp
               liftM (InfixApp f x) multiApplyExp <|> (notFollowedBy (noneOf ")") >> return ls))

leftSectionExp  = return (flip LSection) `ap` multiApplyExp
                                         `ap` infixOpExp
              <?> "left section expression"
rightSectionExp = return       RSection  `ap` infixOpExp
                                         `ap` multiApplyExp
              <?> "right section expression"-}

lambdaExp, justIDExp :: CharParser st Expression
lambdaExp = return Lambda `ap` (reservedOp "\\" >> many (identifier False))
                          `ap` (reservedOp "->" >> expression)
              <?> "lambda expression"
justIDExp = liftM JustID (identifier False) <?> "variable expression"

multiApplyExp :: CharParser st Expression
multiApplyExp = (liftM multiApply $ many1 simpleExp) <?> "multiple application expression"

multiApply :: [Expression] -> Expression
multiApply = foldl1 Apply
