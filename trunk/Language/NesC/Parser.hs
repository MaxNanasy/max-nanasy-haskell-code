module Language.NesC.Parser where

import Language.NesC.Types

import Language.NesC.Parser.Token
import Text.ParserCombinators.Parsec

import Control.Monad

type NesCParser = Parser

configuration :: NesCParser Configuration
configuration = do
  sharpComments
  reserved "configuration"
  name <- identifier
  braces $ return ()
  reserved "implementation"
  (componentDeclarationss, wirings) <- liftM splitEithers $ braces $ many statement
  return $ Configuration name (concat componentDeclarationss) wirings

(<<) :: Monad m => m a -> m b -> m a
x << y = do
  x' <- x
  y
  return x'

splitEithers :: [Either a b] -> ([a], [b])
splitEithers = foldr splitEither ([], []) where
    splitEither (Left  x) (lefts, rights) = (x : lefts,     rights)
    splitEither (Right x) (lefts, rights) = (    lefts, x : rights)

statement :: NesCParser (Either [ComponentDeclaration] Wiring)
statement = ((liftM Left componentDeclarations) <|> (liftM Right wiring)) << semi

componentDeclarations :: NesCParser [ComponentDeclaration]
componentDeclarations = reserved "components" >> commaSep componentDeclaration

componentDeclaration :: NesCParser ComponentDeclaration
componentDeclaration = do
  name   <- componentImport
  asName <- (reserved "as" >> liftM Just identifier) <|> return Nothing
  return $ CD name asName

componentImport :: NesCParser ComponentImport
componentImport = liftM (flip CI Nothing) identifier
                  <|> do
                    reserved "new"
                    name      <- identifier
                    arguments <- parens $ commaSep expression
                    return $ CI name $ Just arguments

expression :: NesCParser String
expression = identifier <|> liftM show integer

wiring :: NesCParser Wiring
wiring = do
  c0 <- component
  rightArrow <- (reservedOp "->" >> return True) <|> (reservedOp "<-" >> return False)
  c1 <- component
  return $ if rightArrow then c0 :-> c1 else c1 :-> c0

component :: NesCParser Component
component = do
  name    <- identifier
  subName <- (dot >> liftM Just identifier) <|> return Nothing
  return $ Component name subName
