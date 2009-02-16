module Language.Pascalish.Parser.Types where

import Text.PrettyPrint.Leijen

type Id = String

prettyPro :: [VarDecl] -> [ProcDecl] -> [Statement] -> Doc
prettyPro vDecls pDecls stmts = (vsep . map pretty) vDecls
                            <$> (vsep . map pretty) pDecls
                             <> pretty "begin"
                            <$> indent 2 ((vsep . map pretty) stmts)
                            <$> pretty "end"

data PascalishProgram = PascalishProgram Id [VarDecl] [ProcDecl] [Statement] deriving(Show)
instance Pretty PascalishProgram where
    pretty (PascalishProgram name vDecls pDecls stmts) = pretty "program" <+> pretty name
                                                     <$> indent 2 (prettyPro vDecls pDecls stmts)

data VarDecl          = VarDecl [Id] Type                                    deriving(Show)
instance Pretty VarDecl where
    pretty (VarDecl names typ) = pretty "var" <+> (hcat . punctuate (comma <> space)) (map pretty names) <+> colon <+> pretty typ <> semi

data ProcDecl         = ProcDecl Id [VarDecl] [Statement]                    deriving(Show)
instance Pretty ProcDecl where
    pretty (ProcDecl name vDecls stmts) = pretty "proc" <+> pretty name
                                      <$> indent 2 (prettyPro vDecls [] stmts)

data Statement        = Read     Id
                      | Write    IdOrNumber
                      | ProcCall Id
                      | Assign   Id         Expr                             deriving(Show)
instance Pretty Statement where
    pretty (Read name) = pretty "read" <+> pretty name <> semi

data IdOrNumber       = Id     Id
                      | Number Integer                                       deriving(Show)

data Type             = PInteger | PShort | PChar | AnyType                  deriving(Show)
instance Pretty Type where
    pretty PInteger = pretty "integer"
    pretty PShort   = pretty "short"
    pretty PChar    = pretty "char"
    pretty AnyType  = pretty "anytype"

data Expr             = CompoundExpr Operator IdOrNumber Expr
                      | SimpleExpr IdOrNumber                                deriving(Show)
data Operator         = Add | Sub | Mul | Div                                deriving(Show)
