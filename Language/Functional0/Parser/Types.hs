{-# OPTIONS_GHC -Wall #-}

module Language.Functional.Parser.Types where

import Text.PrettyPrint.Leijen

import Prelude hiding(exp)

hangDef, indentDef :: Doc -> Doc
indentDef = indent 4
hangDef   = hang   4

data Identifier = ID
    { isUpper    :: Bool
    , isOperator :: Bool
    , preNames   :: [String]
    , baseName   :: String
    }
                deriving(Show)

instance Pretty Identifier where
    pretty (ID _ _ names name) = hcat . punctuate dot . map pretty $ names ++ [name]

data ConsDecl = ConsDecl Identifier [Identifier]
                deriving(Show)

instance Pretty ConsDecl where
    pretty (ConsDecl name args) = sep $ map pretty (name:args)

data Expression = Lambda   [Identifier] Expression
                | Apply     Expression  Expression
                | JustID    Identifier
                | InfixApp  Expression  Expression Expression
                | Literal   Integer
--                | LSection  Expression  Expression
--                | RSection  Expression  Expression
                | Unit
                  deriving(Show)

instance Pretty Expression where
    pretty (Lambda   xs                   e  ) = parens $
                                                 pretty '\\'
                                             <+> (sep . map pretty) xs
                                             <+> pretty "->"
                                             <+> pretty e
                                                 ------
                                                 ------
    pretty (Apply    f x ) = prettyOutfixOp f
                         <+> prettyArgument x
                                                 ------
                                                 ------
    pretty (JustID   x                       ) = pretty x
                                                 ------
                                                 ------
    pretty (InfixApp f                    x y) = pretty x
                                             <+> prettyInfixOp f
                                             <+> pretty y
                                                 ------
                                                 ------
    pretty (Literal n)                         = pretty n
                                                 ------
                                                 ------
{-    pretty (LSection f                    x  ) = parens (pretty x        <+> prettyInfixOp f)
                                                 ------
    pretty (RSection f                    x  ) = parens (prettyInfixOp f <+> pretty x       )
                                                 ------
                                                 ------}
    pretty  Unit                               = pretty "()"
                                                 ------
                                                 ------

prettyInfixOp :: Expression -> Doc
prettyInfixOp (JustID f@(ID _ True _ _)) = pretty f
prettyInfixOp f                          = pretty '`'
                                        <> pretty f
                                        <> pretty '`'

prettyOutfixOp :: Expression -> Doc
prettyOutfixOp (JustID f@(ID _ True _ _)) = parens $ pretty f
prettyOutfixOp f                          = pretty f

prettyArgument :: Expression -> Doc
prettyArgument f@(Apply    {}) = parens $ pretty f
prettyArgument f@(InfixApp {}) = parens $ pretty f
prettyArgument f               = pretty f

data Declaration = DataDecl     Bool       Identifier    [Identifier] [ConsDecl]
                 | VariableDecl Identifier Expression
                 | ModuleDecl   Identifier [Declaration]
                 | ImportDecl   Identifier
                   deriving(Show)

instance Pretty Declaration where
    pretty = prettyDecl -- <> pretty ';'

prettyDecl :: Declaration -> Doc
prettyDecl (VariableDecl name exp) = pretty name
                                 <+> pretty '='
                                 <+> pretty exp
prettyDecl (DataDecl codata name args consDecls) = (pretty $ if codata then "codata" else "data")
                                               <+>  pretty name
                                               <+>  sep (map pretty args)
                                               <+>  pretty '='
                                               <+>  (align . cat) (punctuate (space <> pretty '|' <> space) (map pretty consDecls))
prettyDecl (ModuleDecl name decls) = pretty "module"
                                 <+> pretty name
                                 <+> pretty "where"
                              -- <$$> pretty '{'
                                <$$> indentDef (vcat $ map pretty decls)
                              -- <$$> pretty '}'

prettyDecl (ImportDecl name) = pretty "import"
                           <+> pretty name
