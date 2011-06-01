> {-# OPTIONS_GHC -Wall #-}
>
> module Language.Functional.Parser.Types where
>
> import Text.PrettyPrint.Leijen
>
> import Prelude hiding(exp)
>

Indent the default amount.

> indentDef :: Doc -> Doc
> indentDef = indent 4
> 

This may become far more complicated.

> type Identifier = String
>

Lambda Calculus expressions augmented with Haskell Integer naturals.

> data Expression = 

A lambda abstraction of multiple variables.

>                   Lambda [Identifier] Expression

A function application of one argument.  Multiple arguments coagulate.

>                 | Apply    Expression  Expression

A variable reference.

>                 | JustID   Identifier
>                   deriving(Show)
>
> instance Pretty Expression where
>     pretty (Lambda  xs exp) = 

Parentheses are required for disambiguation.

>                               parens $
>                               pretty '\\'
>                           <+> (sep . map pretty) xs
>                           <+> pretty "->"
>                           <+> pretty exp
>     pretty (Apply   f  x  ) = pretty f
>                           <+> prettyArgument x
>         where

Parentheses may be required for disambiguation.

>           prettyArgument y@(Apply {}) = parens $ pretty y
>           prettyArgument y            = pretty y
>     pretty (JustID  x     ) = pretty x
>

A definition by the program; the top level must always be a ModuleDecl.

> data Declaration =
>                    ModuleDecl   Identifier [Declaration]
>                  | ImportDecl   Identifier
>                  | DataDecl { codata       ::     Bool
>                             , dataName     ::  Identifier
>                             , dataArgs     :: [Identifier]
>                             , constructors ::  [ConsDecl]
>                             }

No explicit functions, which are unneccessary without pattern-matching anyway.

>                  | VariableDecl Identifier  Expression
>                    deriving(Show)
>

A data constructor declaration.

> data ConsDecl = ConsDecl Identifier [Identifier] deriving(Show)

There's no reason why printing can't use the layout rule. :-)

> instance Pretty Declaration where
>     pretty (ModuleDecl   name decls          ) = pretty "module" <+> pretty name <+> pretty "where"
>                                             <$$> (indentDef . vcat . map pretty) decls
>     pretty (ImportDecl   name                ) = pretty "import" <+> pretty name
>     pretty (VariableDecl var  exp            ) = pretty var <+> pretty '=' <+> pretty exp
>     pretty (DataDecl     co   name  args cons) = pretty (if co then "codata" else "data")
> 
