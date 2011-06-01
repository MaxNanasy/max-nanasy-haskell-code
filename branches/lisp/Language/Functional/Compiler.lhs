> {-# LANGUAGE FlexibleContexts #-}
> {-# OPTIONS_GHC -Wall #-}
> 
> module Language.Functional.Compiler(compile, runMain) where
>

I am being unradical in using inconsistent indentation.

> import Language.Functional.Compiler.Types
> import qualified Language.Functional.Parser.Types as PT
> import           Language.Functional.Parser.Types hiding(Apply, Lambda)
> 
> import Data.Function
> 
> import Data.Dynamic hiding(cast)
> 
> import Data.Maybe
> 
> import Prelude hiding(mod, exp)
> 

A module is compiled recursively by compiling it with its generated environment,
postfixed with the default environment.

> compile :: Declaration -> Module
> compile (ModuleDecl _name decls) = Module . fix $ \ env -> concatMap (compileDecl $ env ++ defaultEnv) decls
> compile _                        = error "Compilation is for modules only"
> 
> compileDecl :: Environment -> Declaration -> Environment
> compileDecl env (VariableDecl name exp) = [(name, compileTerm env $ cast exp)]
> compileDecl _   _                       = []
> 
> runMain :: Typeable a => Module -> a
> runMain (Module env) = unHV . fromJust $ lookup "main" env
> 

The default environment contains primitives that could not be expressed in pure
Functional.

Proof that all naturals are constructible through this system:
0. constructible(0)                                -zero
1. V n : constructible(n) => constructible(S(n))   -inc
2. V n : constructible(n)                          -Induction(0, 1)

> defaultEnv :: Environment
> defaultEnv = [("inc"     , hvf  ((1 :: Integer) +)               )
>              ,("zero"    , hv   (0 :: Integer)                   )
>              ,("getChar" , hvb  getChar                          )
>              ,("putChar" , hvf  putChar                          )
>              ,("nextChar", hvf  (succ :: Char -> Char)           )
>              ,("bind"    , hvf' ((>>) :: IO () -> IO () -> IO ()))
>              ,("char0"   , hv   '\0'                             )
>              ]
>
> hv    :: Typeable a                           => a             -> Value
> hvf   :: (Typeable a, Typeable b)             => (a -> b)      -> Value
> hvf'  :: (Typeable a, Typeable b, Typeable c) => (a -> b -> c) -> Value
> hvb   :: Typeable a                           => IO a          -> Value
> unHV  :: Typeable a                           => Value         -> a
> unHVF :: (Typeable a, Typeable b)             => Value         -> (a -> b)
> 
> hv      = toDyn
> hvf   f = hv $ \ x -> hv $ f $ unHV x
> hvf'  f = hv $ \ x -> (hvf . f . unHV) x
> hvb   f = hv $ \ x -> hv $ ((>>=) :: IO a -> (a -> IO ()) -> IO ()) f $ unHVF x
> unHV    = flip fromDyn (error "Badly typed Functional expression :-(")
> unHVF f = \ x -> unHV $ unHV f $ hv x
>
> compileTerm :: Environment -> Term -> Value
> compileTerm  env (Apply   f   x  ) = unHV (compileTerm env f) (compileTerm env x)
> compileTerm  env (Var     var    ) = fromMaybe (error $ var ++ " is undefined") $ lookup var env
> compileTerm  env (Lambda  var exp) = hv $ \ x -> compileTerm ((var, x) : env) exp
>
