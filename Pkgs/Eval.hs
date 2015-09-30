module Pkgs.Eval (EvalError(..), EvalM, runEvalM, eval) where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map as M
import Data.List (isPrefixOf)
import Data.Maybe
import Data.Foldable

import Pkgs.Syntax
import Pkgs.SyntaxHelpers
import Pkgs.Pretty
import Pkgs.Repository

data EvalError = EvalError String

type EvalM a = ExceptT EvalError (StateT Repo FreshM) a

runEvalM :: Repo -> EvalM a -> Either EvalError a
runEvalM repo = runFreshM . flip evalStateT repo . runExceptT

liftFresh :: FreshM a -> EvalM a
liftFresh = lift . lift

----------------------------

-- | Evaluate a package (must not be an abswtraction, i.e. takes no arguments
-- and has a main function)
eval :: Repo -> PkgTerm -> EvalM Expr
eval repo pkg
    = do pkgExpr <- evalPkg repo pkg []
         mainFunc <- findMain pkgExpr
         evalExpr $ App mainFunc Unit

-- | Package application with a list of package argument values
evalPkg :: Repo -> PkgTerm -> [Expr] -> EvalM Expr
evalPkg repo pkg args
    = do when (length args /= length (pArgs pkg)) $
             throwError $ EvalError "Need more arguments to eval function"
         let pkgExpr = PkgVal (pImports $ pBody pkg) (pDefs $ pBody pkg)
         let paramNames = map pArgName $ pArgs pkg
         pkgExpr' <- liftFresh $ foldrM substExpr pkgExpr $ zip paramNames args
         evalExpr pkgExpr'

-- | Evaluate core-level expressions and package values
evalExpr :: Expr -> EvalM Expr
evalExpr (App e1 e2)
     = do Lam x t e <- evalExpr e1
          v <- evalExpr e2
          e' <- liftFresh $ substExpr (x, v) e
          evalExpr e'
evalExpr (Plus e1 e2)
    = do e1' <- evalExpr e1
         e2' <- evalExpr e2
         case (e1', e2') of
             (IntVal x, IntVal y) -> return $ IntVal $ x + y
evalExpr (Proj (PkgVal _ defs) f)
    = let FunDef x t e = findFun f defs
       in evalExpr e
evalExpr (PkgVal (imp:imps) defs)
    -- Resolve import and substitute
    = do repo <- get
         subst <- processImport repo imp
         evalExpr =<< liftFresh (substExpr subst $ PkgVal imps defs)
evalExpr (PkgVal [] (TypeDef tName t:defs))
    -- Resolve remaining body and preserve definition
    = do PkgVal [] defs' <- evalExpr $ PkgVal [] defs
         return $ PkgVal [] (TypeDef tName t:defs)
evalExpr (PkgVal [] (FunDef f t e:defs))
    -- Substitute function body for function name in remaining definitions,
    -- and keep the definition
    = do expr <- liftFresh (substExpr (f, e) (PkgVal [] defs))
         PkgVal [] defs' <- evalExpr expr
         return $ PkgVal [] (FunDef f t e:defs')
evalExpr val
    -- Value, nothing to do
    = return val

-- | Evaluate an import statement to a substitution:
--
--  import P : I as X(Y) generates [ X |-> { .. pkg val .. } ]
processImport :: Repo -> PImport -> EvalM SubstExpr
processImport repo (PImport p i (PExpr x args))
    = case matchPkg repo p i of
          Nothing  -> throwError $ EvalError $ "Package " ++ p ++ " not found"
          Just pkg -> do pkgExpr <- evalPkg repo pkg args
                         return (x, pkgExpr)

----------------------
-- Resolve Definitions

findFun :: String -> [Def] -> Def
findFun f (FunDef f' t e:defs)
    | f == f'   = FunDef f t e
    | otherwise = findFun f defs
findFun f (def:defs) = findFun f defs

findMain :: Expr -> EvalM Expr
findMain (PkgVal imps defs)
    | FunDef main tau e <- findFun "main" defs
    , Arrow UnitType t <- tau
        = return e
    | otherwise
        = throwError $ EvalError "Expected main function"
