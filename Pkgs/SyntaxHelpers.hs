{-# LANGUAGE TypeFamilies #-}

module Pkgs.SyntaxHelpers where

import Control.Monad.State

import qualified Data.Set as S
import qualified Data.Map as M

import Pkgs.Syntax

type FreshM = State Int

type SubstType = (PVar, PVar)
type SubstExpr = (VarName, Expr)

runFreshM :: FreshM a -> a
runFreshM m = evalState m 0

fresh :: VarName -> FreshM VarName
fresh x = modify (+1) >>= \count -> return (x ++ "_" ++ show count)

-- Substitution on types, only renames package variables PVar

-- | Substitute package variable names in interfaces
substType :: (PVar, PVar) -> Type -> Type
substType sub (Arrow t1 t2)
    = Arrow (substType sub t1) (substType sub t2)
substType (x, y) (ProjType z tyname)
    | x == z    = ProjType y tyname
    | otherwise = ProjType z tyname
substType sub (PkgType iName decls)
    = PkgType iName (map (substDecl sub) decls)
substType sub t = t

substDecl :: (PVar, PVar) -> Decl -> Decl
substDecl sub (FunDecl f tau) = FunDecl f (substType sub tau)
substDecl sub (TypeDecl t)    = TypeDecl t

-- | Substitution of expressions for package/core variables
substExpr :: SubstExpr -> Expr -> FreshM Expr
substExpr (x, e1) (Lam y t e2)
    -- avoid capture
    | x `S.member` free e2
        = if y `S.member` free e1
              then do y' <- fresh y
                      Lam y' t <$> (substExpr (x, e1) =<<
                                    substExpr (y, Var y') e2)
              else Lam y t <$> substExpr (x, e1) e2
    | otherwise
        = return $ Lam y t e2
substExpr sub (App e1 e2)
    = App <$> substExpr sub e1 <*> substExpr sub e2
substExpr sub (Plus e1 e2)
    = Plus <$> substExpr sub e1 <*> substExpr sub e2
substExpr sub (Proj e f)
    = Proj <$> substExpr sub e <*> pure f
substExpr sub (PkgVal imps defs)
    = PkgVal <$> mapM (substPImport sub) imps <*> mapM (substDef sub) defs
substExpr (x, e) (Var y)
    | x == y    = return e
    | otherwise = return $ Var y
substExpr sub e = return e

-- | Substitution on imports, which are a form of binding.
-- However, expression must be a (PkgVal [] defs), which does
-- not have free variables
substPImport :: SubstExpr -> PImport -> FreshM PImport
substPImport sub (PImport p i (PExpr x es))
    = PImport p i <$> (PExpr x <$> mapM (substExpr sub) es)

substDef :: (VarName, Expr) -> Def -> FreshM Def
substDef sub (FunDef x t e) = FunDef x t <$> substExpr sub e
substDef sub def = return def

class Free a where
    free :: a -> S.Set VarName

instance Free Expr where
    free (Lam x ty e) = S.delete x (free e)
    free (App e1 e2)  = free e1 `S.union` free e2
    free (Plus e1 e2) = free e1 `S.union` free e2
    free (Proj e f)   = free e
    free (Var x)      = S.singleton x
    free expr         = S.empty
