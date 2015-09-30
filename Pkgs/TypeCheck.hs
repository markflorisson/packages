module Pkgs.TypeCheck where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map as M
import Data.List (isPrefixOf)

import Pkgs.Syntax
import Pkgs.SyntaxHelpers
import Pkgs.Pretty
import Pkgs.Repository

-- | Type checker monad
type TypeChecker a = ExceptT TypeCheckError (StateT TCState IO) a

runTypeChecker repo = runTypeCheckerEnv repo emptyEnv

runTypeCheckerEnv :: Repo -> Env -> TypeChecker a -> IO (Either TypeCheckError a)
runTypeCheckerEnv repo env m = evalStateT (runExceptT m) (TCState repo env)

-- | Errors that can occur during type checking
data TypeCheckError
    = TypeError String
    | NameError VarName
    | AttrError PVar VarName
    | SubtypeError Type Type
    | NoSuchDependencyError PName IVName
    deriving (Eq)

instance Show TypeCheckError where
    show (TypeError s) = "TypeError: " ++ s
    show (NameError x) = "NameError: " ++ x
    show (AttrError p x) = "Package " ++ p ++ " has no attribute " ++ x
    show (SubtypeError s t) = show s ++ " is not a subtype of " ++ show t
    show (NoSuchDependencyError pName iName)
        = "Cannot import package " ++ pName ++ " through interface " ++ iName

----------------------------------------------------------------------
-- State                                                            --
----------------------------------------------------------------------

-- | State kept by the type checker monad
data TCState
    = TCState Repo Env

emptyState = TCState emptyRepo emptyEnv
emptyEnv = M.empty

resetEnv :: TCState -> TCState
resetEnv (TCState repo env) = TCState repo M.empty

-- | Run a computation with local state
localState :: (TCState -> TCState) -> TypeChecker a -> TypeChecker a
localState f computation
    = do s <- get
         put (f s)
         val <- computation
         put s
         return val

local :: TypeChecker a -> TypeChecker a
local = localState id

----------------------------------------------------------------------
-- Context                                                          --
----------------------------------------------------------------------

-- | Environment of bindings
type Env
    = M.Map VarName Type

addEnv :: VarName -> Type -> TypeChecker ()
addEnv x t = modifyEnv (M.insert x t)

lookupEnv :: VarName -> TypeChecker (Maybe Type)
lookupEnv x = do
    TCState r e <- get
    return $ M.lookup x e

getVar :: VarName -> TypeChecker Type
getVar x = lookupEnv x >>= getOrFail (NameError x)

modifyEnv :: (Env -> Env) -> TypeChecker ()
modifyEnv f = modify (\(TCState r e) -> TCState r (f e))

modifyRepo :: (Repo -> Repo) -> TypeChecker ()
modifyRepo f = modify (\(TCState r e) -> TCState (f r) e)

getRepo :: TypeChecker Repo
getRepo = get >>= \(TCState repo env) -> return repo

getOrFail :: TypeCheckError -> Maybe a -> TypeChecker a
getOrFail e (Just x) = return x
getOrFail e Nothing  = throwError e

----------------------------------------------------------------------
--  Packages and Terms                                              --
----------------------------------------------------------------------

checkToplevel :: Toplevel -> TypeChecker Type
checkToplevel (P p) = checkPkg p
checkToplevel (I i) = checkIface i

checkPkg :: PkgTerm -> TypeChecker Type
checkPkg (Pkg pName args iName body)
    = do mapM_ checkArg args
         decls <- checkPBody body
         repo <- getRepo
         ifaceType <- getIface repo iName
         let pkgType = PkgType iName decls
         ensureSubtype pkgType ifaceType
         return pkgType

checkPBody (PBody imports defs)
    = do mapM_ checkImport imports
         mapM_ checkDef defs
         return $ map defToDecl defs

checkImport :: PImport -> TypeChecker ()
checkImport (PImport pName iName pExpr@(PExpr pName' args))
    = do repo <- getRepo
         case matchPkg repo pName iName of
             Just pkgTerm -> checkPExpr iName pExpr >>= addEnv pName'
             Nothing      -> throwError $ NoSuchDependencyError pName iName

checkPExpr iName (PExpr pName args)
    = checkIExpr $ IExpr iName $ map (\(Var x) -> x) args

checkDef (TypeDef tName tau)
    = do addEnv tName tau
         checkTypeProj tau
checkDef (FunDef f tau e)
    = do checkTypeProj tau
         tau' <- checkExpr e
         equiv <- tyEquiv tau tau'
         unless equiv $
             throwError $ TypeError (show tau ++ " != " ++ show tau' ++
                                     " in function definition '" ++ f ++ "'")
         addEnv f tau'

tyEquiv :: Type -> Type -> TypeChecker Bool
tyEquiv t1 t2 = (==) <$> resolveType t1 <*> resolveType t2

resolveType :: Type -> TypeChecker Type
resolveType (Arrow t1 t2) = Arrow <$> resolveType t1 <*> resolveType t2
resolveType (TypeName tName)
    = do ty <- lookupEnv tName
         case ty of
             Just ty -> resolveType ty
             Nothing -> return $ TypeName tName
resolveType t = return t

checkExpr :: Expr -> TypeChecker Type
checkExpr (Lam x argType e)
    = local $ do
        addEnv x argType
        resType <- checkExpr e
        return $ Arrow argType resType
checkExpr (App e1 e2)
    = do funType <- checkExpr e1
         argType <- checkExpr e2
         case funType of
             Arrow t1 t2 -> checkEqual t1 argType >> return t2
             _           -> throwError
                (TypeError $ "Expected a function type, got " ++ show funType)
checkExpr (Plus e1 e2)
    = do checkExpr e1 >>= checkEqual IntType
         checkExpr e2 >>= checkEqual IntType
         return IntType
checkExpr (Proj (Var p) f)
    = do decl <- getDecl p f
         case decl of
             FunDecl f t -> return $ proj p t
             _           -> throwError $ AttrError p f
checkExpr (Var x)    = getVar x
checkExpr (IntVal x) = return IntType
checkExpr Unit       = return UnitType

-- | Prefix types from other packages with paths of their abstract data
-- type names:
--
-- pkg P impl IP { type SomeType ; fun f : Unit -> SomeType = ... }
-- pkg Q { import P : IP ; ... P.f ... }
--
-- Here we have (P.f : Unit -> P.SomeType)
proj :: PVar -> Type -> Type
proj p (t1 `Arrow` t2) = proj p t1 `Arrow` proj p t2
proj p (TypeName tName) = ProjType p tName
proj p t = t

----------------------------------------------------------------------
--  Interfaces and Types                                            --
----------------------------------------------------------------------

checkIface iface = checkIface' iface >>= \(pkgType, argTypes) -> return pkgType

checkIface' :: IfaceTerm -> TypeChecker (Type, [Type])
checkIface' (Iface iName params subtype (IBody decls))
    = do paramTypes <- mapM checkArg params
         mapM_ checkDecl decls
         case subtype of
             Just iName' -> checkIfaceSubtype iName' decls
             Nothing -> return ()
         return (PkgType iName decls, paramTypes)

checkIfaceSubtype iName decls
    = do repo <- getRepo
         let iface1 = PkgType iName decls
         iface2 <- getIface repo iName
         ensureSubtype iface1 iface2

checkArg :: PArg -> TypeChecker Type
checkArg (PArg p iExpr)
    = do ty <- checkIExpr iExpr
         addEnv p ty
         return ty

-- | Type-check an interface application, and return the resulting PkgType
checkIExpr :: IExpr -> TypeChecker Type
checkIExpr (IExpr iName argNames)
    = do -- Retrieve the interface definition and turn it into a PkgType
         iface@(Iface iName params subtype (IBody decls)) <- getIVName iName
         (pkgType, paramTypes) <- localState resetEnv $ checkIface' iface
         -- Get the argument types of the IExpr and match their types against
         -- the parameter types
         argTypes <- mapM getVar argNames
         zipWithM_ ensureSubtype argTypes paramTypes
         -- Substitute paths from the arguments for the parameters
         let paramNames = map pArgName params
         when (length paramNames /= length argNames) $
             throwError $ TypeError (
                 "Interface " ++ iName ++ " expects " ++
                 show (length paramNames) ++ " argument, got " ++
                 show (length argNames))
         return $ parallelSubst paramNames argNames pkgType

-- | Perform parallel (hygienic) substitution
parallelSubst :: [PVar] -> [PVar] -> Type -> Type
parallelSubst paramNames argNames ty
    = -- make sure parameter and argument names do not overlap
      let subst = foldr substType
          paramNames' = map (++"'") paramNames
          ty' = subst ty $ zip paramNames paramNames'
       in subst ty' $ zip paramNames' argNames

checkDecl (TypeDecl tName) = return ()
checkDecl (FunDecl f tau)  = checkTypeProj tau

checkTypeProj :: Type -> TypeChecker ()
checkTypeProj (Arrow t1 t2) = checkTypeProj t1 >> checkTypeProj t2
checkTypeProj (ProjType p tName)
    = do PkgType iName pDecls <- getVar p
         decl <- getDecl p tName
         unless (isTypeDecl decl) (throwError $ AttrError p tName)
checkTypeProj t = return ()

getIface :: Repo -> IVName -> TypeChecker Type
getIface repo iName
    = do iface  <- getOrFail (NameError iName) (findIface repo iName)
         checkIface iface -- inefficient, use a cache for this

------- Subtyping

-- | Ensure that one type is a subtype of the other:
--
--      1. Check that an interface is a nominal subtype of the other
--      2. Check that bodies match up (in particular, check that projections
--         have the same path, i.e. P1.T == P2.T)
--
ensureSubtype :: Type -> Type -> TypeChecker ()
ensureSubtype t1 t2
    | PkgType iName1 decls1 <- t1, PkgType iName2 decls2 <- t2
        = do repo <- getRepo
             let nomSub  = isNomsub repo iName1 iName2
                 bodySub = decls2 `isPrefixOf` decls1 -- width subtyping
             unless (nomSub && bodySub) (throwError $ SubtypeError t1 t2)
    | t1 == t2                         = return ()
    | otherwise                        = throwError $ SubtypeError t1 t2

defToDecl :: Def -> Decl
defToDecl (TypeDef tName _) = TypeDecl tName
defToDecl (FunDef x t _)    = FunDecl x t

-- | Resolve an interface by name
getIVName :: IVName -> TypeChecker IfaceTerm
getIVName iName = do
    TCState repo env <- get
    getOrFail (NameError iName) (findIface repo iName)

------- Helper functions

checkEqual :: Type -> Type -> TypeChecker ()
checkEqual t1 t2
    = unless (t1 == t2) $
        throwError $ TypeError $ "Type " ++ show t1 ++ " != " ++ show t2

isTypeDecl (TypeDecl tName) = True
isTypeDecl _                = False

isFunDecl = not . isTypeDecl

getDecl :: PVar -> VarName -> TypeChecker Decl
getDecl p x
    = do PkgType iName pDecls <- getVar p
         case filter match pDecls of
             [decl] -> return decl
             _      -> throwError $ AttrError p x
    where
        match (FunDecl f tau)  = f == x
        match (TypeDecl tName) = tName == x
