{-# LANGUAGE FlexibleContexts #-}

module Pkgs.Repository
( RepoM, runRepo, Repo, emptyRepo, addToRepo
, findPkg, findIface
, isNomsub, matchPkg
) where

import Control.Monad.State
import Data.List as L
import Data.Map as M

import Pkgs.Syntax
import Pkgs.Pretty

-- | Repository of packages
data Repo
    = Repo { pkgs :: M.Map PVName PkgTerm
           , interfaces :: M.Map IVName IfaceTerm
           , nomsub :: M.Map IVName IVName
           , impls :: M.Map PVName IVName
           }

instance Show Repo where
    show repo = show $ pkgs repo

-- | Monad for updating the repository
type RepoM a = State Repo a

emptyRepo = Repo M.empty M.empty M.empty M.empty

runRepo :: Repo -> RepoM () -> Repo
runRepo = flip execState

setPkgs a = modify (\(Repo _ b c d) -> Repo a b c d)
setInterfaces b = modify (\(Repo a _ c d) -> Repo a b c d)
setNomsub c = modify (\(Repo a b _ d) -> Repo a b c d)
setImpls d = modify (\(Repo a b c _) -> Repo a b c d)

addToRepo :: Repo -> Toplevel -> Repo
addToRepo repo (P p) = runRepo repo $ addPkg p
addToRepo repo (I i) = runRepo repo $ addInterface i

-- | Add a package to the repository
addPkg :: PkgTerm -> RepoM ()
addPkg pkg
    = do pkgList <- gets pkgs
         setPkgs $ M.insert (pName pkg) pkg pkgList
         addImpl (pName pkg) (pImplName pkg)

-- | Add an interface to the repository
addInterface :: IfaceTerm -> RepoM ()
addInterface iface
    = do interfaces <- gets interfaces
         setInterfaces $ M.insert (iName iface) iface interfaces
         case iSubType iface of
             Just ifaceSuper -> addNomsub (iName iface) ifaceSuper
             Nothing         -> return ()

addNomsub :: IVName -> IVName -> RepoM ()
addNomsub i1 i2
    = do curNomsub <- gets nomsub
         setNomsub (M.insert i1 i2 curNomsub)

addImpl :: PVName -> IVName -> RepoM ()
addImpl pName iName
    = do curImpls <- gets impls
         setImpls (M.insert pName iName curImpls)

------------------------------

-- | Lookup a package by name in the repository
findPkg :: Repo -> PVName -> Maybe PkgTerm
findPkg repo name = M.lookup name (pkgs repo)

-- | Lookup an interface by name in the repository
findIface :: Repo -> IVName -> Maybe IfaceTerm
findIface repo name = M.lookup name (interfaces repo)

-- | Check whether the first interface name is a nominal subtype
-- of the latter interface name
isNomsub :: Repo -> IVName -> IVName -> Bool
isNomsub repo n1 n2
    | n1 == n2  = True
    | Just n1' <- M.lookup n1 (nomsub repo)
        = isNomsub repo n1' n2
    | otherwise = False

-- | Try to find a package with name P implementing an interface I or some
-- subtype of I
matchPkg :: Repo -> PName -> IVName -> Maybe PkgTerm
matchPkg repo pname i1
    = matchPkg' (M.keys $ pkgs repo)
    where
        matchPkg' :: [PVName] -> Maybe PkgTerm
        matchPkg' (pvname:pnames)
            | pname `L.isPrefixOf` pvname
            , Just i2 <- M.lookup pvname (impls repo)
            , isNomsub repo i2 i1
                = M.lookup pvname (pkgs repo)
            | otherwise
                = matchPkg' pnames
        matchPkg' []
            = Nothing
