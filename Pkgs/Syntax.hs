module Pkgs.Syntax where

data Toplevel
    = P PkgTerm
    | I IfaceTerm

data PkgTerm
    = Pkg { pName :: PVName
          , pArgs :: [PArg]
          , pImplName :: IVName
          , pBody :: PBody
          }
    deriving (Eq)

data IfaceTerm
    = Iface { iName :: IVName
            , iArgs :: [PArg]
            , iSubType :: Maybe IVName
            , ibody :: IBody
            }
    deriving (Eq)

data PArg
    = PArg { pArgName :: PVar
           , pIExpr :: IExpr
           } deriving (Eq)

data PBody = PBody { pImports :: [PImport], pDefs :: [Def] } deriving (Eq)
data IBody = IBody [Decl] deriving (Eq)

data PImport
    = PImport PName IVName PExpr
    deriving (Eq)

data PExpr
    = PExpr PVName [Expr]
    deriving (Eq)

data IExpr
    = IExpr IVName [PVar]
    deriving (Eq)

data Def
    = TypeDef TyName Type
    | FunDef VarName Type Expr
    deriving (Eq)

data Decl
    = TypeDecl TyName
    | FunDecl VarName Type
    deriving (Eq)

data Expr
    = Lam VarName Type Expr
    | App Expr Expr
    | Plus Expr Expr
    | Proj Expr VarName
    | Var VarName
    | IntVal Int
    | Unit
    | PkgVal [PImport] [Def]
    deriving (Eq)

data Type
    = Arrow Type Type
    | TypeName TyName
    | ProjType PVar TyName
    | IntType
    | UnitType
    -- | Pi PVar Type              -- Package abstraction type
    | PkgType IVName [Decl]     -- Package type
    deriving (Eq)

type PVName = String
type PName  = String
type IVName = String
type TyName = String

type PVar = String
type VarName = String
