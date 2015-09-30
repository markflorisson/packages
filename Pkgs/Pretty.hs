{-# LANGUAGE OverloadedStrings #-}

module Pkgs.Pretty where

import Data.List
import Text.PrettyPrint

import Pkgs.Syntax

instance Show Toplevel where
    show = pretty

instance Show PkgTerm where
    show = render . prettyPkg

instance Show IfaceTerm where
    show = render . prettyIface

instance Show Expr where
    show = render . prettyExpr

instance Show Type where
    show = render . prettyType


pretty :: Toplevel -> String
pretty (P pkg)   = show pkg
pretty (I iface) = show iface

prettyPkg :: PkgTerm -> Doc
prettyPkg (Pkg pName args iName (PBody imports defs))
    = let pkg = "pkg" <+> text pName <+> prettyArgs args <+>
                "impl" <+> text iName
          body = vcat $ bodyList $ map prettyImport imports ++ map prettyDef defs
      in  vcat [ pkg <+> "{", nest 4 body, "}" ]

prettyIface :: IfaceTerm -> Doc
prettyIface (Iface iName args subtype (IBody decls))
    = let iface = "iface" <+> text iName <> parens (prettyArgs args) <+>
                  prettySubtype subtype
          body = vcat $ bodyList $ map prettyDecl decls
      in  vcat [ iface <+> "{", nest 4 body, "}" ]

prettyArgs :: [PArg] -> Doc
prettyArgs = hjoin "," . map prettyArg

prettyArg :: PArg -> Doc
prettyArg (PArg argName iexpr) = text argName <+> ":" <+> prettyIExpr iexpr

prettyImport :: PImport -> Doc
prettyImport (PImport pName iName pExpr)
    = "import" <+> text pName <+> ":" <+> text iName <+>
      "as" <+> prettyPExpr pExpr

prettyDef :: Def -> Doc
prettyDef (TypeDef tName t)
    = "type" <+> text tName <+> "=" <+> prettyType t
prettyDef (FunDef f t e)
    = "fun" <+> text f <+> ":" <+> prettyType t <+> "=" <+> prettyExpr e

prettyDecl :: Decl -> Doc
prettyDecl (TypeDecl tName) = "type" <+> text tName
prettyDecl (FunDecl f t)    = "fun" <+> text f <+> ":" <+> prettyType t

prettyPExpr (PExpr pName args)
    = text pName <> parens (argList (map prettyExpr args))
prettyIExpr (IExpr ivName args)
    = text ivName <> parens (argList (map text args))

prettySubtype :: Maybe IVName -> Doc
prettySubtype (Just iName) = "<:" <+> text iName <+> ""
prettySubtype Nothing      = ""

------------------------------
-- Core-language pretty printers

prettyExpr :: Expr -> Doc
prettyExpr (Lam x t e)   = "λ" <> text x <> ":" <> prettyType t <> "." <> prettyExpr e
prettyExpr (App e1 e2)   = parens (prettyExpr e1 <+> prettyExpr e2)
prettyExpr (Plus e1 e2)  = parens (prettyExpr e1 <+> "+" <+> prettyExpr e2)
prettyExpr (Proj p f)    = prettyExpr p <> "." <> text f
prettyExpr (Var x)       = text x
prettyExpr (IntVal x)    = int x
prettyExpr Unit          = "()"
prettyExpr (PkgVal imps defs)
    = braces (hcat $ bodyList $ map prettyImport imps ++ map prettyDef defs)

prettyType :: Type -> Doc
prettyType (Arrow t1 t2)  = parens (prettyType t1 <+> "->" <+> prettyType t2)
prettyType (TypeName t)   = text t
prettyType (ProjType p t) = text p <> "." <> text t
prettyType IntType        = "Int"
prettyType UnitType       = "Unit"
prettyType (PkgType tName decls)
    = text tName <> braces (hcat $ bodyList $ map prettyDecl decls)

------------------------------
-- Formatting helpers

argList :: [Doc] -> Doc
argList = hjoin ", "

bodyList :: [Doc] -> [Doc]
bodyList = map (<>";")

hjoin :: Doc -> [Doc] -> Doc
hjoin seperator = vcat . intersperse seperator
