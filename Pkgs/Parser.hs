module Pkgs.Parser ( parse, parseAll, parsePkg, parseIface ) where

import Data.Maybe (isJust, fromMaybe)
import Data.Either (partitionEithers)
import Control.Monad
import Control.Applicative ((<$>), (<*>))

import Text.Parsec
import Text.Parsec.String
-- import Text.Parser.Char (char)
import Text.Parsec.Prim
import Text.Parsec.Combinator

import Pkgs.Syntax

----------------------------------------------------------------------
-- Parse Packages and Terms                                         --
----------------------------------------------------------------------

parseAll :: Parser [Toplevel]
parseAll = many1 (liftM P parsePkg <|> liftM I parseIface) <* eof

parsePkg :: Parser PkgTerm
parsePkg = Pkg <$> (stringWS "pkg" *> parsePVName)
               <*> parseArgs parseParam
               <*> parseImpl <*> parsePBody

parseArgs :: Parser a -> Parser [a]
parseArgs p = liftM (fromMaybe []) $ optionMaybe $ parens $ argList p

parseImpl :: Parser IVName
parseImpl  = stringWS "impl" *> parseIVName

-- parsePBody :: Parser PBody
-- parsePBody = braces $ liftM2 PBody (bodyList parseImport <* sep)
--                                    (bodyList parseDef)

parsePBody = braces $ do
    eithers <- bodyList (liftM Left parseImport <|> liftM Right parseDef)
    let (imports, defs) = partitionEithers eithers
    return $ PBody imports defs

parseImport :: Parser PImport
parseImport = PImport <$> (stringWS "import" >> parsePName)
                      <*> (charWS ':' *> parseIVName)
                      <*> (stringWS "as" *> parsePExpr)

parsePExpr :: Parser PExpr
parsePExpr = PExpr <$> parsePName <*> do args <- parseArgs parseName
                                         return $ map Var args

parseDef :: Parser Def
parseDef = parseTyDef <|> parseFunDef

parseTyDef = TypeDef <$> (stringWS "type" *> parseName)
                     <*> (stringWS "=" *> parseType)
parseFunDef = FunDef <$> (stringWS "fun" *> parseVarName)
                     <*> (stringWS ":" *> parseType)
                     <*> (stringWS "=" *> parseExpr)

parseExpr :: Parser Expr
parseExpr
    = do expr1 <- parseLam <|> parseVarOrProj <|>
                  parseIntVal <|> parseUnit <|> parens parseExpr
         appExpr <- optionMaybe parseExpr
         addExpr <- optionMaybe (charWS '+' *> parseExpr)
         return $ case (appExpr, addExpr) of
             (Just expr2, Nothing) -> App expr1 expr2
             (Nothing, Just expr2) -> Plus expr1 expr2
             (Nothing, Nothing)    -> expr1

parseVarOrProj
    = do var <- parseVarName
         isDot <- liftM isJust $ optionMaybe $ char '.'
         if isDot
             then Proj (Var var) <$> parseVarName
             else return $ Var var

parseLam  = Lam <$> (charWS '\\' *> charWS '(' *> parseVarName)
                <*> (charWS ':' *> parseType <* charWS ')')
                <*> (charWS '.' *> parseExpr)
parseProj = Proj <$> (Var <$> parsePVar) <*> (charWS '.' >> parseVarName)
parseVar  = Var <$> parseVarName
parseIntVal = IntVal <$> int
parseUnit   = stringWS "unit" >> return Unit :: Parser Expr

----------------------------------------------------------------------
-- Parse Interfaces and Types                                       --
----------------------------------------------------------------------

parseIface :: Parser IfaceTerm
parseIface = Iface <$> (stringWS "iface" *> parseIVName)
                   <*> parseArgs parseParam
                   <*> parseSubType <*> parseIBody

parseParam :: Parser PArg
parseParam = PArg <$> (parseName <* stringWS ":") <*> parseIExpr

parseIExpr :: Parser IExpr
parseIExpr = IExpr <$> parseIVName <*> parseArgs parseName

parseSubType :: Parser (Maybe IVName)
parseSubType = optionMaybe (stringWS "<:" >> parseIVName)

parseIBody :: Parser IBody
parseIBody = IBody <$> braces (bodyList parseDecl)

parseDecl :: Parser Decl
parseDecl = parseTyDecl <|> parseFunDecl
parseTyDecl = TypeDecl <$> (stringWS "type" >> parseName)
parseFunDecl = FunDecl <$> (stringWS "fun" >> parseName)
                       <*> (charWS ':' >> parseType)

parseType :: Parser Type
parseType
    = do ty <- parseIntType <|> parseUnitType <|>
               parseTypeNameOrProj <|> parens parseType
         isArrow <- liftM isJust $ optionMaybe (stringWS "->")
         if isArrow
             then Arrow ty <$> parseType
             else return ty

parseTypeNameOrProj =
    do name <- parseName
       isDot <- liftM isJust $ optionMaybe $ char '.'
       if isDot
           then ProjType name <$> parseTyName
           else return $ TypeName name

parseTypeName  = TypeName <$> parseTyName
parseIntType   = stringWS "Int"  >> return IntType  :: Parser Type
parseUnitType  = stringWS "Unit" >> return UnitType :: Parser Type

----------------------------------------------------------------------
-- Naming and Versioning                                            --
----------------------------------------------------------------------

data Version = Version Int Int

instance Show Version where
    show (Version x y) = "v" ++ show x ++ "." ++ show y

parsePVName :: Parser IVName
parsePVName = do
    name <- many1 letter
    char '-'
    version <- parseVersion
    return $ name ++ "-" ++ show version

parseName :: Parser VarName
parseName
    = do l <- letter
         rest <- many (letter <|> digit)
         spaces
         return (l:rest)

parsePVar    = parseName
parseVarName = parseName
parseTyName  = parseName
parsePName   = parseName
parseIVName  = parsePVName

parseVersion :: Parser Version
parseVersion = do
    char 'v'
    x <- int
    y <- optionMaybe (charWS '.' >> int)
    return $ case y of
        Just y' -> Version x y'
        Nothing -> Version x 0

---------------------------
-- helpers

bodyList :: Parser a -> Parser [a]
bodyList p = p `sepBy` sep

sep = optional $ charWS ';'

enclosing :: Char -> Char -> Parser a -> Parser a
enclosing left right parse = charWS left *> parse <* charWS right

parens = enclosing '(' ')'
braces = enclosing '{' '}'

argList :: Parser a -> Parser [a]
argList p = p `sepBy` charWS ','

int :: Parser Int
int = read <$> (many1 digit <* spaces)

charWS :: Char -> Parser Char
charWS c = char c <* spaces

stringWS :: String -> Parser ()
stringWS s = string s >> spaces
