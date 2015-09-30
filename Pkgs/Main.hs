module Pkgs.Main where

import Control.Monad.Except
import Data.Either
import Text.Parsec (ParseError)

import Pkgs.Parser
import Pkgs.Syntax
import Pkgs.Pretty
import Pkgs.Repository
import Pkgs.TypeCheck
import Pkgs.Eval

type Compiler = ExceptT Error IO
data Error = ParsingError String | TypingError String | EvaluationError String

instance Show Error where
    show (ParsingError s)    = "Parsing error: " ++ s
    show (TypingError s)     = "Typing error: " ++ s
    show (EvaluationError s) = "Evaluation error: " ++ s

runCompiler :: Compiler a -> IO (Either Error a)
runCompiler = runExceptT

----------

-- | Parse, type-check and run a file, with pkgName as the name of the
-- package entry point (must have a 'main : Unit -> tau' function)
runFile :: PVName -> FilePath -> Compiler Expr
runFile pkgName filename
    = processFile emptyRepo filename >>= flip evalPkg pkgName

-- | Parse and typecheck a file
processFile :: Repo -> FilePath -> Compiler Repo
processFile repo fname
    = do contents <- liftIO $ readFile fname
         defs <- parseFile fname contents
         repo' <- foldM check repo defs
         liftIO $ putStrLn $ "Successfully processed: " ++ fname
         return repo'

parseFile :: FilePath -> String -> Compiler [Toplevel]
parseFile fname contents
    = case parse parseAll fname contents of
        Left err   -> throwError $ ParsingError $ show err
        Right defs -> return defs

check :: Repo -> Toplevel -> Compiler Repo
check repo def
    = do result <- liftIO $ runTypeChecker repo $ checkToplevel def
         case result of
             Left err    -> throwError $ TypingError $ show err
             Right repo' -> return $ addToRepo repo def

evalPkg :: Repo -> String -> Compiler Expr
evalPkg repo pkgName
    | Just pkg <- findPkg repo pkgName
        = case runEvalM repo $ eval repo pkg of
            Left (EvalError s) -> throwError $ EvaluationError s
            Right expr         -> return expr
    | otherwise
        = throwError $ EvaluationError $
            "Cannot find package " ++ pkgName ++ " in repo " ++ show repo

----------------

failWhen :: Bool -> String -> IO ()
failWhen True s = fail s
failWhen _    s = return ()

failEither :: Show a => Either a b -> IO b
failEither x = failWhen (isLeft x) (show $ getLeft x) >> return (getRight x)

getLeft  (Left x)  = x
getRight (Right y) = y
