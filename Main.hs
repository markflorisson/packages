import System.Environment
import Control.Monad

import Pkgs.Main
import Pkgs.Repository

main :: IO ()
main = do
    args <- getArgs
    failWhen (length args /= 2) $
        "Expected a package name (of the package with the main function) " ++
        "and file name as arguments"
    let [pkgName, fname] = args
    print =<< failEither =<< runCompiler (runFile pkgName fname)
