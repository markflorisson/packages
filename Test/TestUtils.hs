module Test.TestUtils
(Test(..), success, failure, run)
where

import Control.Monad (void)

import Pkgs.Main
import Pkgs.Repository (emptyRepo)
import Pkgs.Syntax (Expr, PVName)

data Test = Success { name :: String, test :: Compiler () }
          | Failure { name :: String, test :: Compiler () }
          | Eval  { name :: String, testEval :: Compiler Expr }

success, failure :: String -> Test
success s = Success s $ void $ processFile emptyRepo s
failure s = Failure s $ void $ processFile emptyRepo s

run :: String -> PVName -> Test
run s pName = Eval s $ runFile pName s
