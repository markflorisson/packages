import Control.Monad

import Pkgs.Main

import Test.TestUtils
import Test.TypeCheck
import Test.Eval

runTest :: Test -> IO ()
runTest test
    = do putStrLn $ "Running: " ++ name test ++ " ..."
         runTest' test
         putStrLn "Success :)"

runTest' (Success s m) = void . failEither =<< runCompiler m
runTest' (Failure s m) = failSuccess =<< runCompiler m
    where failSuccess (Right x) = fail "Expected test failure!"
          failSuccess (Left x)  = putStrLn $ "Expecting error: " ++ show x
runTest' (Eval s m) = print =<< failEither =<< runCompiler m

tests = typeCheckTests ++ evalTests

main :: IO ()
main = mapM_ runTest tests
