-- --------------------------------------------------------------- [ Paths.idr ]
-- Module    : Test.System.Posix.Paths
-- Copyright : (c) 2017 Josh Filstrup
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Paths

import System.Posix.Paths

assertEq : (Show a, Show b, Eq b) => (id : String) -> (fn : a -> b) 
           -> (input : a) -> (expected : b) -> IO Bool 
assertEq id f input expected = let res = f input in
                                  if res == expected 
                                    then do putStrLn ("Test " ++ id ++ " passed!")
                                            pure True                            
                                    else do putStrLn ("Test " ++ id ++ " failed!")
                                            putStrLn ("\t Expected: " ++ (show expected))
                                            putStrLn ("\t Actual: " ++ (show res))
                                            putStrLn ("\t Input: " ++ (show input))
                                            pure False

export
test_Normalization : IO ()
test_Normalization = do putStrLn "--------------------------------------------"
                        putStrLn "Running test_Normalization"
                        putStrLn "--------------------------------------------"

                        (assertEq 
                          "test_Normalization/tooManySlashes" 
                          Paths.normalize 
                          "//foo/bar//baz////billy/" 
                          "/foo/bar/baz/billy/")

                        (assertEq 
                          "test_Normalization/dotSlashPrefix" 
                          Paths.normalize 
                          "./hello/there" 
                          "hello/there")
                        
                        (assertEq 
                          "test_Normalization/doubleDots" 
                          Paths.normalize
                          "hello/../there" 
                          "there")

                        (assertEq 
                          "test_Normalization/manyDoubleDots" 
                          Paths.normalize
                          "hello/../there/billy/../" 
                          "there")

                        putStrLn "--------------------------------------------"
                        putStrLn "Finished test_Normalization."
                        putStrLn "--------------------------------------------"

