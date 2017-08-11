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

concatTestImpl : String -> (Maybe (Path t Directory)) 
                        -> (Maybe (Path Relative k)) -> String -> IO Bool
concatTestImpl id (Just left) (Just right) expected = let concatFn = Paths.concat left in
                                                        assertEq id (show . concatFn) right expected 
concatTestImpl id _ _ _ = do putStrLn ("Test " ++ id ++ " failed! (Invalid args)")
                             pure False

export
test_Concatenation : IO ()
test_Concatenation = do putStrLn "--------------------------------------------"
                        putStrLn "Running test_Concatenation"
                        putStrLn "--------------------------------------------"

                        (concatTestImpl
                          "test_Concatenation/absRelFile"
                          (mkAbsoluteDirectory "/foo/bar")
                          (mkRelativeFile "temp.txt")
                          "Absolute File: /foo/bar/temp.txt")

                        (concatTestImpl
                          "test_Concatenation/absRelDir"
                          (mkAbsoluteDirectory "/foo/bar")
                          (mkRelativeDirectory "temp")
                          "Absolute Directory: /foo/bar/temp")

                        (concatTestImpl
                          "test_Concatenation/relRelFile"
                          (mkRelativeDirectory "./foo/bar")
                          (mkRelativeFile "temp.txt")
                          "Relative File: foo/bar/temp.txt")

                        (concatTestImpl
                          "test_Concatenation/relRelDir"
                          (mkRelativeDirectory "./foo/bar")
                          (mkRelativeDirectory "temp")
                          "Relative Directory: foo/bar/temp")

                        (concatTestImpl
                          "test_Concatenation/normalize"
                          (mkRelativeDirectory "./foo///bar//baz")
                          (mkRelativeDirectory "temp/")
                          "Relative Directory: foo/bar/baz/temp")

                        putStrLn ""

export
test_MkRelativeFile : IO ()
test_MkRelativeFile = do putStrLn "--------------------------------------------"
                         putStrLn "Running test_MkRelativeFile"
                         putStrLn "--------------------------------------------"

                         (assertEq 
                           "test_MkRelativeFile/basic"
                           (show . Paths.mkRelativeFile)
                           "./foo/bar/temp.txt"
                           "Just Relative File: foo/bar/temp.txt")

                         (assertEq 
                           "test_MkRelativeFile/absolute"
                           (show . Paths.mkRelativeFile)
                           "/foo/bar/temp.txt"
                           "Nothing")

                         (assertEq 
                           "test_MkRelativeFile/normalized"
                           (show . Paths.mkRelativeFile)
                           "foo/bar///baz//temp.txt"
                           "Just Relative File: foo/bar/baz/temp.txt")

                         (assertEq 
                           "test_MkRelativeFile/relativeSpecifiers"
                           (show . Paths.mkRelativeFile)
                           "foo/bill/../bar/../baz/temp.txt"
                           "Just Relative File: foo/baz/temp.txt")

                         putStrLn ""
                         
export
test_MkRelativeDirectory : IO ()
test_MkRelativeDirectory = do putStrLn "--------------------------------------------"
                              putStrLn "Running test_MkRelativeDirectory"
                              putStrLn "--------------------------------------------"

                              (assertEq 
                                "test_MkRelativeDirectory/basic"
                                (show . Paths.mkRelativeDirectory)
                                "./foo/bar/"
                                "Just Relative Directory: foo/bar")

                              (assertEq 
                                "test_MkRelativeDirectory/absolute"
                                (show . Paths.mkRelativeDirectory)
                                "/foo/bar/"
                                "Nothing")

                              (assertEq 
                                "test_MkRelativeDirectory/normalized"
                                (show . Paths.mkRelativeDirectory)
                                "foo/bar///baz//"
                                "Just Relative Directory: foo/bar/baz")

                              (assertEq 
                                "test_MkRelativeDirectory/relativeSpecifiers"
                                (show . Paths.mkRelativeDirectory)
                                "foo/bill/../bar/../baz/"
                                "Just Relative Directory: foo/baz")

                              putStrLn ""
                         
export
test_MkAbsoluteFile : IO ()
test_MkAbsoluteFile = do putStrLn "--------------------------------------------"
                         putStrLn "Running test_MkAbsoluteFile"
                         putStrLn "--------------------------------------------"

                         (assertEq 
                           "test_MkAbsoluteDirectory/basic"
                           (show . Paths.mkAbsoluteFile)
                           "/foo/bar/baz.txt"
                           "Just Absolute File: /foo/bar/baz.txt")

                         (assertEq 
                           "test_MkAbsoluteDirectory/normalized"
                           (show . Paths.mkAbsoluteFile)
                           "/foo//bar/////baz.txt"
                           "Just Absolute File: /foo/bar/baz.txt")

                         (assertEq 
                           "test_MkAbsoluteDirectory/relative"
                           (show . Paths.mkAbsoluteFile)
                           "./foo/bar/baz.txt"
                           "Nothing")

                         (assertEq 
                           "test_MkAbsoluteDirectory/relativeSpecifiers"
                           (show . Paths.mkAbsoluteFile)
                           "/foo/../bar/../baz.txt"
                           "Nothing")

                         putStrLn ""
                         
export
test_MkAbsoluteDirectory : IO ()
test_MkAbsoluteDirectory = do putStrLn "--------------------------------------------"
                              putStrLn "Running test_MkAbsoluteDirectory"
                              putStrLn "--------------------------------------------"
                         
                              (assertEq 
                                "test_MkAbsoluteDirectory/basic"
                                (show . Paths.mkAbsoluteDirectory)
                                "/foo/bar/"
                                "Just Absolute Directory: /foo/bar")

                              (assertEq 
                                "test_MkAbsoluteDirectory/normalized"
                                (show . Paths.mkAbsoluteDirectory)
                                "/foo/bar///baz/"
                                "Just Absolute Directory: /foo/bar/baz")
                        
                              (assertEq 
                                "test_MkAbsoluteDirectory/normalized2"
                                (show . Paths.mkAbsoluteDirectory)
                                "/foo/../bar///baz/"
                                "Nothing")

                              (assertEq 
                                "test_MkAbsoluteDirectory/missingFrontSlash"
                                (show . Paths.mkAbsoluteDirectory)
                                "foo/bar/"
                                "Nothing")
                
                              (assertEq 
                                "test_MkAbsoluteDirectory/tildeExists"
                                (show . Paths.mkAbsoluteDirectory)
                                "~/foo/bar/"
                                "Nothing")

                              putStrLn ""

export
test_Normalization : IO ()
test_Normalization = do putStrLn "--------------------------------------------"
                        putStrLn "Running test_Normalization"
                        putStrLn "--------------------------------------------"

                        (assertEq 
                          "test_Normalization/tooManySlashes" 
                          (Paths.normalize Absolute) 
                          "//foo/bar//baz////billy/"
                          "/foo/bar/baz/billy")

                        (assertEq 
                          "test_Normalization/dotSlashPrefix" 
                          (Paths.normalize Relative) 
                          "./hello/there"
                          "hello/there")
                        
                        (assertEq 
                          "test_Normalization/doubleDots" 
                          (Paths.normalize Relative)
                          "hello/../there" 
                          "there")

                        (assertEq 
                          "test_Normalization/manyDoubleDots" 
                          (Paths.normalize Relative)
                          "hello/../there/billy/../" 
                          "there")

                        putStrLn ""
