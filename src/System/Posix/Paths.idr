-- --------------------------------------------------------------- [ Paths.idr ]
-- Module    : System.Posix.Paths
-- Copyright : (c) 2017 Josh Filstrup
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Paths

import Data.Vect

public export
data Kind = Directory | File

public export
data Anchoring = Relative | Absolute

||| An opaque path type in which paths are normalized at construction time.
|||
||| This means:
||| - '..' sequences are collapsed, i.e. foo/../bar  => bar/
|||     - Note that there is an exception here. 
|||       Leading '..'s are disallowed, because we dont know how to expand them.
||| - '~'  sequences are disallowed, as they rely on the user environment.
||| - '/'  sequences are collapsed
export
data Path : (a : Anchoring) -> (f : Kind) -> Type where
  AbsoluteDirectory : String -> Path Absolute Directory
  AbsoluteFile      : String -> Path Absolute File
  RelativeDirectory : String -> Path Relative Directory
  RelativeFile      : String -> Path Relative File

-- ---------------------------------------------------------------------------
-- Implementation Details
-- ---------------------------------------------------------------------------
rmConsecutiveSlashes : String -> String
rmConsecutiveSlashes s = pack $ rmConsecutiveSlashesImpl (unpack s)
  where rmConsecutiveSlashesImpl : List Char -> List Char
        rmConsecutiveSlashesImpl [] = []
        rmConsecutiveSlashesImpl ('/' :: '/' :: cs) = rmConsecutiveSlashesImpl ('/' :: cs)
        rmConsecutiveSlashesImpl (c :: cs) = c :: rmConsecutiveSlashesImpl cs

rmDotSlashPrefix : String -> String
rmDotSlashPrefix s = pack $ rmDotSlashPrefixImpl (unpack s)
  where rmDotSlashPrefixImpl : List Char -> List Char
        rmDotSlashPrefixImpl [] = []
        rmDotSlashPrefixImpl ('.' :: '/' :: cs) = cs
        rmDotSlashPrefixImpl cs = cs

rmConsectiveDots : String -> String
rmConsectiveDots s = pack $ reverse $ rmConsectiveDotsImpl $ reverse $ unpack s
  where clearNextSegment : List Char -> List Char
        clearNextSegment [] = []
        clearNextSegment (c :: cs) = if c == '/' 
                                        then cs
                                        else clearNextSegment cs
    
        rmConsectiveDotsImpl : List Char -> List Char
        rmConsectiveDotsImpl [] = []
        rmConsectiveDotsImpl ('/' :: '.' :: '.' :: '/' :: cs) = (
          rmConsectiveDotsImpl (clearNextSegment cs))
        rmConsectiveDotsImpl (c :: '/' :: '.' :: '.' :: '/' :: cs) = (
          c :: rmConsectiveDotsImpl (clearNextSegment cs))
        rmConsectiveDotsImpl (c :: cs) = c :: rmConsectiveDotsImpl cs

containsTilde : String -> Bool
containsTilde s = containsTildeImpl $ unpack s
  where containsTildeImpl [] = False
        containsTildeImpl (c :: cs) = (c == '~') || containsTildeImpl cs 

containsPrefixDotDot : String -> Bool
containsPrefixDotDot s = containsPrefixDotDotImpl $ unpack s
  where containsPrefixDotDotImpl : List Char -> Bool
        containsPrefixDotDotImpl [] = False
        containsPrefixDotDotImpl ('.' :: '.' :: cs) = True
        containsPrefixDotDotImpl (c :: cs) = False

containsAnyDotDot : String -> Bool
containsAnyDotDot s = containsAnyDotDotImpl $ unpack s
  where containsAnyDotDotImpl : List Char -> Bool   
        containsAnyDotDotImpl [] = False
        containsAnyDotDotImpl ('.' :: '.' :: cs) = True
        containsAnyDotDotImpl (c :: cs) = containsAnyDotDotImpl cs

rmLeadingSlash : List Char -> String
rmLeadingSlash [] = "" 
rmLeadingSlash ('/' :: cs) = pack cs
rmLeadingSlash cs = pack cs

addFrontSlash : List Char -> String
addFrontSlash [] = "" 
addFrontSlash ('/' :: cs) = pack $ '/' :: cs
addFrontSlash cs = pack $ '/' :: cs

rmTrailingSlash : List Char -> String
rmTrailingSlash cs = reverse $ rmLeadingSlash $ reverse cs 

||| Normalize a string such that it can be used to construct a path
||| via one of the mk* functions.
export
normalize : Anchoring -> String -> String
normalize Relative s = rmTrailingSlash $ unpack $ rmLeadingSlash $ unpack $ rmConsectiveDots $ rmDotSlashPrefix $ rmConsecutiveSlashes s  
normalize Absolute s = rmTrailingSlash $ unpack $ addFrontSlash $ unpack $ rmConsectiveDots $ rmDotSlashPrefix $ rmConsecutiveSlashes s 

parsePathImpl : List Char -> Anchoring -> Bool
parsePathImpl ('/' :: s) Relative = False 
parsePathImpl s Relative          = True 
parsePathImpl ('/' :: s) Absolute = True 
parsePathImpl s Absolute          = False 

parsePath : String -> Anchoring -> Maybe String
parsePath s Absolute = let containsTilde' = containsTilde s
                           containsAnyDotDot' = containsAnyDotDot s
                           basicPreconditionsFailed = containsTilde' || containsAnyDotDot' in
                       if basicPreconditionsFailed 
                          then Nothing
                          else (if parsePathImpl (unpack s) Absolute 
                                   then Just (normalize Absolute s)
                                   else Nothing)
parsePath s Relative = let containsTilde' = containsTilde s
                           containsPrefixDotDot' = containsPrefixDotDot s
                           basicPreconditionsFailed = containsTilde' || containsPrefixDotDot' in
                       if basicPreconditionsFailed 
                          then Nothing
                          else (if parsePathImpl (unpack s) Relative 
                                   then Just (normalize Relative s)
                                   else Nothing)

||| Attempt to construct a relative directory path from a string.
||| If the operation fails, it will return Nothing 
export
mkRelativeDirectory : String -> Maybe (Path Relative Directory)
mkRelativeDirectory s = case parsePath s Relative of
                             Just s   => Just (RelativeDirectory s)
                             Nothing  => Nothing 

||| Attempt to construct a relative file path from a string.
||| If the operation fails, it will return Nothing 
export
mkRelativeFile : String -> Maybe (Path Relative File)
mkRelativeFile raw = case parsePath raw Relative of
                        Just raw  => Just (RelativeFile raw)
                        Nothing   => Nothing

||| Attempt to construct an absolute directory path from a string.
||| If the operation fails, it will return Nothing 
export
mkAbsoluteDirectory : String -> Maybe (Path Absolute Directory)
mkAbsoluteDirectory raw = case parsePath raw Absolute of
                            Just raw => Just (AbsoluteDirectory raw)
                            Nothing  => Nothing 

||| Attempt to construct an absolute file path from a string.
||| If the operation fails, it will return Nothing 
export
mkAbsoluteFile : String -> Maybe (Path Absolute File)
mkAbsoluteFile raw = case parsePath raw Absolute of
                        Just raw => Just (AbsoluteFile raw)
                        Nothing  => Nothing 

getFileExtensionImpl : String -> Maybe String
getFileExtensionImpl s = case split (\c => c == '.') s of
                            []        => Nothing
                            (c :: cs) => Just $ last (c :: cs)

||| Get the file extension from a path. If there
||| is no file extension, Nothing will be returned.
export
getFileExtension : Path _ File -> Maybe String
getFileExtension (AbsoluteFile raw) = getFileExtensionImpl raw 
getFileExtension (RelativeFile raw) = getFileExtensionImpl raw 

getDirectoryNameImpl : String -> Maybe String
getDirectoryNameImpl s = case (List.reverse (split (\c => c == '/') s)) of
                              []        => Nothing
                              (c :: cs) => Just $ concat $ List.reverse cs

||| Get the directory name from a file path.
export
getDirectoryName : Path _ File -> Maybe String
getDirectoryName (AbsoluteFile raw) = getDirectoryNameImpl raw
getDirectoryName (RelativeFile raw) = getDirectoryNameImpl raw

||| Join two paths together.
||| Its only valid to concatenate certain types of paths 
||| 
||| AbsoluteDirectory ++ Relative Directory/File
||| RelativeDirectory ++ Relative Directory/File
||| 
||| Any other combination should be disallowed by the type sig.
export
concat : Path a Directory -> Path Relative k -> (Path a k)
concat (AbsoluteDirectory s1) (RelativeDirectory s2) = (AbsoluteDirectory (normalize Absolute (s1 ++ s2)))
concat (AbsoluteDirectory s1) (RelativeFile s2)      = (AbsoluteFile (normalize Absolute (s1 ++ s2)))
concat (RelativeDirectory s1) (RelativeDirectory s2) = (RelativeDirectory (normalize Relative (s1 ++ s2)))
concat (RelativeDirectory s1) (RelativeFile s2)      = (RelativeFile (normalize Relative (s1 ++ s2)))

export
Show (Path a k) where
  show (AbsoluteDirectory raw) = "Absolute Directory: " ++ raw
  show (AbsoluteFile raw)      = "Absolute File: "      ++ raw
  show (RelativeDirectory raw) = "Relative Directory: " ++ raw
  show (RelativeFile raw)      = "Relative File: "      ++ raw

||| Equality for Path.
||| 
||| Two paths are equal if they are of the same kind, same anchor,
||| and the underlying normalized representations are equal.
Eq (Path a k) where
  (==) (AbsoluteFile s1)      (AbsoluteFile s2)      = s1 == s2
  (==) (AbsoluteDirectory s1) (AbsoluteDirectory s2) = s1 == s2
  (==) (RelativeDirectory s1) (RelativeDirectory s2) = s1 == s2
  (==) (RelativeFile s1)      (RelativeFile s2)      = s1 == s2
  (==) _                      _                      = False
