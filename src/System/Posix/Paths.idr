-- --------------------------------------------------------------- [ Paths.idr ]
-- Module    : System.Posix.Paths
-- Copyright : (c) 2017 Josh Filstrup
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Paths

import Data.Vect

%default total

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

-- XXX: Fix this, it shouldnt be partial 
partial
rmConsectiveDots : String -> String
rmConsectiveDots s = pack $ reverse $ rmConsectiveDotsImpl $ reverse $ unpack s
  where clearNextSegment : List Char -> List Char
        clearNextSegment [] = []
        clearNextSegment (c :: cs) = if c == '/' 
                                        then cs
                                        else clearNextSegment cs
    
        rmConsectiveDotsImpl : List Char -> List Char
        rmConsectiveDotsImpl [] = []
        rmConsectiveDotsImpl (c :: '.' :: '.' :: '/' :: cs) = c :: rmConsectiveDotsImpl (clearNextSegment cs)
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

normalize : String -> String
normalize s = rmConsectiveDots $ rmDotSlashPrefix $ rmConsecutiveSlashes s  

parsePath : String -> Anchoring -> Maybe String
parsePath s a = let containsTilde' = containsTilde s
                    containsPrefixDotDot' = containsPrefixDotDot s
                    preconditionsFailed = containsTilde' || containsPrefixDotDot' in
                if preconditionsFailed 
                   then Nothing
                   else Just (normalize s)

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
getFileExtension (AbsoluteFile x) = getFileExtensionImpl x
getFileExtension (RelativeFile x) = getFileExtensionImpl x

||| Join two paths together.
||| Its only valid to concatenate certain types of paths 
||| 
||| AbsoluteDirectory ++ Relative Directory/File
||| RelativeDirectory ++ Relative Directory/File
||| 
||| Any other combination should be disallowed by the type sig.
export
concat : Path a Directory -> Path Relative k -> (Path a k)
concat (AbsoluteDirectory s1) (RelativeDirectory s2) = (AbsoluteDirectory (normalize (s1 ++ s2)))
concat (AbsoluteDirectory s1) (RelativeFile s2)      = (AbsoluteFile (normalize (s1 ++ s2)))
concat (RelativeDirectory s1) (RelativeDirectory s2) = (RelativeDirectory (normalize (s1 ++ s2)))
concat (RelativeDirectory s1) (RelativeFile s2)      = (RelativeFile (normalize (s1 ++ s2)))

Show (Path a k) where
  show (AbsoluteDirectory raw) = show raw
  show (AbsoluteFile raw)      = show raw
  show (RelativeDirectory raw) = show raw
  show (RelativeFile raw)      = show raw
