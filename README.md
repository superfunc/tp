![](./misc/tp_logo.jpg) TP: An idris package for typed, unix-style, path manipulation. 
----

[![Build Status](https://travis-ci.org/superfunc/tp.svg?branch=master)](https://travis-ci.org/superfunc/tp)

#### Usage:

The [tests](src/Test/System/Posix/Paths.idr) provide decent examples for the time being.
Once things have been cleaned up, I'll try to provide more thorough examples here.

The core pieces are:

- `Path`: An opaque path type that encodes the kind(file/directory) 
  and anchoring(absolute/relative). 
- `mkRelativeFile`: Create a relative file from a string repr.
- `mkRelativeDirectory`: Create a relative directory from a string repr.
- `mkAbsoluteFile`: Create an absolute file from a string repr.
- `mkAbsoluteDirectory`: Create an absolute directory from a string repr.
- `concat`: Join two paths together.
- `str`: Get a string repr of a path.
- `Eq`: Paths can be compared by usual equality methods.
- `Show`: Pretty print path information.

Note that many of these operations, `concat` for example, restrict the type
of paths that can be used via the kind/anchoring, preventing a large class of
errors. In other cases, we use Maybe to encode possible failure, such as in
the case of the `mk` functions.

#### Dependencies:

This package provides an idris package, `System.Posix.Paths` for typed
path manipulation. I'd like to add some neat usage of dependent types to
this library, but the initial library just has Haskell-style,
strongly-typed, paths. 

Contributions are very welcome.

#### Disclaimer(s):

> 1) This work is my sole effort and is in no way associated with or sponsored by
my current, past, or future employer(s).
