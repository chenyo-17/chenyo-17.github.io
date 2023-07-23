---
title: "Real World Haskell 05: Work with JSON data"
date: 2023-07-23 14:00:00 +0200
categories: lecture
tags: haskell
math: false
---

### General
- `:reload` remembers the last loaded file
- `ghc -c SimpleJSON.hs` compiles the source file and only generates the object code (`-c`), the source file must have `main` function (the start of the program)
- `SimpleJSON.hi` is an interface file, where `ghc` stores information about the exports from in machine-readable form
- `SimpleJSON.o` is an object file, which contains the generated machine code (the textbook is out of date, see [this](https://stackoverflow.com/questions/53685213/the-io-action-main-is-not-exported-by-module-main))
- `ghc -o simple Main.hs` generate an executable named `simple`, then run with `./simple`
- use explicit type signatures
- can use `undefined` or `error "TODO"` as placeholders, which always typecheck when compiling the code
    ```haskell
    string :: String -> Doc
    string str = undefined
    
    undefined :: a
    ```
- `intercalate`: similar to `join` in Python
- regularly compile when writing large code
- point-free style: not mention the values the function operates on


### represent JSON data
- JSON supports 4 basic types: strings, numbers, booleans `null`, and 2 compound types: array (an ordered sequence of values) and objects (an unordered collection of key-value pairs, and key must be a string)    
    ```json
    {"numbers": [1,2,3,4], "useful": false}
    ```
- use algebraic data types to represent JSON data, supply a distinct value constructor for each JSON type
    ```haskell
    data JValue = JString String
                | JNumber Double
                | JBool Bool
                | JNull
                | JObject [(String, JValue)]
                | JArray [JValue]
                  deriving (Eq, Ord, Show)
    ```
- a source file contains a definition of a single module, which determines which names are accessible from other modules
- a source file begins with a module declaration, it must precede all other definitions in the source file
    ```haskell
    module SimpleJSON  -- module name must begin with a capital letter, and it must be the same as the file name
        ( -- a list of exports
          JValue(..)  -- export both the type and all of its constructors
        , getString
        , getInt
        , getDouble
        , getBool
        , getObject
        , getArray
        , isNull
        ) where  -- where indicates the body of the module follows
    ```
- if omit the exports, e.g., `module ExportEverything where`, all names are exported
- `module ExportNothing () where` exports nothing
- can only export type name, but not type constructors, so that the user cannot pattern match or construct a new value of that type
- module import must appear in a group at the beginning of a module
    ```haskell
    module Main (main) where -- module Main () is invalid in new GHC version
    import SimpleJSON

    main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
    ```
- to create an executable, `ghc` expects a module named Main that contains a function named `main`, the function will be called when running the program
- when importing a module, if not specify the list of names to import, all the names that the module exports will be imported
    - an explicit list of import can act as a reminder where the missing names come from
    - avoid the name conflict

### print JSON data
- should separate the rendering code from the code that actually prints a value (performs I/O) for more flexibility
    ```haskell
    putJValue :: JValue -> IO ()
    putJValue v = putStrLn (renderJValue v)
    ```
- can use function composition to chain the rendering and the compressing functions before printing

### Petty printer
- to produce output that is suitable for both human consumption and for machine processing
- write a new JSON renderer that uses the pretty-printing API
- instead of rendering straight to a string, the printer use an abstract type `Doc` to choose an implementation that is flexible and efficient

### Package
- `Cabal` is a standard set of tools, that help wth building, installing and distributing Haskell packages.
- A `package` contains one library, and possibly several executable programs
- `Cabal` needs a package description in a `.cabal` file, which is in the top level directory of the package, the package name usually matches the `.cabal` file name, as well as the directory name
    ```txt
    Name: mypretty
    Version: 1.0
    Synopsis: My pretty printing library, with JSON support
    Description:
        A simple pretty-printing library that illustrates how to
        develop a Haskell library.
    Author: Real Word Haskell
    Maintainer: nobody@realworldhaskell.org
    Cabal-Version: >= 1.2

    library  # to describe an individual library, the contents must be indented
        Exposed-modules: Prettify  # contains a list of modules that are available to users
                         PrettyJSON
                         SimpleJSON
        # Other-Modules: contains a list of internal modules that are required for the library to function, but invisible to users
        Build-depends: base >= 2  # contains a comma-separated list of packages
        # the base package contains many core Haskell modules, e.g., Prelude
        # no need to guess the dependency, the compiler will tell 
    ```
- GHC includes a package manager, which distinguishes between system-wide and per-use packages (only visible to current user)
- `ghc-pkg list` lists all installed packages, `ghc-pkg unregister` tells GHC that a package will not be used any longer, but users need to delete the installed files manually
- need a setup file to customize the building process
    ```haskell
    -- ensures the packages we ned are available
    -- and stores the settings to be used by Cabal
    -- install the package in the per-user database
    #!/usr/bin/env runhaskell
    import Distribution.Simple
    main = defaultMain
    ```
- `runghc Setup configure`: instruct Cabal how to build and where to install the package
- `runghc Setup build`: build the package
- `cabal install`: install the package
