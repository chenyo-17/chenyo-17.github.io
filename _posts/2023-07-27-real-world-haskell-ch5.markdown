---
title: "Real World Haskell 05: Work with JSON data"
date: 2023-07-27 09:00:00 +0200
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
    ```haskell
    -- SimpleJSON.hs
    module SimpleJSON
        (
        JValue(..) 
        , getString
        , getInt
        , getDouble
        , getBool
        , getObject
        , getArray
        , isNull
        ) where

    data JValue = JString String
                | JNumber Double
                | JBool Bool
                | JNull
                | JObject [(String, JValue)]
                | JArray [JValue]
                deriving (Eq, Ord, Show, Read)
    ```

    ```haskell
    -- PrettyJSON.hs
    module PrettyJSON
    ( renderJValue
    ) where

    import           Data.Bits  (shiftR, (.&.))
    import           Data.Char  (ord)
    import           Numeric    (showHex)
    import           SimpleJSON (JValue (..))
    import           Prettify   (Doc, char, compact, pretty, double, fsep, hcat, punctuate,
                                text, (<>))

    import Prelude hiding ((<>))

    renderJValue :: JValue -> Doc
    renderJValue (JBool True) = text "true"
    renderJValue (JBool False) = text "false"
    renderJValue JNull = text "null"
    renderJValue (JNumber n) = double n
    renderJValue (JString s) = string s
    renderJValue (JArray a) = series '[' ']' renderJValue a
    renderJValue (JObject o) = series '{' '}' field o
    where
        field (name, val) = string name <> text ": " <> renderJValue val

    string :: String -> Doc
    -- point-free style
    -- a string is a series of characters wrapped in double quotes
    string = enclose '"' '"' . hcat . map oneChar

    -- enclose x with specific characters
    enclose :: Char -> Char -> Doc -> Doc
    enclose left right x = char left <> x <> char right

    oneChar :: Char -> Doc
    oneChar c
    -- lookup a key in a list of pairs (called association list, or alist)
    -- and return the value if found,
    -- otherwise return Nothing
    =
    case lookup c simpleEscape of
        Just r -> text r
        Nothing
        | mustEscape c -> hexEscape c
        | otherwise -> char c -- only printable ASCII characters are not escaped
    where
        mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

    -- [('\b', "\\b"), ('\n', "\\n"), ('\f', "\\f")]
    simpleEscape :: [(Char, String)]
    simpleEscape = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where
        ch a b = (a, ['\\', b])

    smallHex :: Int -> Doc
    -- replicate 3 "foo" = ["foo","foo","foo"]
    smallHex x = text "\\u" <> text (replicate (4 - length h) '0') <> text h
    where
        -- return a hexadecimal string of x
        -- showHex 114111 "" = "1bdbf"
        h = showHex x ""

    astral :: Int -> Doc
    -- split a character into 2
    astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where
        a = (n `shiftR` 10) .&. 0x3ff
        b = n .&. 0x3ff

    -- turn a character into the string "\u" followed by a 4-char sequence of hexadecimal digits
    -- representing the numerical value of the character
    hexEscape :: Char -> Doc
    hexEscape c
    -- smallHex can only represent Unicode characters up to 0xffff
    | d < 0x10000 = smallHex d
    -- to also represent a character above 0xffff in a JSON string
    | otherwise = astral (d - 0x10000)
    where
        d = ord c -- return the integer of a character

    -- print arrays and objects are similar,
    -- each starts with an opening character, followed by a series of elements separated by commas,
    -- and ends with a closing character
    series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
    series open close item =
    enclose open close . fsep . punctuate (char ',') . map item
    ```

    ```haskell
    -- Prettify.hs
    module Prettify
    ( Doc
    , (<>)
    , char
    , double
    , fsep
    , hcat
    , punctuate
    , text
    , compact
    , pretty
    ) where

    import Prelude hiding ((<>))

    -- Doc is a tree
    data Doc = Empty
            | Char Char
            | Text String
            | Line -- line breaker
            | Concat Doc Doc
            | Union Doc Doc  -- Union is used to represent a soft break or a hard break
            deriving (Show, Eq)

    empty :: Doc
    empty = Empty

    char :: Char -> Doc
    char c = Char c

    text :: String -> Doc
    text "" = Empty
    text s  = Text s

    double :: Double -> Doc
    double d = text (show d)

    line :: Doc
    line = Line

    -- text "foo" <> text "bar" = Concat (Text "foo") (Text "bar")
    -- text "foo" <> empty = Text "foo"
    (<>) :: Doc -> Doc -> Doc
    Empty <> y = y
    x <> Empty = x
    x <> y    = x `Concat` y

    punctuate :: Doc -> [Doc] -> [Doc]
    punctuate p []     = []
    punctuate p [d]    = [d]
    punctuate p (d:ds) = (d <> p) : punctuate p ds

    hcat :: [Doc] -> Doc
    hcat = fold (<>)

    fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
    fold f = foldr f empty

    fsep :: [Doc] -> Doc
    fsep = fold (</>)

    (</>) :: Doc -> Doc -> Doc
    x </> y = x <> softline <> y

    softline :: Doc
    -- softline inserts a new line if the line is too long
    -- otherwise it inserts a space
    -- this is done by maintaining 2 representations of the document, using the Union constructor
    softline = group line

    group :: Doc -> Doc
    group x = flatten x `Union` x

    flatten :: Doc -> Doc
    -- flatten replaces a Line with a space
    flatten (x `Concat` y) = flatten x `Concat` flatten y
    flatten Line           = Char ' '
    -- always call the flatten on the left side of a Union,
    -- the left of each Union is always the same width as, or wider than, the right
    -- the left of a Union could contain hard break, while the right side does not
    flatten (x `Union` _)  = flatten x 
    flatten other          = other

    -- compact representation when transmitting data
    compact :: Doc -> String
    compact x = transform [x]
    where
        transform [] = ""
        transform (d:ds) =
        case d of
            Empty        -> transform ds
            Char c       -> c : transform ds
            Text s       -> s ++ transform ds
            Line         -> '\n' : transform ds
            a `Concat` b -> transform (a:b:ds)
            _ `Union` b  -> transform (b:ds)

    pretty :: Int -> Doc -> String
    -- pretty takes one more argument to control the maximum line width when encountering a softline
    pretty width x = best 0 [x]
    where
        -- col: the number of columns emitted so far on the current line
        -- ds: the remaining document
        best col (d:ds) =
        case d of
            Empty        -> best col ds
            Char c       -> c : best (col + 1) ds
            Text s       -> s ++ best (col + length s) ds
            Line         -> '\n' : best 0 ds
            a `Concat` b -> best col (a:b:ds)
            -- check which representation fits the column width better
            a `Union` b  -> nicest col (best col (a:ds)) (best col (b:ds))
        best _ _ = ""

        -- nicest chooses the best representation of a document
        -- based on the current column
        nicest col a b
        | (width - least) `fits` a = a
        | otherwise                = b
        where
            least = min width col

    fits :: Int -> String -> Bool
    w `fits` _ | w < 0 = False
    w `fits` ""        = True
    w `fits` ('\n':_)  = True
    w `fits` (c:cs)    = (w - 1) `fits` cs
    ```

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
