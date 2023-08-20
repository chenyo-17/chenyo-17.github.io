---
title: "Real World Haskell 08: File Processing"
date: 2023-08-20 11:00:00 +0200
categories: lecture
tags: haskell
math: false
---

### Efficient string processing
- `String :: [Char]` is not efficient, every element is allocated individually and has bookkeeping overhead. Even programs write in interpreted languages can outperform Haskell code that uses String by an order of magnitude.
- `bytestring` library provides a fast alternative to `String`. Code written with `bytestring` can match or exceed the performance and memory footprint of C.
  - `Data.ByteString`: a strict type (evaluated when generated) named `ByteString`, represent a string of binary or text data in a single array, works best for small memory or random memory access
  - `Data.ByteString.Lazy`: a lazy type (only evaluate when needed) named `ByteString`, represent a string of binary or text data as a list of chunks (arrays of up to 64KB), works best for large streaming 

- read binary data
    ```haskell
    -- qualified: allow to refer to a module with a name
    -- always use qualified for ByteString
    import qualified Data.ByteString.Lazy as L
    -- if not explicit with the module name, the compiler will report error as Prelude also has take
    hasElfMagic :: L.ByteString -> Bool      
    -- since L is a lazy type, and we always take 4 bytes, it can read file of any size
    hasElfMagic content = L.take 4 content == elfMagic
        -- L.pack: take a list of Word8 values, and pack them into a lazy ByteString
        where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]
    
    isElfFile :: FilePath -> IO Bool
    isElfFile path = do 
        -- lazy reading the file as data when demanded
        content <- L.readFile path
        return (hasElfMagic content)
    ```
- read text data: `Data.ByteString.Char8` and `Data.ByteString.Lazy.Char8`, works with ASCII and some European character sets (values <=255)
    ```haskell
    -- ghci> putStr =<< readFile "price.csv"  print the content of the file
    -- (=<<)  :: Monad m => (a -> m b) -> m a -> m b
    import qualified Data.ByteString.Lazy.Char8 as L
    closing = readPrice . (!!4) . L.split ',' -- read the 4th column of the csv file, delimited by ','

    readPrice :: L.ByteString -> Maybe Int
    readPrice str = 
        case L.readInt str of
            Nothing             -> Nothing
            Just (dollars,rest) ->  -- parse 15 from 15.38
                case L.readInt (L.tail rest) of -- drop .
                    Nothing           -> Nothing
                    Just (cents,more) ->  -- parse 38 from 38
                        Just (dollars * 100 + cents)  -- combine 15 and 38 to 1538

    highestClose :: L.ByteString -> Maybe Int
    highestClose = maximum . (Nothing:) . map closing . L.lines  -- prepend Nothing as maximum cannot handle empty list
    
    highestCloseFrom path = do
        contents <- L.readFile path
        print (highestClose contents)
    ```

### Regular expression library
- use the infix operator `=~` in `:module Text.Regex.Posix` to match a string against a regular expression. The return type is polymorphic, so need to specify the type.
  ```haskell
  -- if the result is of type Bool, get a pass/fail result
  "my left foot" =~ "foo" :: Bool -- True
  -- match either "hand" or "foot"
  "your right hand" =~ "(hand|foot)" :: Bool -- True
  ```
- the typeclass `RegexContent` describes how a result type should behave. `Bool` is an instance of this typeclass. `Int` is another instance tat counts the number of times the regexp matches.
  ```haskell
  "a star called henry" =~ "planet" :: Int -- 0
  -- match any vowel
  "honorificabilitudinitatibus" =~ "[aeiou]" :: Int -- 13
  ``` 
- if the result is of type `String`, get the first matching string. If the result is of type `[String]`, get all matching strings.
  ```haskell
  "my left foot" =~ "foo" :: String -- "foo"
  -- the empty string means no match, which is difficult to distinguish from a match of the empty string,
  -- use [String] as the result type instead 
  "my left foot" =~ "bar" :: String -- ""
  "I, B. Ionsonii, uurit a lift" =~ "(uu|ii)" :: [String] -- ["uu","ii"]
  ```
- the result type can be more complex
  ```haskell
  -- [] matches any character in the [], * matches anything, ? matches any character
  pat = "(foo[a-z]*bar|quux)"
  -- get the string before the match, the match, and after the match
  "before foodiebar after" =~ pat :: (String, String, String) -- ("before ","foodiebar"," after")
  -- if the match fails, the second and the third element are empty strings
  "no match here" =~ pat :: (String, String, String) -- ("no match here","","")
  -- get a list of all groups in the pattern that matched in the forth element of a return tuple
  -- each group in the pattern is enclosed in parentheses
  "before foodiebar after" =~ pat :: (String, String, String, [String]) -- ("before ","foodiebar"," after",["foodiebar"])
  -- get the starting position and the length of a match
  "before foodiebar after" =~ pat :: (Int, Int) -- (7,9)
  -- get the starting position and the length of all matches
  "i foobarabr a quux" =~ pat :: [(Int, Int)] -- [(2,9),(14,4)]
  ```
- `=~` uses typeclasses for its argument types, can use either `String` or `ByteString` as the input type for the string to be matched and the pattern
  ```haskell
  import qualified Data.ByteString.Char8
  pack "foo" =~ "foo" :: Bool -- True
  ```
- If the return type is a string, it must be the same string type as the matching string
- `Text.Regex.Base` defines the common API adhered to by all other regexp modules
- PISIX regexp engine will match `fooooo` when given the pattern `foo|foo*`, while in Perl-style regex engine like Python, only the first `foo` will be matched

### Glob pattern to regular expression
```haskell
globToRegex :: String -> String
-- the regex must be anchored at both ends with ^ and $
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' ""             = ""
globToRegex' ('*':cs)       = ".*" ++ globToRegex' cs
globToRegex' ('?':cs)       = '.' : globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs)     = '[' : c : charClass cs
globToRegex' ('[':_)        = error "unterminated character class"
-- passes every other character through the escape function
globToRegex' (c:cs)         = escape c ++ globToRegex' cs

-- ensures that the regexp engine does not interpret certain characters
-- as regex syntax
escape :: Char -> String
escape c
| c `elem` regexChars = '\\' : [c]
| otherwise = [c]
where
    regexChars = "\\+()^$.{}]|" -- \ is a special character in Haskell string

-- checks that the character class is terminated
charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"

-- usage
"foo.c" =~ globToRegex "f??.c" :: Bool -- True
```
- `globToRegex` is not a tail recursive function, as in the expression `escape c ++ globToRegex' cs`, the last operation is `++`, while in a tail recursive function, the last operation should be a recursive call to itself. In a strict language, not using tail recursion could be problematic, but not in Haskell. This is because the implementation of `++`  allows lazy evaluation, so the list is generated on demand, and the memory usage is still constant (consumed elements are garbage collected).
  ```haskell
  (++) :: [a] -> [a] -> [a]
  (x:xs) ++ ys = x : (xs ++ ys)
  []     ++ ys = ys
  ```

### Match file system
```haskell
module Glob
  ( namesMatching
  ) where
import           System.Directory  (doesDirectoryExist, doesFileExist,
                                    getCurrentDirectory, getDirectoryContents)
import           System.FilePath   (dropTrailingPathSeparator, splitFileName,
                                    (</>))
import           Control.Exception (handle)
import           Control.Monad     (forM)
import           GlobRegex         (matchesGlob)

-- check whether the given string is a pattern or a literal name
isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

-- since the function searches real-world files, it returns IO type
-- if the string does not contain any pattern, simply check the given name exists in the file system
-- otherwise, split the pattern into a directory name and a base name
-- if the directory name is empty, list all matching files in the current directory
-- if the directory is not a pattern, create a list containing the directory name
-- if the directory contains a pattern, list all matching directories
namesMatching :: FilePath -> IO [String]
namesMatching pat
  | not (isPattern pat) = do
    exists <- doesNameExist pat
    return ([pat | exists]) -- if exists, [pat], otherwise []
  | otherwise = do
    case splitFileName pat of
      ("", baseName) -> do
        curDir <- getCurrentDirectory
        listMatches curDir baseName
      (dirName, baseName) -> do
        dirs <-
          if isPattern dirName
            then namesMatching (dropTrailingPathSeparator dirName)
            else return [dirName]
        let listDir =
              if isPattern baseName
                then listMatches
                else listPlain
        -- forM, maps the second argument (an action of type IO) over the list
        -- and returns a list of the results
        pathNames <-
          forM dirs $ \dir -> do
            baseNames <- listDir dir baseName
          -- find all matching file/directory names in the given directory
            return (map (dir </>) baseNames)
        return (concat pathNames)

-- the System.Directory module provides separate functions to check whether a file exists or a directory exists
-- this function combines the two
doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
  fileExists <- doesFileExist name
  if fileExists
    then return True
    else doesDirectoryExist name

-- list all files in the given directory that match the given pattern
listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <-
    if null dirName
      then getCurrentDirectory
      else return dirName
  -- (NOTE: this is a signature for old GHC version) handle :: (Exception -> IO a) -> IO a -> IO a
  -- the first argument is a function that passes an exception
  -- the second argument is an IO action that may raise an exception
  -- const :: a -> b -> a, takes two arguments and always returns the first one
  -- return [] :: IO [String], returns an empty list
  -- use const to ignore the exception (it is a partial function waiting for the exception as the second argument)
  handle (const (return []) :: IOError -> IO [String]) $ do
    names <- getDirectoryContents dirName'
    let names' =
          if isHidden pat
            then filter isHidden names
            else filter (not . isHidden) names
    return (filter (`matchesGlob` pat) names')

isHidden :: String -> Bool
isHidden ('.':_) = True
isHidden _       = False

-- if the given base name is not a pattern, simply return a singleton list containing the base name
listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <-
    if null baseName
      then doesDirectoryExist dirName
      else doesNameExist (dirName </> baseName)
  return ([baseName | exists])
```
- Instead of throwing an error, return type `Either GlobError [String]` to handle invalid patterns
```haskell
-- takes a filepath and renames it using the given function
renamWith :: (FilePath -> FilePath) -> FilePath -> IO FilePath
-- replaceExtention :: FilePath -> String -> FilePath
-- flip: takes another function and swaps the order of the two arguments
-- =<<: feeds the result of the action on the right to the action on the left
cc2cpp = mapM (renameWith (flip replaceExtension ".cpp")) =<< namesMatching "*.cc"
```
