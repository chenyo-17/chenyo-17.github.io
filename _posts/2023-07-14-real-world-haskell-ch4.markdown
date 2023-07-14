---
title: "Real world haskell 04: functional programming"
date: 2023-07-14 12:00:00 +0200
categories: lecture
tags: haskell
math: false
---

### General
- compile one haskell file with `ghc -make <file-name.hs>`, the file must contain a `main` function, then use `./<file-name> <args>` to run it
- functions that have only returns values defined for a subset of valid inputs are partial functions, otherwise they are total functions, sue `unsafe` prefix to indicate a function is a partial function, or use `Maybe` type to cover all inputs
- usually use variable name `foo'` as the updated value of `foo`
- `shiftL (import Data.Bits)`: logical shift 
- `.&.` bitwise and
- `.|.` bitwise or

### Infix functions and types
- if a function or constructor takes 2 or more arguments, can use the infix form of the function by placing it between its first and second arguments and surround the function name in backticks
    ```haskell
    a `plus` b = a + b
    data a `Pair` b = a `Pair` b deriving (Show)

    -- can use either prefix or infix forms
    foo = Pair 1 2
    bar = True `Pair` "quux"

    -- infix forms improve readability for some functions
    3 `elem` "camogie"
    import Data.List
    "foo" `isPrefixOf` "foobar"
    ```
- there are no other usage of backticks in Haskell

### Lists
- `Prelude` re-exports a large subset of list functions exported by `Data.List`
- `length :: [a] -> Int`: count the number of elements in a list
- `null :: [a] -> Bool`: whether a list is empty
- not handle empty list -> partial functions
    - `head :: [a] -> a`: access the first element of a list
    - `tail :: [a] -> [a]`: return all but the first element
    - `last :: [a] -> a`: access the last element
    - `init :: [a] -> [a]`: return all but the last element
- use `length` to check whether a list is empty before using function like `head` is efficient, as `length` will go through the entire list, use `null` instead, which checks in constant time
    ```haskell
    safeHead xs = if not (null xs) then head xs else 'Z'

    -- alternative
    safeHead2 (x: _) = x
    safeHead2 [] = 'Z'
    ```
- `(++) :: [a] -> [a] -> [a]`: concatenate two lists
- `concat :: [[a]] -> [a]`: flatten all sublists
    ```haskell
    concat [[1,2,3], [4,5,6]] --  [1, 2, 3, 4, 5, 6]
    concat (concat [[[1,2], [3]], [[4], [5], [6]]]) -- [1,2,3,4,5,6]
    ```
- `reverse :: [a] -> [a]`: returns elements in reverse order
- `and :: [Bool] -> Bool`: and all boolean elements, `and []` returns True
- `or :: [Bool] -> Bool`: or all boolean elements, `or []` returns False
- `all :: (a -> Bool) -> [a] -> Bool`: checks whether the predicate holds on all elements
- `any :: (a -> Bool) -> [a] -> Bool`: checks whether the predicate holds on any element
- `take :: Int -> [a] -> [a]`: returns a sublist of the first k elements
- `drop :: Int -> [a] -> [a]`: returns a sublist without the first k elements
- `splitAt :: Int -> [a] -> ([a], [a])`: return a pair of the input lists, split at the given index, e.g., `splitAt 3 "foobar` returns `("foo", "bar")`
- `takeWhile :: (a -> Bool) -> [a] -> [a]`: take elements from the beginning as long as the predicate returns True
- `dropWhile`: drop elements as long as the predicate returns True
- `break :: (a -> Bool) -> [a] -> ([a], [a])`: tuple up the results of `dropWhile`, e.g., `break even [1,3,5,6,8,9]` returns `([1,3,5], [6,8,9])`
- `span`: tuple up the results of `takeWhile`, e.g., `span even [2,4,6,7,9,10]` returns `([2,4,6], [7,9,10])`
- `elem :: (Eq a) => a -> [a] -> Bool`: whether a value is present in a list
- `notElem`: not `elem`
- `filter :: (a -> Bool) -> [a] -> [a]`: returns every element of the list on which the predicate succeeds
- `isPrefixOf :: (Eq a) => [a] -> [a] -> Bool`: whether the given sublist matches the beginning of the list, need to import `Data.List`
- `isInfixOf`: whether it is a sublist of the list
- `zip :: [a] -> [b] -> [(a, b)]`: returns a list of the same length as the shorter of the two input lists, e.g., `zip [12,72,93], "zippity` returns `[(12, 'z'), (72, 'i'), (93, 'p')]`
- `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`: takes two lists and applies a function to each pair of elements, e.g., `zipWith (+) [1,2,3], [4,5,6]` returns `[5,7,9]`
- `zip7`, `zipWith7`: zip up to 7 lists together
- `lines`: separate strings into lines separated by `\n`
- `unlines`: place a `\n` at the end of each line, e.g., `unlines ["foo", "bar"]` returns `"foo\nbar\n"`
- `words`: split an input string on any whitespace, e.g., `words "the   \r  quick \t   brown\n\nfox"` returns `["the","quick","brown","fox"]`
- `unwords`: use a single space to join a list of words

#### Tail recursion

- structured recursion has a base case and a recursive case
- the loop can be achieved with a tail recursive function, where in the recursive case, the function just calls itself
    ```haskell
    loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                        in loop acc' xs
    ```
- functional programming languages make tail recursive optimization (TCO) so that the recursive functions only require constant space

#### Map
- `map :: (a -> b) -> [a] -> [b]` takes a function and applies it to every element of a list, returns a new list constructed from the results of applications
    ```haskell
    square xs = map squareOne xs
        where squareOne x = x * x

    upperCase cs = map toUpper xs
    ```

- a function that takes another function as an argument or return functions is a high-order function
    ```haskell
    myMap f (x: xs) = f x : myMap f xs
    -- wildcard on f indicates f is not called on the right hand
    -- the constructor on the second argument is necessarily the empty list
    myMap _ _ = []  

    import Data.Char
    myMap toUpper "whispering"
    myMap negate [1,2,3]
    ```
- this pattern of spotting a repeated idiom and then abstracting it to reuse it is a common aspect of Haskell programming

#### Filter
- `filter :: (a -> Bool) -> [a] -> [a]` takes a predicate and applies to every element in ist input list, returns a list only for which the predicate is True

#### Fold

```haskell
-- compute one answer over a collection
mySum xs = helper 0 xs
    where helper acc (x:xs) = helper (acc + x) xs
        helper acc _ = acc
```
- fold up: do something to every element of a list, update an accumulator as we go, and returns the accumulator when we are done
- `foldl :: (a -> b -> a) -> a -> [b] -> a` takes a function, an initial value of the accumulator and the list
    ```haskell
    foldl step zero (x:xs) = foldl step (step zero x) xs
    foldl _ zero [] = zero 

    foldlSum xs = foldl step 0 xs
        where step acc x = acc + x -- accumulator is the first arg of step in foldl

    niceSum xs = foldl (+) 0 xs

    -- unfold evaluation of foldl (+) 0 (1:2:3:[])
    -- = foldl (+) (0+1) (2:3:[])
    -- = foldl (+) ((0+1) + 2) (3:[])
    -- = foldl (+) (((0+1)+2) + 3) []
    -- = (((0+1) + 2)+3)
    ```
- fold, map and filters have more readability than explicit recursion (also applies to other high-order functions)
- `foldr :: (a -> b -> b) -> b -> [a] -> b`: start from right
    ```haskell
    foldr step zero (x:xs) = step x (foldr step zero xs)
    foldr _ zero [] = zero

    -- unfold evaluation of foldr (+) 0 (1:2:3:[])
    -- = 1 + foldr (+) 0 (2:3:[])
    -- = 1 + (2 + foldr (+) 0 (3: []))
    -- = 1 + (2 + (3 + foldr (+) 0 []))
    -- = 1 + (2 + 3 + 0)
    
    -- replace the empty list ([]) with the zero value (0), and every constructor (:) with an application in the step function (+)
    ```
- many list manipulations can be written with `foldr`, e.g., `filter`, `identity`, `(++)`, and `foldl`, its first 2 arguments are: "what to do with each head/tail element" and "what to substitute for the end of the list"
    ```haskell
    -- filter with foldr
    myFilter p xs = foldr step [] xs
        where step x ys | p x = x : ys  -- accumulator is the second arg of step in foldr
                        | otherwise = ys

    -- filter with foldl
    myFilter2 p xs = foldl step [] xs
        where step ys x | p x = ys ++ [x]  -- x : ys will lead to reverse order
                        | otherwise = ys
    
    -- identity with foldr
    identity :: [a] -> [a]
    identity xs = foldr (:) [] xs  -- use [] to replace empty list, and from [], use (:) to connect each element from the end of the list

    -- (++) with foldr
    append :: [a] -> [a] -> [a]
    append xs ys = foldr (:) ys xs

    -- write foldl with foldr     
    myFoldl :: (a -> b -> a) -> a -> [b] -> a
    myFoldl f z xs = foldr step id xs z  -- (foldr step id xs) returns another function, that takes argument z 
        where step x g a = g (f a x)
    ```

#### Space leak
- use `foldr` instead of `foldl` in practice because `foldl` can have space leak (i.e., more space expensive)
    - `foldl f z (x:xs) = foldl f (f z x) xs`: it first evaluates the entire `foldl`, which recursively binds arguments to the next `foldl` application, the entire thing is not evaluated until the end of the recursion, which builds up a long thunk
    - `foldr f z (x:xs) = f x (foldr f z xs)`: it first evaluates `f` which is not recursive, if `x` is enough for the evaluate (e.g., `&& False`), then the rest is not evaluated, hence `foldr` can work on infinite lists
- use `foldl'` imported from `Data.List` instead, which dos not build up thunks

### Anonymous (lambda) functions
```haskell
isInAny needle haystack = any InSequence Stack
    where inSequence s = needle `isInfixOf` s

isInAny2 needle haystack = any (\s -> needle `isInfixOf` s) haystack
-- \ starts an anonymous function
-- put arguments (s) right after \
-- -> starts the function body
-- wrap the lambda function in () to indicate when the function ends
```
- a lambda can only have one single clause (one pattern) in its definition
    ```haskell
    unsafeHead = \(x:_) -> x
    ```
- lambda reduces the readability

### Partial function application (Currying)

- `->` has only one meaning: denotes a function that takes an argument of the type on the left, and returns a value of the type on the right
- all functions take only one argument
    ```haskell
    dropWhile :: (a -> Bool) -> [a] -> [a] -- (a -> Bool) -> ([a] -> [a])
    -- dropWhile returns a function that takes one argument
    -- dropWhile isSpace :: [Char] -> [Char]
    map (dropWhile isSpace) [" a", "f", "    e"]  -- ["a", "f", "e"]
    ```
- every time we supply an argument to a function, we can chop an element off the front of its type signature
    ```haskell
    zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
    zip3 "foo" :: [b] -> [c] -> [(Char, b, c)]
    (zip "foo") "aaa" "bbb" -- [('f','a','b'),('o','a','b'),('o','a','b')]
    zip3foo = zip3 "foo"
    zip3foo "aaa" "bbb" -- [('f','a','b'),('o','a','b'),('o','a','b')]
    ```
- partial function application can avoid writing reused functions and is often more useful than anonymous functions
    ```haskell
    isInAny3 needle haystack = any (isInfixOf needle) haystack
    -- take the function `isInfixOf` and fix its first argument
    ```
- partial function is called currying
    ```haskell
    niceSum :: [Integer] -> Integer
    niceSum xs = foldl (+) 0 xs
    -- don't need to fully apply foldl, can omit the list xs from both parameter lists
    -- this gives a more compact function that has the same type
    nicerSum :: [Integer] -> Integer
    nicerSum = fold (+) 0
    ```

### Sections
- can write a partially applied function in infix style: if enclose an operator in `()`, can supply its left or right argument argument inside `()` to get a partially applied function, this function is called a section
    ```haskell
    (1+) 2 -- 3
    map (*3) [24, 36] -- [72, 108]
    -- fix `elem`'s second argument
    (`elem` ['a'..'z']) :: Char -> Bool
    all (`elem` ['a'..'z'] "Frobozz") -- False 

    isInAny4 needle haystack = any (needle `isInfixOf`) haystack
    
    ```

### As-pattern
- `tails` from `Data.List` returns all suffixes plus an extra empty list at the end
    ```haskell
    tails "foobar"  -- ["foobar", "oobar", "obar", "bar", "ar", "r", ""]
    tails [] -- [[]]
    ```
- write own version to exclude `[[]]`
    ```haskell
    suffixes :: [a] -> [[a]]
    suffixes xs@(_:xs') = xs : suffixes xs'
    -- bind the variable xs to the value that matches the pattern 
    -- xs is bound to the whole list, and xs' is bound to the tail
    suffixes _ = []

    suffixes "foo" -- ["foo", "oo", "o"]
    ```
- as-patterns improve readability, and share data instead of copying it
    ```haskell
    noAsPattern (x:xs) = (x:xs) : noAsPattern xs -- construct a new copy, allocate a new list node at runtime
    noAsPattern _ = []
    ```

### Composition
- `suffixes2 xs = init (tails xs)` behaves identically to `suffixes` by applying a function to the result of another function, can turn this pattern into a definition
    ```haskell
    compose :: (b -> c) -> (a -> b) -> a -> c
    compose f g x = f (g x)

    suffixes3 xs = compose init tails xs 
    -- use currying
    suffixes4 = compose init tails
    ```
- `Prelude` provides function `.` to compose functions
    ```haskell
    (.) :: (b -> c) -> (a -> b) -> a -> c
    suffixes5 = init.tails
    ```
- `.` is right-associative
    ```haskell
    -- count the number of words in a string that begins with a capital letter
    count = length . filer (isUpper . head). words
    
    -- extract a list of macro names from a C header file starting with `DLT_`
    dlts = foldr step [] . lines
        where step l ds
                | "#define DLT_" `isPrefixOf` l = secondWord l : ds
                | otherwise = ds
              secondWord = head . tail . words  -- head . tail only works for >= 2 words (tail [1] = [])
              -- manual inspection of ds shows that it contains at least 2 words (#define and DLT_), so the partial function is safe
    ```

### Tips for readable code
- `map, take, filter` > `fold` > explicit recursion
- less lambda

### Strict evaluation with `seq`
- expressions that are not evaluated lazily as strict, `foldl` is strict by bypassing the usual nonstrict evaluation with function `seq :: a -> t -> t` which forces the first argument to be evaluated, and returns the second argument
    ```haskell
    foldl' _ zrro [] = zero
    foldl' step zero (x:xs) = 
        let new = step zero x
        in new `seq` foldl' step new xs  -- new is evaluated and the result is bound to `step new xs`
    
    -- evaluate foldl' (+) 1 (2:[])
    -- let new = 1 + 2
    -- in new `seq` foldl' (+) new [] -- new is evaluated to 3
    -- foldl' (+) 3 [] -- 3, no thunks inside
    ```
- to have any effect, a `seq` expression must be the first thing evaluated in an expression
    ```haskell
    -- incorrect
    hiddenbyLet x y z = let a = x `seq` someFunc y
                        in anotherFunc a z

    -- correct
    onTheOutside x y = x `seq` someFunc y
    ```
- to strictly evaluate several values, chain `seq` expressions together, e.g., 
    ```haskell
    chained x y z = x `seq` y `seq` someFunc z
    ```
- when evaluating an expression, `seq` stops as soon as it reaches a constructor, e.g., `:`, `()`
    - `(1+2):(3+4): []`: only evaluates `(1+2)`
    - `((1+2), (3+4))`: evaluate nothing because it encounters pair constructor `()`
    ```haskell
    -- work around
    strictPair (a, b) = a `seq` b `seq` (a, b)
    strictList (x:xs) = x `seq` x : strictList xs
    strictList [] = []
    ```
- `seq` needs to perform runtime check to see if an expression has been evaluated

