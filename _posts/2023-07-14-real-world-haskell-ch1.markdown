---
title: "Real World Haskell 01: Getting started"
date: 2023-07-14 8:00:00 +0200
categories: lecture
tags: haskell
math: false
---

### Major features

- pure functions: take immutable values as input and produce new values as output
- laziness: defer every computation until its result is actually required, e.g.,

    ```haskell
    -- find the k smallest elements in a list
    -- laziness ensures that the list is only sorted to find k minimum elements
    -- so that the this minima function also works even for infinite lists such as [1..]
    minima k xs = take k (sort xs)
    ```
- safe: can easily state properties and automatically generate test cases

### Resources

- Hoogle: API search engine to search for functions by name or type
- Hackage: a central repository of open-source Haskell libraries

### GHC

- a real-world compiler and interpreter, supports parallel execution and debugging tools
- ghc: an optimizing compiler, generates fast native code
- ghci: an interactive interpreter and debugger
    - `Preluce >` indicates the standard library Prelude is loaded, other loaded modules also show up in the prompt
    - change the default prompt with `:set prompt "ghci>"`
- `runghc`: run ghc programs as scripts without compiling it

### Interpreter (ghci)
- note that the syntax in ghci is different from source files

#### Basic operation
- write expressions in infix form (e.g., the operator appears between operands)
- in prefix form, must enclose the operator in parentheses, e.g., `(+) 2 2`
- integers can be arbitrarily large with `Integer` type (not `Int`), e.g., `313 ^ 15` (exp)
- use `**` for a floating exponent, e.g., `10 ** 2.5`
- use `:module + Data.Ratio` or `:m + Data.Ratio` to load other modules e.g., rational numbers (fractions)
- use `%` to represent a rational number, e.g., `11 % 29` is 11/29
- use `:unset +t` to turn off type information and use `:type 'a'` to print the type for the given expression, the expression is not evaluated
- define local variables with `let e = exp 1`
- `:show bindings` prints the current local variables and their types
- `truncate pi` returns 3
- `round 3.5` rounds to the nearest integer 
- `sqrt 16` returns floating-point number
- first character of a type name is uppercase (to distinguish from type variables), variable names are all low-case

#### Negative number
- `-3` means the operator `-` is applied to 3, `-` is the only unary operator in Haskell
- necessary to enclose a negative number in parentheses     
    ```haskell
    2 + (-3)
    3 + (-(13 * 37))
    2*-3  -- error, need to define operator *-
    ```

#### Basic logic
- boolean values: `True` and `False` (with capitalization)
- `&&` and `||`
- non-zero number is not True, 0 is not False
- ` 4 == 4.0` is True
- is not equal to: `/=`
- negation: `not`

### Operator precedence and associativity (fixity rules)
- assigns numeric precedence values to operators, 1 is the lowest and 9 is the highest
- use `:info (+)` to inspect the precedence levels, e.g., `infixl 6 +`
- associativity determines whether an expression is evaluated from left to right, or from right to left
    - `infixl`: left associative, `infixr`: right associative

#### Lists
- no commas after the last element, e.g., `[1,2,3]`
- a list can be of any length, all elements must be of the same type
- enumeration notation
    ```haskell
    [1..10]  -- [1,2,3,4,5,6,7,8,9,10]
    -- can optionally specify the step size by providing the first 2 elements
    -- followed by the value at which to stop the generation
    [1.0, 1.25..2.0] -- [1.0, 1.25, 1.5, 1.75, 2.0]
    [1, 4..15] -- [1, 4, 7, 10, 13]
    [10, 9..1] -- [1-,9,8,7,6,5,4,3,2,1]
    [1..] -- infinite list, have to kill ghci
    ```
- beware of enumerating floating list, e.g., `[1.0..1.8]` returns `[1.0, 2.0]`
    
- concatenate two lists with `(++)`, e.g., `[] ++ [False, True] ++ [True]` returns `[False, True, True]`
- `(:)` adds an element to the front of a list, e.g., `1 : [2, 3]` returns `[1, 2, 3]`, the first argument must be an element, the second must be a list

#### Strings and characters
- use single and double quotes to differ characters and strings
    ```haskell
    -- print a string with escape characters
    putStrLn "Here's a newline -->\n<-- See?"
    'a' -- a single character
    ```
- a string is a list of characters
    ```haskell
    let a = ['h', 'e', 'l', 'l', 'o']
    a  -- "hello"
    a == "hello"  -- True
    "" == []  -- True, the empty string is a synonym for []
    'a' : "bc"  -- "abc"
    "foo" ++ "bar"  -- "foobar"
    ```

#### Basic types
- use `:set +t` in ghci to print the type of an expression
    ```haskell
    "foo"
    -- it :: [Char]  
    -- String is a synonym of [Char]
    -- `it` stores the result of the last expression ghci evaluates
    -- ghci does not change the value of `it` if the last expression evaluation fails
    -- x :: y means expression x has the type y
- the size of `Integer` type is only bounded by the system's memory capacity
- `11 % 29` has the type `Ratio Integer`, meaning both numerator and denominator are of Integer type, different types such as `3.14 % 8` are not allowed  

- expressions can have different types
    ```haskell
    3 + 2 -- 5, and `it` is also 5
    :type it  -- it :: Integer
    -- a literal number such as 1 can be either an integer or a floating-point based on the context,
    -- here ghci by default chooses Integer to evaluate the expression
    :type 3 + 2  -- 3 + 2 is not evaluated
    -- 3 + 2 :: (Num t) => t, means the type is numeric
    ```
