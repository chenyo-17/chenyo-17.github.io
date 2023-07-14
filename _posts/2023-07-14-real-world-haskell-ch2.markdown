---
title: "Real World Haskell 02: Types and functions"
date: 2023-07-14 09:00:00 +0200
categories: lecture
tags: haskell
math: false
---

### Major features
-  why need type systems: introduce abstraction, ignore implementation details
- strong
    - an ill typed expression will raise a type error at compile time
    - no implicit coercion (e.g., type casting from int to float) for function input
- static
    - the compiler knows the type of every value and expression at compile time
    - the combination of strong and static typing makes runtime type error impossible 
- type inference: the compiler can deduce the types of almost all expressions, explicitly declare the type is optional

### Basic types
- Char: Unicode character
- Bool: True or False
- Int: signed, fixed-width integer values
- Integer: signed integer of unbounded size, not used as often as Int due to expensive performance and space consumption
- Double: floating-point numbers, is typically 64 bits wide, Float is discouraged and slower than Double
- `:: MyType` is type signature, if omitted, the compiler will infer the type

### Function

- function application has higher precedence than using operators
- the parentheses in function arguments is necessary, e.g., `compare (sqrt 3) (sqrt 16)`
- function application is left-associative
- `add a b = a + b`, lhs is the name of the function and arguments, rhs is the body of the function
- use `:load add.hs` to use `add` function 
- use `:cd` to change the working directory, (but will lose all loaded modules in current directory)
- functions don't have `return`, as a function is a single expression, not a sequence of statements
- `=` means "meaning", the lhs is defined by the expression on the rhs

### Composite data types

#### Lists
- `head` returns the first element of a list
- `tail` returns all but the head of a list, but cannot be applied to an empty list
- the list type is polymorphic as the values in a list can have any type, use a lowercase letter to declare a type variable, e.g., `[a]`
- recursively substitute the type variable a specific type, e.g., `[[Int]]`
- the for loop in imperative languages are achieved by traversing a list recursively
- `take 2 [1,2,3,4,5]` returns the first 2 elements of the list
- `drop 3 [1,2,3,4,5]` returns all but the first 3 elements of the list 
- `null` checks whether a list is empty

#### Tuples
- tuples are fixed-size collections of values, each value can have a different type, e.g., `(True, "hello")`
- `()` is a tuple of 0 elements, it has one element also written as `()`, pronounced as "unit" (similar to void in C)
- there is no 1-element tuple
- a tuple's type is represented by the number, positions and the types of its elements, so `(False, 'a')` and `('a', False)` are tuples of different types
- often use tuples to return multiple values of a function
- `fst (1, 'a')` and `snd` functions return the first and second element of a pair

#### Function types
- `:type lines` returns `:: String -> [String]`, means `lines` has the type String to list-of-String
- side-effect means there is a dependency between the global state and the function behavior, essentially, side effects are invisible inputs to or outputs from functions
- Most Haskell functions are pure functions to not have side effects, e.g., the result of a function depends only on the explicit inputs
- impure functions have IO return type, e.g., `: type readFile` returns `readFile :: FilePath -> IO String`

### Variable
- a variable gives a name to an expression, once a variable is bound to an expression, its value does not change
    ```haskell
    x = 10
    x = 11 -- compiler error due to multiple declarations of x
    ```

### Conditional evaluation

```haskell
myDrop n xs =  
  if n <= 0 || null xs  -- predicate of type Bool
    then xs  -- indentation is important to continue the existing definition instead of starting a new one
    else myDrop (n - 1) (tail xs)  -- two branches must have the same type, else cannot be omitted
```
#### Lazy evaluation

- in `isOdd (1 + 2)`, the sub-expression `1 + 2` is not evaluated, instead, it creates a thunk to record the unevaluated expression and defer the actual evaluation until it's really needed
- the evaluation of `myDrop 2 "abcd"`
  - bind `n` to `2` and bind `xs` to `abcd`
  - evaluate the predicate `2 <= 0 || null "abcd"`
  - evaluate `2 <= 0` to False, therefore need to evaluate `null "abcd"`
  - `null "abcd"` evaluate to False, `False || False` evaluates to False, start to evaluate `else` branch
  - evaluate `myDrop (2 - 1) (tail "abcd")`, note even if `2-1` is not evaluated at this point
  - bind `n` to `2-1` and bind `ns` to `tail "abcd"`
  - evaluate `(2-1) <= 0 || null (tail "abcd")`, evaluate `2 -1` as 1 and `1 <= 0` as False
  - evaluate `tail "abcd"` as `bcd` and evaluate `null "bcd"` as False, still evaluate `else` branch
  - ...
  - in the second recursion application, bind `n` to `1-1` and `xs` to `tail "bcd"`, t`0 <=0` evaluates to True, so the `then` branch is evaluates
  - `myDrop (1 - 1) (tail "bcd") = tail "bcd"`
  - return from the first recursive application, substitute `else myDrop (n-1) (tail xs)` with `else (tail "bcd")`
  - `myDrop (2-1) (tail "abcd") = tail "bcd"`, return from the original application
  - `myDrop 2 "abcd" = tail "bcd"`
  - in the end, `myDrop 2 "abcd"` returns `tail "bcd"`, if it is `print (myDrop 2 "abcd")`, it will then evaluate `tail "bcd"`

### Polymorphism
- `last :: [a] -> a` means the function is polymorphic, `a` is a type variable, means the list can be of any type
- when applying `last` a list of Char, the compiler substitutes `Char` for each `a`, and the last now has the type `[Char] -> Char`, this is parametric polymorphism (e.g., a function can have parameters that can be later bound to real values)
- other polymorphism
    - subtype polymorphism is widely used in object-oriented languages, where a base class defines a set of behaviors that its subclasses can modify and extend
    - coercion polymorphism allows a value of one type to be implicitly converted into another type
- parametric polymorphism makes the real type inaccessible, e.g., the function type `: (a, b) -> a` can only return the first element of a pair

### Function with more than 1 argument
- `take :: Int -> [a] -> [a]` is `Int -> ([a] -> [a])` (-> is right associative) means it takes an Int and returns another function
- it can also be understood as it takes two arguments and returns a list

