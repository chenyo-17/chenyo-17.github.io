---
title: "Real World Haskell 04: Pattern matching"
date: 2023-07-14 11:00:00 +0200
categories: lecture
tags: haskell
math: false
---

### Define new data types

```haskell
data BookInfo = Book Int String [String]
                deriving (Show) -- tack Show on a type declaration so that ghci know how to a value of this type                
myInfo = Book 12345 "A book" ["Author 1", "Author 2"]
-- define a new data type with the data keywords
-- BookInfo is a type constructor
-- Book is a data/value constructor, use this to create a value of BookInfo type
-- Int, String and [String] are type components
-- BookInfo type contains the same components as a 3-tuple (Int, String, [String]), but it has a distinct type
-- the 3-tuple cannot be used when a BookInfo type is required
-- :type MyInfo :: BookInfo
```
- instead of using an anonymous type, can give a collection of related values a name and s distinct type
- two data types with different names cannot be mixed even if they have the same types

- use `let myInfo = Book 1 "Book' []` in ghci to define new type variables
- use `:info BookInfo` to gets more information about the type
- can treat a value constructor as a function, that returns the new type, e.g., `:type Book :: Int -> String -> [String] -> BookInfo`
- the type constructor and the value constructor often have the same name, e.g., `data BookReview = BookReview BookInfo CustomerID String`. Because the type constructor is only used in type declaration and signature, and the value constructor is only used in the expression, so there is no ambiguity


- can give a synonym for an existing type to give a type a more descriptive name, e.g., `BookReview` instead of `String` 
    ```haskell
    type CustomerID = Int  -- does not create a new type
    type ReviewBody = String
    data BetterReview = BetterReview BookInfo CustomerID ReviewBody

    type BookRecord = (BookInfo, BookReview)
    ```

### Algebraic data types
- an algebraic data type can have more than one value constructor
    ```haskell
    data Bool = False | True
    -- the Book type has 2 value constructors, can construct a Bool that has the value True or False
    -- different value constructors are different cases/alternatives
    ```
- each value constructor can take 0 or more arguments
    ```haskell
    type CardHolder = String
    type CardNumber = String
    type Address = [String]

    data BillingInfo = CreditCard CardNumber CardHolder Address
                     | CashDelivery
                     | Invoice CustomerID
                       deriving (Show)
    ```
- a data type whose value constructors all take 0 arguments are similar to enumerations in other languages
    ```haskell
    data Roygbiv = Red | Orange | Yellow | Green | Blue | Indigo | Violet deriving (Eq, Show)
    Red == Yellow -- False, Red and Yellow are not integers as in C
    ```


- two tuples with elements of the same types have the same type, but algebraic data types distinguish between otherwise identical pieces of information
    ```haskell
    data Cartesian2D = Cartesian2D Double Double
                       deriving (Eq, Show)  -- Eq generates code that compares the values for equality
    data Polar2D = Polar2D Double Double
                     deriving (Eq, Show)
    Cartesian2D (sqrt 2) (sqrt 2) == Polar2D (pi/4) 2 -- False, the equality operator cannot compare values of different types
    ```

### Pattern matching
```haskell
-- reproduce Not function
-- a function is a series of equations defining the behavior of the function for different input patterns
myNot True = False
myNot False = True

-- a more complex example
sumList (x:xs) = x + sumList xs -- (:) makes sure it is a non-empty list
sumList [] = 0

subList [1, 2] --  = sumList (1:(2:[])) = 1 + sumList (2:[]) = 1 + (2 + sumList []) = 1 + 2 + 0 = 3
```
- the first equation that matches the input pattern is used
- reverse the const to pattern match against the constructor by first checking the constructor name, then inspect the values
    ```haskell
    myInfo = Book 12345 "A book" ["Author 1", "Author 2"]
    Book id title authors = myInfo  -- this process does not modify myInfo, it just looks inside it
    id -- 12345
    title -- "A book"
    authors -- ["Author 1", "Author 2"]
    ```
- pattern matching on a tuple
    ```haskell
    third (a,b,c) = c  -- return the third element of a tuple
    complicated (True, a, x:xs, 5) = (a, xs)  
    complicated (True, 1, [1,2,3], 5) -- (1, [2,3])
    complicated (False, 1, [1,2,3], 5) -- Non-exhaustive patterns in function complicated
    ```
- pattern matching on an algebraic data type
    ```haskell
    -- accessor functions on the data type components
    bookID (Book id title authors) = id  -- :type BookID :: BookInfo -> Int
    bookTitle (Book id title authors) = title
    bookAuthors (Book id title authors) = authors

    nicerID (Book id _ _) = id  -- wildcard means not care, it does not bind a new variable
    ```
- covering all of a type's constructors is important,e.g., the non-empty list constructor `(:)` and the empty constructor `[]` should all be present
    ```haskell
    badExample (x:xs) = x + badExample xs

    badExample [] -- Non-exhaustive patterns in function badExample
    ```
- use `-fwarn=incomplete-patterns` to print warnings during compilation if a sequence of patterns does not cover all possible constructors
- use `_` to provide a default behavior

### Record syntax
- writing accessor functions for each data type component creates boilerplate code: necessary but bulky and irksome
- can define a data type and accessors for each component simultaneously
    ```haskell
    data Customer = Customer {
        customerID :: CustomerID,
        customerName :: String,
        customerAddress :: Address
    } deriving (Show)
    -- :type customerID :: Customer -> CustomerID
    ```
- can use the usual syntax or record syntax to create a value of this type
    ```haskell
    -- normal
    customer1 = Customer 123 "Customer 1" ["Address 1", "Address 2"]
    -- print: Customer 123 "Customer 1" ["Address 1","Address 2"]
    -- record syntax has more notations
    customer2 = Customer {
        customerID = 123, -- customerID is a normal function
        customerAddress = ["Address 1", "Address 2"],  -- can vary the order
        customerName = "Customer 1"
    }
    -- the record syntax also changes how the type is printed
    -- Customer {customerID = 123, customerName = "Customer 1", customerAddress = ["Address 1","Address 2"]}
    ```

### Parameterized types
- can introduce type variables in a type declaration to add polymorphism
    ```haskell
    -- type defined in Prelude
    -- give Maybe type constructor a parameter to create a new type
    data Maybe a = Just a  -- a is a type variable, indicates Maybe type takes another type as a parameter
                 | Nothing

    someBool = Just True  -- can use Maybe on values of any type, :: Maybe Bool
    someString = Just "something" -- :: Maybe [Char]

    -- can nest uses of parameterized types
    wrapped = Just (Just "wrapped")  -- :: Maybe (Maybe [Char])
    ```

### Recursive types

- recursive types are defined in terms of itself
    ```haskell
    data List a = Cons a (List a) -- similar to (:) constructor
                | Nil  -- similar to [] constructor
                deriving (Show)
    
    Nil -- Nil
    Cons 1 Nil -- Cons 1 Nil
    Cons 2 it -- Cons 2 (Cons 1 Nil)
    
    -- a binary tree is either a node with 2 children, or an empty value
    data Tree a = Node a (Tree a) (Tree a)
                | Empty
                deriving (Show)
    ```
- type `List a` and `[a]` are isomorphic with a function that takes any value of type `[a]` and produces a value of type `List a` 
    ```haskell
    fromList (x:xs) = Cons x (fromList xs)  -- substitute (:) with Cons
    fromList [] = Nil
    ```

### Error reporting
- `error :: String -> a` is a standard function to abort the evaluation and display error messages
- it takes the error string as the input and return any type, so it can be called anywhere and will always have the right type
    ```haskell
    mySecond :: [a] -> a
    mySecond xs = if null (tail xs)
                    then error "list too short"
                    else head (tail xs)
    ```
- problem of error function: not possible to recover from errors, can use `Maybe` type to represent the possibility of an error
    ```haskell
    tidySecond :: [a] -> Maybe a
    tidySecond (_:x:_) = Just x
    tidySecond _ = Nothing
    ```

### Local variables
- use a `let` expression to introduce local variables in a function
    ```haskell 
    lend amount balance = let reserve = 100  -- the block of variable declarations ends with `in`, the name on the left is bound to the expression on the right (not a value)
                              newBalance = balance - amount  -- each line introduces a new variable, the indentation is important
                          in if balance < reserve 
                             then Nothing
                             else Just newBalance
    ```
- can use the variable names in a `let` block both within the `let` block and the expression following the `in`
- the place where we can use a name is the name's scope, if a variable name is visible throughout the source file, it is at the top-level, can define variables at the top level with `item = "Item"`
- can ``use `where` to introduce local variables after the code to let reader focus more on the expression
    ```haskell
    lend2 amount balance = if amount < reserve * 0.5
                          then Just newBalance
                          else Nothing
        where reserve = 100
              newBalance = balance - amount
    ```

#### Shadowing
- not common to nest multiple lets
- can use the same name in a smaller scope to shadow the outer variable
    ```haskell
    bar = let x = 1
          in ((let x = "foo" in x), x)  -- ("foo", 1)
    
    -- can also shadow the function arguments
    quux a = let a = "foo"
             in a ++ "eek!"  -- "fooeek!"
    -- quux :: t -> [Char]  -- since the variable name is shadowed, it can have any type
    ```
- use `fwarn-name-shadowing` option in ghc to print warnings when there is a shadowing

### Local functions
- can define local functions in a `let` or `where` block, local functions can use variables from the scope that encloses the function
    ```haskell
    pluralise :: String -> [Int] -> [String]
    pluralise word counts = map plural counts -- map function applies plural function to every element of counts
        where plural 0 = "no " ++ word ++ "s"  -- if the current element is 0
            plural 1 = "one " ++ word
            plural n = show n ++ " " ++ word ++ "s"
    -- pluralise "dog" [0, 1, 2] -- ["no dogs", "one dog", "2 dogs"]
    ```

### Offside rule
- the top-level declarations can start in any column, every subsequent top-level declarations must have the same indentation
- an empty line or a line with more indentation is treated as the continuation of the current item
- after a `where` or `let` keyword, the compiler remembers the indentation of the next token, if the line of that follows is empty, or the indentation is further to the right, it is considered as the continuation of previous line, if the indentation is the same as the start of the preceding item, it is treated as beginning of a new item in the same block
    ```haskell
    bar = let b = 2
              c = True
          in let a = b  -- a is invisible in the outer let block
             in (a, c)    
    foo = x
        where x = y   -- x is visible to foo
                where y = 2  -- y is only visible to the first where block
    ```
- use spaces instead of tabs as tabs have different width on different editors
- can use explicit structuring to avoid offside rule, by using `{}` to group the block, and use `;` to separate each item, it works even for top-level, but it is seldom used
    ```haskell
    foo = let {a = 1; b = 2;
               c = 3}
          in a + b + c
    ```

### Case
- can use pattern matching not only in function definitions, but also within an expression
    ```haskell
    -- defined in Data.Maybe
    fromMaybe defval wrapped = 
        case wrapped of -- case keyword is followed by an arbitrary expression, of signifies the end of the expression
            Nothing -> defval
            Just value -> value  -- can also use _ as the default case
    ```

#### Common mistakes with patterns
- incorrectly matching against a variable
    ```haskell
    data Fruit = Apple | Orange
        deriving (Show)
    apple = "apple"
    orange = "orange"
    whichFruit :: String -> Fruit
    whichFruit f = case f of
                    -- apple is a local variable that shadows the outer apple, not a pattern,
                    -- so it will always match by binding the result of f to apple
                    apple -> Apple  
                    orange -> Orange
    ```
- incorrectly comparing for equality: cannot place a variable in multiple positions to express teo values should be the same, need to use guards to do this
    ```haskell
    bad_nodesAreSame (Node a _ _) (Node a _ _) = Just a  -- compiling error for conflicting definitions of a 
    bad_nodesAreSame _ _ = Nothing
    ```

### Guards
- pattern matching limits to performing fixed tests of a value's shape, need more expressive checks before evaluating a function's body
- a pattern can be followed by 0 or more guards, each an expression of type Bool, each guard is introduced by a `|` symbol
- when a guard is evaluated, all variables mentioned in the pattern are bound and can be used
    ```haskell
    lend3 amount balance
        | amount <= 0 = Nothing
        | amount > reserve * 0.5 = Nothing
        | otherwise = Just newBalance   -- a variable bound to True
        where reserve = 100
              newBalance = balance - amount

    nodesAreSame (Node a _ _) (Node b _ _)
        | a == b = Just a

    niceDrop n xs | n <= 0 = xs
    niceDrop _ [] = []
    niceDrop n (_:xs) = niceDrop (n - 1) xs
    ```
