---
title: "Real World Haskell 06: Typeclasses"
date: 2023-07-29 10:00:00 +0200
categories: lecture
tags: haskell
math: false
---

### Definition
- allow to define generic interfaces that provide a common feature set over a wide variety of types
- define a set of functions that can have different implementations depending on the types
    ```haskell
    -- first need to define the typeclass
    -- an instance type a of this typeclass implements the function defined in the typeclass
    class BasicEq a where
        isEqual :: a -> a -> Bool
    -- :type isEqual
    -- isEqual :: (BasicEq a) => a -> a -> Bool
    -- for all types a that implement BasicEq, isEqual takes two values of type a and returns a Bool
    
    instance BasicEq Bool where
        isEqual True True = True
        isEqual False False = True
        isEqual _ _ = False  -- isEqual "Hi" "Hi" arises error, because no instance of BasicEq for String

    class BasicEq2 a where
        isEqual2 :: a -> a -> Bool
        isNotEqual2 :: a -> a -> Bool

    -- rather than let users define both functions for all types, can provide default implementation
    -- users only need to define one function, as it is enough to figure out what the other function would do
    class BasicEq3 a where
        isEqual3 :: a -> a -> Bool
        isEqual3 x y = not (isNotEqual3 x y)  -- user can choose whether to define isNotEqual3 or isEqual3
        isNotEqual3 :: a -> a -> Bool
        isNotEqual3 x y = not (isEqual3 x y)

    -- when there are more types, they can also use the same function name
    instance BasicEq3 Color where
        isEqual3 Red Red = True  
        isEqual3 Green Green = True
        isEqual3 Blue Blue = True
        isEqual3 _ _ = False
    ```
- Haskell's built-in `==` and `/=` looks similar to `BasicEq3`
    ```haskell
    class Eq a where
        (==), (/=) :: a -> a -> Bool
        x == y = not (x /= y)
        x /= y = not (x == y)
    ```
    ```


### Built-in typeclasses
- `Show`: convert values to `String`, function `show` takes one argument and returns a `String`, `show` is used for a machine to parse back with `Read` 
    ```haskell
    -- :type show
    -- show :: (Show a) => a -> String
    show [1,2,3]  -- "[1,2,3]"
    show 1 -- "1", the quotes are not part of the string
    show "Hello" -- "\"Hello\"", ghci also uses show to print result, so quotes and escaping get added twice
    show ['H', 'i'] -- "\"Hi\""

    putStrLn (show 1) -- 1

    -- define show instance for new type
    instance Show Color where
        show Red = "Red"
        show Green = "Green"
        show Blue = "Blue"
    ```

- `Read`: defines functions that converts a `String` to any type that is a member of `Read`, `read :: (Read a) => String -> a`, 
    ```haskell
    main = do
            putStrLn "Please enter a Double:"
            inpStr <- getLine
            -- without knowing a specific type to convert to, the compiler will guess it is an `Integer` because the 2 below is treated as an `Integer`
            -- in more general case, e.g., read "4.5", the compiler will refuse to guess and just return error
            let inpDouble = (read inpStr)::Double  
            putStrLn ("Twice " ++ show inpDouble ++ " is " ++ show (inpDouble * 2))
    
    -- (read "5")::Integer -- 5
    -- (read "5")::Double -- 5.0
    -- (read "5.0")::Integer -- Exception: no parse
    ```
- `read` function relies on implementing `readsPrec` parsing function for each type
    ```haskell
    instance Read Color where
        -- the main function for parsing input
        readsPrec _ value = 
            tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
            where tryParse [] = []
                  tryParse ((attempt, result):xs) = 
                    if (take (length attempt) value) == attempt
                        then [(result, drop (length attempt) value)]  -- return the result and the remaining string for next parsing
                        else tryParse xs -- the current attempt failed, try the next one
    
    (read "Red")::Color -- Red
    (read "[Red,Green,Blue]")::[Color] -- [Red,Green,Blue], read will first invoke the read parser for list, then for Color, so not one readsPrec for the entire string
    (read "[Red, Green, Blue]")::[Color] -- Exception: no parse, the parser is not smart enough to handle spaces, can modify readsPrec to discard leading spaces
    ```
- serialization: convert data in memory to a flat series of bits for storage, `read`, `show` can be used for this process
- string handling in haskell is normally lazy, so `read` and `show` can used on quite large data structures
    ```haskell
    let d1 = [Just 5, Nothing, Nothing, Just 8, Just 9]::[Maybe Int]
    putStrLn (show d1) -- [Just 5,Nothing,Nothing,Just 8,Just 9]
    writeFile "test" (show d1)
    input <- readFile "test" -- "[Just 5,Nothing,Nothing,Just 8,Just 9]"
    let d2 = (read input)::[Maybe Int]  -- must specify the type
    d1 == d2 -- True
    ```
- many types are instances of `Read` and `Write` by default
    ```haskell
    putStrLn $ show [("hi", 1), ("there", 2)] -- [("hi",1),("there",2)]
    -- $ is for avoiding parentheses, otherwise need to write putStrLn (show [("hi", 1), ("there", 2)])
    -- $ has the lowest precedence, and is right-associative, so the function on the right is evaluated first
    -- function application is left-associative, e.g., f a b c is the same as ((f a) b) c
    ```

### [Numeric types](https://book.realworldhaskell.org/read/using-typeclasses.html#numerictypes.funcs)
- numeric operators are implemented in typeclasses so that that can be applies to any type
    - `Int`: [-2^29, 2^29-1]
    - `Int8`: 8-bit signed integer
    - `Int64`: 64-bit signed integer
    - `Integer`: arbitrary precision integer, range limited only by machine resources
    - `Rational`: arbitrary precision rational number, represented as a ratio of two `Integer`s
    - `Word`: unsigned integer, same size as `Int`
    - `Word8`: 8-bit unsigned integer
    - `Word64`: 64-bit unsigned integer
- operators
    - `(/)`: fractional division
    - `(**)`: the power of
    - `(^)`: the non-negative, integral power of
    - `(^^)`: the fractional number to any integral power of
    - `(%)`: ratio composition `Integral a => a -> a -> Ratio a`
    - `abs`: absolute value
    - `(.&.)`: bitwise AND
    - `div`: integer division always truncated down
- `==` and `/=` are defined in the `Eq` typeclass, `>=` and `<=` are defined in the `Ord` typeclass, they are in separate typeclass because there are types that are comparable but not ordered, e.g., `Handle`

### Automatic derivation
- for many data types, the compiler can automatically derive `Read`, `Show`, `Bounded`, `Enum`, `Eq` and `Ord` instances
    ```haskell
    data Color = Red | Green | Blue  -- the order is based on the order of construction
        deriving (Read, Show, Eq, Ord, Bounded, Enum)
    
    data MyType = MyType (Int -> Bool) -- cannot derive Show as it does not know how to render a function
    ```
- all types a new type refer to must belong to the same typeclasses as the new type

###  Making JSON easier to use
- need to wrap values of different types with a `JValue` constructor as Haskell does not natively support lists that contain types of different value. Therefore, if want to change the number 3920 to "3,920", need to change the constructor from `JNumber` to `JString`. This limitation can be solved with typeclasses
    ```haskell
    type JSONError = String
    
    class JSON a where
        toJValue :: a -> JValue  -- a is usually a String
        fromJValue :: JValue -> Either JSONError a
    
    instance JSON JValue where  -- JValue is already a JValue, so no need to convert
        toJValue = id
        fromJValue = Right  -- convert a JValue to the desired type
    ```
- `Either` type is often used to represent a computation that could fail (`Nothing` in `Maybe` gives no information if a failure occurs) with `Left` containing an error message and `Right` containing the result
    ```haskell
    data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

    instance JSON Bool where
        toJValue = JBool  -- toJValue True = JBool True 
        fromJValue (JBool b) = Right b  
        fromJValue _ = Left "not a JSON boolean"
    ```
- Haskell 98 standard does not allow the following instance, because `String` is a synonym for `[Char]`, which in turn is of type `[a]` (`[Char]` is not a concrete type), Haskell does not allow an instance of a typeclass for a specialized version of a polymorphic type. This is can be relaxed by placing `{-# LANGUAGE TypeSynonymInstances #-}` at the top of the file so that the code below can be compiled
    ```haskell
    instance JSON String where 
        toJValue = JString
        fromJValue (JString s) = Right s
        fromJValue _ = Left "not a JSON string"
    ```
- the comment below is a pragma, which is a directive to the compiler, which tells it to enable a language extension
- can add new instances anywhere
    ```haskell
    doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
    doubleToJValue f (JNumber v) = Right (f v)
    doubleToJValue _ _ = Left "not a JSON number"

    instance JSON Int where 
        toJValue = JNumber . realToFrac  -- convert an Int to a Double
        fromJValue = doubleToJValue round  -- convert a JValue to an Int

    instance JSON Integer where
        toJValue = JNumber . realToFrac
        fromJValue = doubleToJValue round  -- round to neatest integer (as need to return an Integer)
    
    instance JSON Double where
        toJValue = JNumber
        fromJValue = doubleToJValue id
    ```
- the following two instances can cause problem, as they are overlapping instances (`[(String, a)]` is also `[a]`) so that the compiler cannot decide which one to use, the problem occurs when the instance is used
    ```haskell
    instance (JSON a) => JSON [a] where  -- a is already an instance of JSON
        toJValue = undefined
        fromJValue = undefined
    
    instance (JSON a) => JSON [(String, a)] where  -- the input is an association list
        toJValue = undefined
        fromJValue = undefined

    -- can have multiple type parameters
    instance (JSON a, JSON b) => JSON [(a, b)] where
    ```
- the overlapping issue can be relaxed by using `OverlappingInstances` pragma to ask the compiler to pick the most specific one, but it rejects the code it if finds more than one equally specific instance. Need to add the extension for the module that constains the instance definition, so that the compiler knows the instance can be overlapped with other instances, no need to enable it in the importing module
    ```haskell
    {-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
    import Data.List
    class Foo a where
        foo :: a -> String

    instance Foo a => Foo [a] where
        foo = concat . intersperse ", " . map foo
    instance Foo Char where
        foo c = [c]
    instance Foo String where  -- this is more specific than [a]
        foo = id
    ```
- the language extensions are specific to GHC, so they are not present in Haskell 98
- Haskell 98 has two methods in `Show` class: `show` and `showList`. The default implementation of `showList` renders a list using square brackets and commas, and the instance of `Show` for `[a]` uses `showList` and treats `String` differently (e.g., render quotes instead of brackets)

### Newtype
- `newtype` provides another way to create a new type, it renames an existing type and gives it a distinct identity
- `type` gives another way to refer to a type, `newtype` hide the nature of a type
    ```haskell
    newtype UniqueID = UniqueID Int deriving (Eq)  -- UniqueID is different from Int, the user does not it is an Int

    newtype NewtypeInt = N Int deriving (Eq, Ord, Show)
    ```
- when declaring a `newtype`, must chose which of the underlying type's typeclass instances we want to expose, if there is no automatic derivation, can either a new instance, or leave the typeclass unimplemented
    ```haskell
    N 1 < N 2  -- True
    N 313 + N 37 -- no an instance for +
    ```
- a `newtype` can only have one value constructor with exactly one field, as its purpose is to give an existing type a new identity
- a type created with `data` needs to track which constructor a value is created with, a `newtype` dies not need this overhead, and it more space and time efficient at runtime
- a `newtype`'s constructor is used only at compile time and does not exist at runtime, pattern matching on `undefined` behaves differently between `data` and `newtype`
  - if `D` is a constructor of a `data` type, `case undefined of D _ -> 1` will cause exception, as `undefined` is evaluated to check its constructor
  - if `D` is a constructor of a `newtype`, `case undefined of D _-> 1` will return `1`, as `D` constructor does not exist at runtime, so it is equivalent to `case undefined of _ -> 1`, which always matches, so `undefined`` is not evaluated
- usually, when exporting a `newtype`, we don't export its data constructor, to keep the details of the type abstract. Instead, we use a function to apply a constructor for us.
    ```haskell
    module JSONClass (JAry(fromJAry), jary) where  -- users do not know how JAry is implemented to avoid user pattern matching on JAry
    jary :: [a] -> JAry a
    jary = JAry
- use `newtype` can help avoid overlapping instances
    ```haskell
    module JSONClass (JAry(..)) where  -- (..) means export all details of this type
    -- wrap up the list type so that the compiler does not see it as a list, to distinguish from JSON object [(String, a)]
    -- fromJAry is a deconstruct
    newtype JAry a = JAry { fromJAry :: [a] } deriving (Eq, Ord, Show)
    newtype JObj a = JObj { fromJObj:: [(String, a)] } deriving (Eq, Ord, Show)

    data JValue = JString String
                | JNumber Double
                | JBool Bool
                | JNull
                -- (JObj JValue) = ([(String, JValue)]), just replace the newtype with the underlying type and apply the type parameter
                | JObject (JObj JValue)  
                | JArray (JAry JValue)  -- JArray is a constructor of JValue, JAry is a type constructor
                  deriving (Eq, Ord, Show)
    
    jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
    jaryToJValue :: (JSON a) => JAry a -> JValue

    instance (JSON a) => JSON (JAry a) where
        toJValue = jaryToJValue  -- now just a type signature, no implementation
        fromJValue = jaryFromJAry
    
    -- convert a list where everything is an instance of JSON to a list of JValues
    listToJValues :: (JSON a) => [a] -> [JValue]
    listToJValues = map toJValue

    jvaluesToJAry :: [JValue] -> JAry JValue
    jvaluesToJAry = JAry
    -- the above has no performance cost, as it just tells the compiler to hide the fact that we are using a list

    jaryOfJValuesToJValue :: JAry JValue -> JValue
    jaryOfJValuesToJValue = JArray

    -- assemble the above functions to convert a list to JValue
    -- the argument is JAry = [a], fromJAry just return it as a list
    -- toJValue finds the right instance of JSON for each element of the list
    -- JAry wrap it up as a JAry again
    -- JArray wrap it up as a JValue
    jaryToJValue = JArray . JAry . map toJValue . fromJAry

    -- convert a JValue to a JAry
    jaryFromJValue (JArray (JAry a)) = whenRight JAry (mapEithers fromJValue a)
    jaryFromJValue _ = Left "not a JSON array"
    
    whenRight :: (b -> c) -> Either a b -> Either a c
    whenRight _ (Left err) = Left err
    whenRight f (Right a) = Right (f a)

    mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
    mapEithers f (x:xs) = case mapEithers f xs of
                            Left err -> Left err -- if encountered an error, just return it
                            Right ys -> case f x of
                                          Left err -> Left err
                                          Right y -> Right (y:ys)
    mapEithers _ _ = Right []
    ```
- conversion between `JObj` and `JValue`
    ```haskell
    import Control.Arrow (second)

    instance (JSON a) => JSON (JObj a) where
        -- second: send the second argument to the argument arrow (to apply it with other function), 
        -- while keep the first unchanged
        toJValue = JObject . JObj . map (second toJValue) . fromJObj  
        fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
            where 
                -- (,) is a tuple declaration, it takes one more argument to form a tuple
                unwrap (k, v) = whenRight ((,) k) (fromJValue v)  
        fromJValue _ = Left "not a JSON object"
    ``` 

### Monomorphism Restriction (may no longer exist in new GHC)
```haskell
-- myshow = show -- compiling error
-- correct
myShow2 :: (Show a) => a -> String
myShow2 = show

myShow3 value = show value
```
