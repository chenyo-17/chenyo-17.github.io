---
title: "Real World Haskell 07: IO"
date: 2023-07-30 15:00:00 +0200
categories: lecture
tags: haskell
math: false
---

### Classical IO
- `<-` binds the result from executing an IO action to a name
    ```haskell
    -- can run the program with runghc (run the script without compiling)
    main = do
        putStrLn "Hello, what's your name?"
        name <- getLine
        putStrLn $ "Welcome to Haskell " ++ name ++ "!"
    -- putStrLn :: String -> IO ()
    -- getLine :: IO String
    ```
- functions with `IO` return type means they may have side effects (e.g., show something in the terminal), or return different values when called with the same arguments 
- type `IO t` is an action. Actions can be created, assigned and passed everywhere, but is only executed within another IO action (e.g., `main`).
    ```haskell
    -- ghci> let writefoo = putStrLn "foo"
    -- ghci> writefoo -- when given an IO action, ghci will execute it
    -- foo  -- foo is not a return value of pustStrLn, but is the side effect of putStrLn writing foo to the terminal
    ```
- `IO t` indicates the return value is of type `t`, `IO ()` means there is no return value
- Io actions produce an effect when executed/performed/called by something else in a IO context, but not when evaluated. 
    ```haskell
    writefoo = putStrLn "foo"  -- this expression is not evaluated, writefoo just stores an IO action
    ```
- `main :: IO ()` is the entry point of any Haskell program. It is the mechanism that provides isolation from side effects: IO actions are executed in a controlled environment (the function return type indicates whether it is pure).
- `do` define a sequence of actions, it is only needed when there are multiple actions to execute. The value of a `do` block is the value of the last action executed. In a `do` block, use `<-` to get results from IO actions, and use `let` to get results from pure code
- every line in a `do` is a new context, so one can re-declare variables
    ```haskell
    main = do 
        let a = 1
        let a = 2  -- fine
        str <- getLine
        putStrLn $ "Data: " ++ str
        str <- getLine
        putStrLn $ "Data: " ++ str
    ```
- `System.Environment.getArgs :: IO [String]` returns a list of command line arguments
- use `System.Environment.getEnv` to look for a specific environment variable

### Handles
- import `System.IO` for basic IO functions
- `openFile` returns a file `Handle` to perform specific operations on the file, e.g., `hPutStrLn` works just like `putStrLn` but takes additional `Handle` argument, `hClose` is to close the `Handle`. There are `h` functions corresponding to usually all of the non-`h` functions in `System.IO`.
- `return` in a `do` block is the opposite of `<-`, it takes pure value and wraps it to IO, since every IO action must return some IO type. E.g., `return 7` would create ana action stored in `IO Int` type. When executed, the action would produce the result `7`.
  ```haskell
  returnTest =
      do one <- return 1  -- return does not terminate the do block, <- pull out of the values in the IO (e.g., Int)
         let two = 2
         putStrLn $ show (one + two)  
  ```
- `openFile :: FilePath -> IOMode -> IO Handle`, `openBinaryFile` handles binary files
  - `FilePath` is just a type synonym for `String`
  - `IOMode`: `ReadMode`, `WriteMode` (file is completely emptied), `AppendMode` (start from the end of the file), `ReadWriteMode`
- Haskell maintains internal buffers for files, until `hClose` is called on a file, the data may not be flushed out to the OS. Although when a program exits, Haskell will take care of closing files, but in some crashes this may not happen, so call `hClose` explicitly.
    ```haskell
    do 
      tempDir <- catch (getTemporaryDirectory) (\_ -> return ".")
      -- the return value from finally is the first actions's return value
      -- ensures even there is en exception before, the file will be closed
      finally (func tempfile temph)
              (do hClose temph
                  removeFile tempfile)
    ```
- when reading anf writing from a `Handle`, the OS maintains an internal record of the current position, `hTell` takes a `Handle` and returns the current position in the file, `hSeek` takes a `Handle` and a `SeekMode` and moves the position to the specified location. `SeekMode` can be `AbsoluteSeek`, `RelativeSeek` (from the current position, can be positive or negative), `SeekFromEnd` (seek from the end of the file, `hSeek handle SekFromEnd 0` goes to the end of the file).
- `Handle` can also corresponds to a network connection or a terminal, use `hIsSeekkable` to check if a `Handle` is seekable.
- there are 3 predefine `Handle`s: `stdin`, `stdout`, `stderr`, some OS allows to redirect the file handles, e.g., `$ echo John | runghc callingpure.hs`
    ```haskell
    getLine = hGetLine stdin  -- non-h functions are shortcuts for h functions
    putStrLn = hPutStrLn stdout
    print = hPrint stdout
    ```

### Files
- `System.Directory` provides `removeFile` and `renameFile` (can also be used to move files if the second argument is a different directory)
- `openTempFile` takes a directory (e.g., `.` or with `System.Directory.getTemporaryDirectory`) and a template for naming the file (it will be added with random characters), and returns the file path and a handle opened in `ReadWriteMode`, so can `hClose` and `removeFile` when done
    ```haskell
    -- the second is a lambda function, that takes an argument and ignores it, and return "."
    tempdir <- catch (getTemporaryDirectory) (\_ -> return ".")  
    ```
- `pos <- hTell temp` can appear multiple times in a IO context

### Lazy IO
- in classical IO, each line or block of data is requested and processed individually
- `hGetContents :: Handle -> IO String`, the string it returns is evaluated lazily. Data is only read from the Handle as the characters are processed. When elements are no longer used, the gabage collector automatically frees the memory (when all inputs are consumed, the file is automatically closed). The return value can be directly passed to pure functions. 
    ```haskell
    inpStr <- hGetContents inh
    hPutStr outh (map toUpper inpStr)  -- the compilers knows the consumed dtata can be freed
    ```
- `hGetContents` allows to read file that is larger than the memory, but the file cannot be closed until no processing is needed (e.g., after the `hPutStr` call). If `inpStr` is hanged on, the file will be kept open.
- `readFile :: FilePath -> IO String` wraps `openFile`, `hGetContents` and `hClose` together, `writeFile :: FilePath -> String -> IO ()` wraps `openFile`, `hPutStr` and `hClose` together
    ```haskell
    main = do 
        inpStr <- readFile "input.txt"
        writeFile "output.txt" (map toUpper inpStr)  -- hClose is no need
    ```
- one can think of `String` between `readFile` and `writeFile` as a pipe linking the two, data goes in one end, is transformed and comes out the other end. Therefore even for large files, the memory usage is small. 
- `interact :: (String -> String) -> IO ()` reads from standard input, applies the function to the input, and writes the result to standard output.
    ```haskell
    main = interact (map toUpper)
    -- runghc toUpper.hs < input.txt > output.txt  -- redirect input and output
    -- runghc toUpper.hs -- ENTER will output the result and wait for more input
    ```

### IO monad
- actions resemble functions
- IO actions are defined within the IO monad. Monads are a way of chaining functions together purly
- can store and pass actions in pure code
    ```haskell
    list2actions :: [String] -> [IO ()]
    list2actions = map str2action
    ```
- every element, except `let`, in a `do` block must yield an IO action
    ```haskell
    str2message :: String -> String
    str2message input = "Data: " ++ input

    str2action :: String -> IO ()
    str2action = putStrLn . str2message

    numbers :: [Int]
    numbers = [1..10]

    main = do 
        str2action "Start of the program"
        -- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
        -- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
        -- takes an IO action which is executed for each element in the list
        -- map does not execute the action
        mapM_ (str2action . show) numbers
        str2action "Done!"
    ```

### Sequencing
- `do` blocks join together actions, `(>>) :: (Monad m) => m a -> m b -> m b` and `(>>=) :: (Monad m) => m a -> (a -> m b) -> m b` are also sequencing operators 
    - `(>>)`: the first action is executed, then the second, the result is the result of the second action, e.g., `putSteLn "line 1" >> putStrLn "line 2"`
    - `(>>=)`: runs an action, pass the result to a function that returns the second action, runs the second action and return the result of the second action, e.g., `getLine >>= putStrLn` reads a line from the keyboard and displays it back
    ```haskell
    main = putStrLn "Greetings! What is your name?" >>
           getLine >>=
           (\inpStr -> putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!")
    ```

### Buffering
- writing to a disk can be thousands slower than writing to memory, even if the operation does not directly communicate with the disk (e.g., the data is cached), IO involves a system call, which is slow
- buffering reduces the number of IO requests by request a large chunk of data at once, even if the code processes one character at a time
- can manually change buffering modes
  - `Nobuffering`: no buffering, data read/write is one character at a time
  - `LineBuffering`: the input/output buffer is read/written whenever a newline character is met, or it gets too large
  - `BlockBuffering`: the input/output buffer is read/written whenever in fixed-size chunks whenever possible, unusable for interactive programs
- can check the buffering mode with `hGetBuffering` and change it with `hSetBuffering stdin LineBuffering`
- can force Haskell to write out all data in (flush) the buffer with `hFlush` (`hClose` automatically flushes the buffer). This is useful when we want to make the data on disk available as another program is reading it concurrently
