# Command Line Stuff

### Compiling to an Executable Binary

The Haskell compiler is also capable of compiling your program to an executable that can be run from the terminal. Create a file "HelloTerminal.hs" and add the following to it:

```
main :: IO ()
main = do
  putStrLn "Hello! What is your name? Enter below:"
  name <- getLine
  putStrLn $ "Hello " ++ name ++ "!"
```

Before we convert it to an executable, make sure it works in GHCi. Once you are satisfied, you can use the terminal command "ghc" (which was installed by ghcup) to convert the source file to an executable. So in the terminal (or with ":!" in GHCi) execute this:

```
ghc HelloTerminal.hs
```

It should create a few files; one of which is the executable "HelloTerminal". The other files allow the program to be quickly re-compiled if necessary. Technically you can us ghc for development too since it has the same error messages as GHCi. However GHCi allows you to test individual functions while ghc does not.

You run the executable like you would any other executable file. It can also be run on a computer that doesn't have Haskell installed.

### Buffering...

Before we move on, there is one key difference between how GHCi executes something and how the compiled binary does. To see the difference, change your HelloTerminal.hs file to this:

```
main :: IO ()
main = do
  putStr "Hello! What is your name: "  -- this was changed to use putStr
  name <- getLine
  putStrLn $ "Hello " ++ name ++ "!"
```

First try it in GHCi. It should work great; you should be able to enter your name on the same line as the prompt. Now convert it to a binary and try executing it.

You probably see a blank line. Try entering your name anyway and hit ENTER. This is what happened for us:

```
Sarah
Hello! What is your name: Hello Sarah!
```

That's a tangled mess! This is due to buffering. There are two methods to address this: we can temporarily flush the buffer using hFlush from System.IO or we can disable buffering globally using another function from System.IO. Here is how each method would look:

```
import System.IO

-- method 1: temporarily flush buffer after prompt
main :: IO ()
main = do
  putStr "Hello! What is your name: "
  hFlush stdout  -- flush the buffer
  name <- getLine
  putStrLn $ "Hello " ++ name ++ "!"
  
-- method 2: disable buffering for your entire program
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering  -- disable buffering globally
  putStr "Hello! What is your name: "
  name <- getLine
  putStrLn $ "Hello " ++ name ++ "!"
```

If you keep buffering enabled, you will need to flush the buffer after every putStr in your program. There are trade-offs to both but as a beginner, it is probably better to just disable buffering globally.

### Command Line Arguments

It is also possible to pass in arguments when the command is executed using the module System.Enviroment (already installed). Here is how it would go:

```
import System.Environment

main :: IO ()
main = do
  args <- getArgs  -- get the arguments that were passed
  print args
```

Don't forget that since you are trying to get command line arguments, you can only test this feature from the terminal with a compiled binary. In other words, you cannot test this from within GHCi. Try passing in different numbers of arguments (including zero) and see what happens. For other things you can do, you can refer to the documentation for System.Environment [here](https://hackage.haskell.org/package/base-4.16.0.0/docs/System-Environment.html).

### Executing Terminal Commands from Withing Haskell

To execute terminal commands, we will need three functions from System.Process (already installed): callCommand, readCreateProcess, and shell. The documentation for System.Process can be intimidating ([here](https://hackage.haskell.org/package/process-1.6.13.2/docs/System-Process.html)) but it is actually pretty straight forward to use these three functions. First, let's look at the type signatures for the three functions:

```
callCommand :: String -> IO ()
readCreateProcess :: CreateProcess -> String -> IO String
shell :: String -> CreateProcess
```

The callCommand function is the simplest. You just pass in the String for the command you want to execute and that's it. It will not save the output to a variable. Here is an example:

```
callCommand "ls | grep .hs"  -- notice how you can use piping
```

For readCreateProcess, the CreateProcess type is rather complex. That is why the shell function is necessary. The shell function takes the String of the command you want to execute and converts it to the correct form for readCreateProcess. The String input in the type signature for readCreateProcess is the stdin; for most cases, you can just make it the empty string (""). The function readCreateProcess will save the results in a variable with the type IO String (it is a tagged String). Here is an example:

```
*Main> x <- readCreateProcess (shell  "ls | grep .hs") ""
*Main> x
"Chapter1.hs\nChapter2.hs\nChapter3-Csv.hs\nChapter3.hs\nChapter3-JSON.hs\nHelloTerminal.hs\n"
```

As beginners ourselves, these are the only functions we use from System.Process since they are already powerful enough.
