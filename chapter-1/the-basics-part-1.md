# The Basics - Part 1

### Comments

Just like in other languages, there are two different types of comments in Haskell. The first type is the one line comment which starts with "--". The one line comment does not need to start at the edge of the file. Here are some example one line comments in Haskell:

```
-- this comment covers this entire line
x = 10  -- this comment starts after the variable and covers the rest of the line
```

The other type of comment in Haskell is the multi-line comment. These comments are enclosed with "{-" and "-}". These comments can start and end anywhere in the file. Here is an example:

```
{- This is a
short comment -}
```

We covered comments first because we will use them extensively in our source files to help explain what is going on.

### Significant White Spaces

Haskell uses white spaces instead of curly brackets to determine the scope of functions. Everything with the same indentation has the same scope. We recommend not using the tab key since the number of spaces inserted by it may not be standardized across platforms. We generally use two spaces for our indents but this is arbitrary. You will eventually fine your own style.

### camelCase

Haskell uses the camelCase naming standard. You can separate words with the underscore but most people use camelCase. Functions and variables start with lowercase letters while type names are capitalized.

### Calling Functions with Variable

Parenthesis are not needed for calling and applying functions in Haskell. All you need to do is separate the function from the variables with spaces. Here are some examples:

```
sum [1,2,3]  -- adds up the numbers in the list
"This is a " ++ "broken sentence."  -- concatenates the two strings
take 3 [1,3,4,5,6,7,8] -- takes the first 3 elements of the list and drops the rest
elem 1 [1,2,3,4,5] -- checks to see if the number 1 is present in the list
```

Feel free to copy/paste those lines into GHCi. It will ignore the comments.

### Loading Source Files into GHCi

All Haskell source files end in ".hs". Create a file called Chapter1.hs (it is okay that it is empty). To load the file into GHCi, we use ":l" like this:

```
Prelude> :l <path_to_file>
```

GHCi is in the same directory in which it was started unless you changed it with ":! cd". So make sure you use the correct file path or else GHCi will not find the file. Once it is loaded, you should see this:

```
[1 of 1] Compiling Main             ( Chapter1.hs, interpreted )
Ok, one module loaded.
*Main>
```

You have successfully loaded your first Haskell source file! When a source file is loaded, all functions defined within it are available for interacting in GHCi. You'll see this in a bit but first we actually need to write a function.

### Haskell's Type System

You may be intimidated by the thought of Haskell's type system if this is your first time seeing it. However, the concepts are really quite simple. If you did well with high school algebra, this should be pretty easy.

_**All Haskell functions must return one, and only one, thing.**_ So when you see a function's type signature, the last type is the output while all the other types are the inputs. Here are some examples:

```
constantString :: String -- this is the function's type signature
constantString = "This is a string."  -- this is the function

-- this type signature says to expect one String as input
oneInputString :: String -> String
oneInputString x = "This string took " ++ x ++ " as input."

-- this type signature says to expect two Strings as input
twoInputString :: String -> String -> String
twoInputString x y = "This string took both " ++ x ++ " and " ++ y ++ " as input."
```

Notice how the functions' type signatures use "::" instead of "=". This is required by the syntax.

constantString doesn't take any inputs so it is basically just a global variable of the type String. The other two functions each take input. Add the above functions to your Chapter1.hs file.

### Reloading Source Files After Making Changes

To reload the source file, you can just enter ":r" like this:

```
*Main> :r
```

The way GHCi works, if your file successfully loads, then you didn't have any compile time errors in it. Constantly refreshing with ":r" is a good way to check your source file for bugs. We will see more of this later. For now, let's see Haskell's type safety in action.

### Haskell's Type Safety

Now that your source file has been reloaded, you should now be able to call our functions from within GHCi:

```
*Main> constantString
*Main> oneInputString "Input1"
*Main> twoInputString "Input1" "Input2" -- notice how the inputs are separated with a space
```

The above code should work just fine. But what happens if we pass in input of a type other than String?

```
*Main> oneInputString (1 :: Int) -- we tell ghci what type the literal 1 is
```

Here is the error we get:

```
<interactive>:10:17: error:
    • Couldn't match type ‘Int’ with ‘[Char]’
      Expected type: String
        Actual type: Int
    • In the first argument of ‘oneInputString’, namely ‘(1 :: Int)’
      In the expression: oneInputString (1 :: Int)
      In an equation for ‘it’: it = oneInputString (1 :: Int)
```

The first bullet tells us what happened: the function expected a String but got an Int instead. In Haskell, String is a list of Char so String and \[Char] are type synonyms. The function didn't even execute because the type system caught the bad input. If you have an error like this in your source file, the compiler will alert you to the error when you reload the source file (we will see an example of this a bit later). The compiler will be your best friend for debugging; it is as easy as reloading your source file.

### Functions as Inputs to Other Functions

A powerful thing about Haskell is that all functions can be used as inputs to other functions as long as the type system allows it. When a function can take another function as input, the top function is called a "Higher Order Function". Let's create our first higher order function:

```
doubleNum :: Int -> Int
doubleNum x = x * 2

add10 :: Int -> Int  -- notice how numbers are allowed in the function name
add10 x = x + 10

higherF :: (Int -> Int) -> Int -> Int  -- the higher type declaration
higherF f x = f x
```

In a function's type signature, you enclose the type of the input function in parenthesis like on line 7. The variable f on line 8 is the function and it will be applied to the input x. Add the above to your Chapter1.hs and reload it. Try the following:

```
*Main> higherF add10 2
*Main> higherF doubleNum 2
*Main> higherF 2 add10 -- what happens if you reorder the inputs?
```

If you tried line 3, you would get this error:

```
<interactive>:16:11: error:
    • Couldn't match expected type ‘Int’ with actual type ‘Int -> Int’
    • Probable cause: ‘add10’ is applied to too few arguments
      In the second argument of ‘higherF’, namely ‘add10’
      In the expression: higherF 2 add10
      In an equation for ‘it’: it = higherF 2 add10
```

Again the first bullet tells you what happened. The inputs must be entered in the same order as the function's type signature. So the function must come before the number in our higherF function. You may not immediately see a use for higher order functions but, as you'll see later, they are incredibly useful.

### Pattern Matching

For pattern matching, it is best to learn with an example:

```
-- this function converts an Int to a String
myRead :: Int -> String
myRead 0 = "0"
myRead 1 = "1"
myRead 2 = "2"
myRead 3 = "3"
myRead 4 = "4"
```

Notice how there are seven different function declarations for myRead. This function uses pattern matching. Pattern matching takes the input and tries to match to the first function declaration on line 3. If it fails, it tries the next one down and so on. The pattern matching will execute whatever pattern matches the input. Add the above to Chapter1.hs, reload it, and try the following:

```
*Main> myRead 0
*Main> myRead 3
*Main> myRead 4
*Main> myRead 7
```

If you tried line 4, you would have gotten this error:

```
"*** Exception: Chapter1.hs:(20,1)-(24,14): Non-exhaustive patterns in function myRead
```

Non-exhaustive patterns are one of the few run-time errors that can still occur in Haskell. It would be really annoying to have to add a myRead for every number. Thankfully, Haskell allows us to use underscores as a catchall like this:

```
myRead :: Int -> String
myRead 0 = "0"
myRead 1 = "1"
myRead 2 = "2"
myRead 3 = "3"
myRead 4 = "4"
myRead _ = "I can't count that high..."
```

Add the last line to your source file and retry "myRead 7". This time myRead will work for every number. To really hammer home the pattern matching works, what do you think this order would do:

```
myRead :: Int -> String
myRead 0 = "0"
myRead 1 = "1"
myRead _ = "I can't count that high..."
myRead 2 = "2"
myRead 3 = "3"
myRead 4 = "4"
```

If you reload the above into GHCi, the compiler will give you a hint.

### Order in the Source File

The order of functions in your source file do not matter. You can use a function before it is defined in the file. Everything gets compiled together so the compiler will be able to figure it all out.
