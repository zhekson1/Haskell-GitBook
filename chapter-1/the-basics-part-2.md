# The Basics - Part 2

### Conditionals

There are three ways of doing conditionals in Haskell. The first way is the typical if-then-else. If-then-elses operate similarly in Haskell as they do in other languages. The only differences are: there are no elifs and elses are necessary. The necessity of elses come from the requirement that all Haskell functions must return something. Without the else, nothing would be returned in the case of a false predicate. Here is myRead from the last section that uses if-then-else instead of pattern matching:

```
myRead' :: Int -> String
myRead' x = 
  if x == 0 then "0"
  else if x == 1 then "1"
       else if x == 2 then "2"
            else if x == 3 then "3"
                 else if x == 4 then "4"
                      else "I can't count that high..."
```

Notice the significant spaces. The corresponding if, then, elses must at least be at the same level of indentations. As you can see, the pattern matching method is nicer. Whenever you have several predicates to test, it is probably better to use one of the other methods.

The next method to cover is called guards. Guards use pattern matching with predicates. Here is myRead again using guards (the pipe operator "|"):

```
myRead'' :: Int -> String
myRead'' x      -- notice how there is no "=" here
  | x == 0    = "0"
  | x == 1    = "1"
  | x == 2    = "2"
  | x == 3    = "3"
  | x == 4    = "4"
  | otherwise = "I can't count that high..."
```

The otherwise in line 8 functions exactly the same as the underscore. The myRead'' function will match top to bottom and stop and the first True.

The last method to cover is using case. Case also uses pattern matching but the syntax is different:

```
myRead''' :: Int -> String
myRead''' x = case x of
  0 -> "0"
  1 -> "1"
  2 -> "2"
  3 -> "3"
  4 -> "4"
  _ -> "I can't count that high..."
```

While the above example works with pattern matching at the function level too, pattern matching at the function level won't work when you are testing anything other than identity.&#x20;

So what are the differences between the four methods (function level pattern matching, if-then-else, guards, and case)? The table below can help summarize them:

| Method                          | Trade-Off                                                                                              |
| ------------------------------- | ------------------------------------------------------------------------------------------------------ |
| Function Level Pattern Matching | Can only match based on identity                                                                       |
| If-then-else                    | Can only match True/False and requires messy nesting                                                   |
| Guards                          | Can only match True/False but is cleanly nested since it can test many things simultaneously           |
| Case                            | Can match against anything but handling all possibilities can be tedious since it can only do one test |

To elucidate the differences between case and guards, here are two functions that both take an age and tells you what stage of life that age falls in; one uses guards and one uses case:

```
age :: Int -> String
age x
  | x <= 12 = "Pre-teen"            - test # 1
  | x > 12 && x < 20 = "Teenage"    - test # 2
  | otherwise = "Adult"             - test # 3
  
age' :: Int -> String
age' x = case x of  -- can only test one thing
  0 -> "Pre-teen"
  1 -> "Pre-teen"
  2 -> "Pre-teen"
  3 -> "Pre-teen"
  4 -> "Pre-teen"
  5 -> "Pre-teen"
  6 -> "Pre-teen"
  7 -> "Pre-teen"
  8 -> "Pre-teen"
  9 -> "Pre-teen"
  10 -> "Pre-teen"
  11 -> "Pre-teen"
  12 -> "Pre-teen"
  13 -> "Teenage"
  14 -> "Teenage"
  15 -> "Teenage"
  16 -> "Teenage"
  17 -> "Teenage"
  18 -> "Teenage"
  19 -> "Teenage"
  _ -> "Adult"
```

As you can see, this is a situation where guards are preferable. This isn't always the case though. Below is a function that tells us what the first initial is:

```
firstInitial :: String -> String
firstInitial x = case head x of -- test doesn't return True or False
  'A' -> "The first initial is A"
  'B' -> "The first initial is B"
  'C' -> "The first initial is C"
  _ -> "I don't know that letter..."
```

FYI, in Haskell, Strings use double quotes while Chars use single quotes. Since the test doesn't return True/False, we have to use case here. We could have used guards with "head x == 'A'" and so on but there are times when testing something other than True/False is better. Guards and case are both very useful so you should be comfortable writing both.

To conclude this section, here are the main conditional operators and functions for True/False tests:

| Operator/Function | Description               | Example                |
| ----------------- | ------------------------- | ---------------------- |
| <                 | less than                 | x < 4                  |
| >                 | greater than              | x > 2                  |
| <=                | less than or equal to     | x <= 4                 |
| >=                | greater than or equal to  | x >= 2                 |
| ==                | equal to                  | x == 2                 |
| /=                | not equal to              | x /= 4                 |
| &&                | and                       | x > 2 && x < 4         |
| \|\|              | or                        | x < 2 \|\| x > 4       |
| and               | all True                  | and \[True,False,True] |
| any               | any True                  | any \[True,False,True] |
| not               | negation                  | not True               |

### Purity

Haskell is very protective of side effects. Side effects are whenever the function needs to do more than just math. For example, adding one to a number AND printing the result involves side effects. A rule of thumb is: any time your program interacts with anything outside your source files, it is using side effects.

When a function uses side effects, it gets "tagged" by Haskell. The tag is usually a monad (you don't need to know what a monad is). Once the function is tagged with a monad, it cannot be un-tagged. This feature requires you to treat tagged functions slightly differently than un-tagged functions. Any un-tagged function is considered pure while any tagged function is considered impure (it is contaminated with side effects).

It may seem like it would be hard to tell the difference but it is actually as simple as telling even numbers from odd numbers. All impure functions you will likely deal with return some form of IO. Here are example impure functions:

```
putStrLn :: String -> IO () -- prints a string followed by a newline
putStr :: String -> IO () -- prints a string without a newline
getLine :: IO String -- gets a line from input
getChar :: IO Char -- gets a single Char from input
```

The () is the unit type for Haskell functions. Sometimes you have nothing to return (like after you print to the terminal) but since Haskell requires something to be returned, the unit type is used. Notice how IO is part of the output for all of these functions. The IO is basically the tag for side effects. If a function outputs something with IO, it uses side effects and is thus impure. Otherwise, it is pure. If you come across a function and you are unsure of its purity, you can use ":t" in GHCi and have GHCi tell you the type signature like this:

```
*Main> :t getLine
```

### Getting User Input

In the previous section, you learned how to tell the difference between impure and pure functions. Now we will use them by creating a function to get the user's name and print a greeting. First, let's create the greet function. The greet function will be passed the name so it doesn't need to interact with the outside world. For this reason, the greet function will be pure. Everything we have done so far has been with pure functions so just apply what you have learned. Here is the pure greeting:

```
greet :: String -> String
greet name = "Hello " ++ name ++ "! It is nice to meet you!"
```

Now for the impure hello function. The beginner friendly syntax for impure functions is slightly different than the syntax for pure functions. To make it explicit, pure functions are usually written in a declarative style while impure functions are more easily written in the imperative style. Impure functions can be written declaratively but that is not a beginner friendly method, especially if you are coming from other imperative languages.

```
hello :: IO ()
hello = do     -- notice the do
  putStrLn "What is your name? Enter it below."
  name <- getLine
  let greeting = greet name
  putStrLn greeting
```

In the hello function, we begin the function with "do" on line 2. The do is required to use the beginner friendly method for dealing with monads (the IO Tag). Then we print the String asking for the name. The putStrLn function only works on String so don't try to print an Int with it. If you want to, you can try replacing putStrLn with putStr to see the difference. Don't forget the type signature for putStr is the same as putStrLn which means you can't use non-String types with it either. The last thing to note here is that the very last function in the do block must output the same type as the parent function, which in this case is IO (). In case you need it, there is also a "return" function that will take any pure input and tag it with a monad. So the last function above could have just been "return ()".&#x20;

Lines 4 and 5 are both declaring local variables but doing it differently. The difference has to do with getLine being impure while greeting is pure. Since the result of getLine is tagged by IO, we need to get passed the tag to use the String tagged by the IO. The left facing arrow is how we do that. When we declare a local variable based on a pure function, we use "let" followed by the variable name and the equal sign.

Finally, we print the results in line 6. Notice how the hello function has a similar feel to writing a python function (minus the impure/pure variable differences). Add the above two functions to Chapter1.hs, reload GHCi, and try them out. The hello function doesn't take any input so it can be called like this:

```
*Main> hello
```

To see what happens when you don't use the left facing arrow, change the hello function to this:

```
hello :: IO ()
hello = do
  putStrLn "What is your name? Enter it below."
  let name = getLine    -- this was changed to let
  let greeting = greetings name
  putStrLn greeting
```

Try to reload it into GHCi. You should get this error:

```
[1 of 1] Compiling Main             ( Chapter1.hs, interpreted )

Chapter1.hs:101:28: error:
    • Couldn't match type ‘IO String’ with ‘[Char]’
      Expected type: String
        Actual type: IO String
    • In the first argument of ‘greetings’, namely ‘name’
      In the expression: greetings name
      In an equation for ‘greeting’: greeting = greetings name
    |
101 |   let greeting = greetings name
    |                            ^^^^
Failed, no modules loaded.
```

The error occurs because the function greet expects String but got a String tagged with IO. This is what happens if you try to mix impure functions with pure functions. The tagging in Haskell forces you to explicitly think about the side effects. Go ahead and change the hello function back to using the left facing arrow.

Also an important note, while we can call pure functions from within impure functions, the reverse is not true. The predictability of Haskell comes from the pure functions. Side effects are the only things that can mess with the predictability. For this reason, it is good practice to try to isolate side effects from your code as much as possible by having separate pure functions as opposed to local functions within impure parent functions (which you will see next).

### Locals

We just saw two methods for declaring local variables in impure functions using let and the left facing arrow. But there are more than just those two and there are methods that work within pure functions as well.

The first method we will cover here works for both pure and impure functions: the "where". Here is a palindrome function that can take a String and returns True if it is a palindrome and False otherwise:

```
palindrome :: String -> Bool  -- Bool is the type for True/False
palindrome xs = xs == xs'
  where xs' = reverse xs   -- notice how this line is indented
```

Since the scope of functions is determined by indentation, the where and the local variables must be indented further than the parent function.

You can have more than one variable created with where. Here is a function that determines is the last letter is 'y' (there are other ways to do this but humor us):

```
lastIsY :: String -> Bool
lastIsY xs = lastLetter == 'y'
  where xs' = reverse xs
        lastLetter = head xs'  -- notice how the indent matches the xs'
```

Only one where is needed despite having multiple local variables. Just make sure all of your local variables are indented the same. You can also define local functions with where. Here is a function that adds 2 and then doubles a number:

```
addThenDouble :: Int -> Int
addThenDouble x = (add x) * 2  -- we pass the x to the local function add
  where add y = y + 4  -- add takes an input
```

Just make sure your variable names don't overlap with the parent function's. When you define local functions, the local function's type signature is derived from the parent function's type signature. So since we passed the variable x to add, the compiler knows that add has the type signature of Int -> Int as well. This means the compiler can help you check local variables and functions for errors too.&#x20;

An important note, you cannot defined local variables with the left facing arrow using where. This means impure variables should be set like in the hello function. Only pure variables can be set with where. Don't confuse this for meaning that where cannot be used in impure functions; that isn't true. For example, here is the hello function from before using where:

```
hello :: IO ()
hello = do
  putStrLn "What is your name? Enter it below."
  name <- getLine
  putStrLn (greeting name)
  where greeting y = greetings y    -- right here
```

Another important note when using where, only the variables set in the parent function declaration are "in scope" for where. For example, we could have written our addThenDouble function like this:

```
addThenDouble :: Int -> Int
addThenDouble x = add * 2
  where add = x + 4   -- add is no longer a local function
```

This works because the x is in scope for add. However, in our hello function with where, the name variable is not in scope for the where which means we need to pass it as an input to the local function. On the other hand, all local variables set by where are in scope for each other. For example, we could have written addThenDouble like this:

```
addThenDouble :: Int -> Int
addThenDouble x = double  -- we can just call double here
  where add = x + 4
        double = add * 2   -- this uses the add local variable
```

Enough with where. The second method for declaring multiple variables that works for both pure and impure functions is let-in. Here is addThenDouble using let-in:

```
addThenDouble' :: Int -> Int
addThenDouble' x = let add y = y + 4
                   in (add x) * 2 -- make sure in is indented at least the same as let
```

Just as with the where, the left facing arrow doesn't work with let-in. You can define multiple variables similarly too:

```
lastIsY' :: String -> Bool
lastIsY' xs = let xs' = reverse xs
                  lastLetter = head xs'
              in lastLetter == 'y'
```

Also like with where, the same scoping rules apply to let-in. Which method you choose is up to you. We prefer using where so this guide will use where more than let-in.
