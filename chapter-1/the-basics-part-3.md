# The Basics - Part 3

### Main Types

You have already seen Int, String, and Char but there are other types included in Prelude. The table below summarizes some of them:

| Type     |                                            Description                                            |                           Example |
| -------- | :-----------------------------------------------------------------------------------------------: | --------------------------------: |
| Int      |          <p>A fixed-precision integer type from</p><p><code>[-2^29 .. 2^29-1]</code></p>          |                                 9 |
| Integer  |                An arbitrary-precision integer type which has no upper or lower bound              |                            213445 |
| Float    |                                   Haskell's floating point type                                   |                              3.14 |
| Double   | Another Haskell floating point type that uses twice as much memory as Float for greater precision |                             2.214 |
| Rational |                    An arbitrary precision fractional representation of a number                   |                             5 % 1 |
| Char     |                                         A single character                                        |                               '?' |
| String   |                                           A list of Char                                          | \['H','e','l','l','o'] or "Hello" |

### Main Data Structures

The main data structures for Haskell are tuples and lists. There are no dictionaries by default like in other languages; we will see why this doesn't matter in a later chapter. The table below summarizes the rules for tuples and lists:

| Data Structure |                                          Description                                          |                                                             Examples |
| -------------- | :-------------------------------------------------------------------------------------------: | -------------------------------------------------------------------: |
| Tuple          | A grouping of elements not necessarily of different types. They have a fixed length once set. |                                        (1,2) or (1,"Hello",3.14,'8') |
| List           |          An array made up of elements of the same type. The array can shrink or grow.         | \[1,2,3] or \['H','e','l','l','o'] or \[(1,"Person1"),(2,"Person2")] |

There are only really two functions in Prelude for working with tuples and they are fst and snd. They just get the first and second element from a tuple pair (length 2) respectively. However there are a lot of useful functions and operators for lists (some you have already seen):

| Function/Operator |         Example - answer after '=>' |
| ----------------- | ----------------------------------: |
| (++)              |      \[1,2] ++ \[3,4] => \[1,2,3,4] |
| (:)               |          1 : \[2,3,4] => \[1,2,3,4] |
| head              |                head \[1,2,3,4] => 1 |
| tail              |         tail \[1,2,3,4] => \[2,3,4] |
| last              |                last \[1,2,3,4] => 4 |
| init              |         init \[1,2,3,4] => \[1,2,3] |
| take              |         take 2 \[1,2,3,4] => \[1,2] |
| drop              |                    drop 2 => \[3,4] |
| filter            |    filter even \[1,2,3,4] => \[2,4] |
| zipWith           | zipWith (+) \[1,2] \[3,4] => \[4,6] |
| map               |  map (\*2) \[1,2,3,4] => \[2,4,6,8] |
| length            |              length \[1,2,3,4] => 4 |

The map function is especially useful since it allows you to apply a function to every element in a list (a process called mapping; hence the name of the function).

### Pattern Matching on Data Structures

In Haskell, you can pattern match on data structures too. Pattern matching on tuples is simple. Here is how fst and snd can be implemented:

```
fst' :: (Int,String) -> Int
fst' (x,y) = x     -- the pattern is that of a tuple pair

snd' :: (Int,String) -> String
snd' (x,y) = y
```

Just make sure that your tuple pattern is the correct length for the tuple you want to match on. For example, if you had a tuple with four elements you could get the fourth element like:

```
fourth :: (Int,String,Char,Float) -> Float
fourth (x,y,z,a) = a
```

For lists, there are two main patterns for matching:

```
foo :: [Int] -> Int
foo [x,y,z,a] = z   -- same idea as with tuples; this matches a list of length 4

foo' :: [Int] -> Int
foo' (x:xs) = x    -- this pattern uses the (:) operator

foo'' :: [Int] -> [Int]  -- this type is different
foo'' (x:xs) = xs
```

Notice how foo' and foo'' have different type signatures despite using the same pattern. This is because of how the operator (:) works (see the table above). The x is the first element while the xs is the rest of the list. foo' is the head function from Prelude while foo'' is the tail function. If you wanted to, you can pattern match out more than the first element using (:) like this:

```
foo''' :: [Int] -> Int
foo''' (x:y:xs) = y
```

But be careful because it may not behave the way you might expect. For example, what would happen if \[1] was passed to foo'''?

There are other data structures we can pattern match on but this is enough to get us started. We will see more data structures in a later chapter.

### Recursion

Now that we know how to pattern match, we can learn the art of recursion. All loops can be represented with recursion. To put it simply, all recursion means is for a function to call itself. This would be an infinite loop:

```
infLoop :: Int -> Int
infLoop x = infLoop (x+1)     -- infinitely increment by one
```

The compiler cannot catch infinite loops so this code will still compile. The way we stop infinite loops (and thus make recursion useful) is by using pattern matching. We simply place the termination case ABOVE the loop like this:

```
infLoop :: Int -> Int
infLoop 10 = 10  -- termination case
infLoop x = infLoop (x+1)
```

Now the recursion will stop as long as it hits 10. You can also use recursion with conditionals like guards and case:

```
loop :: Int -> Int
loop 10 = 10
loop x | x > 10 = loop (x - 1)
       | x < 10 = loop (x + 1)
       | otherwise = loop 10
```

This version should always approach 10 and thus should always terminate. Let's use recursion to define our own map function for \[Int]:

```
myMap :: (Int -> Int) -> [Int] -> [Int]   -- higher order function's type signature
myMap f [] = []   -- termination case is when list is empty
myMap f (x:xs) = f x : myMap f xs  -- apply f to first element then map over rest of list
```

You may not be able to see it right away so let's expand on what is happening. Imagine we call myMap like this:

```
myMap (+2) [1,2,3,4]
```

This is what happens:

```
myMap (+2) (1:[2,3,4]) = 1+2 : myMap (+2) (2:[3,4])
                       = 1+2 : 2+2 : myMap (+2) (3:[4])
                       = 1+2 : 2+2 : 3+2 : myMap (+2) (4:[])
                       = 1+2 : 2+2 : 3+2 : 4+2 : myMap (+2) []
                       = 1+2 : 2+2 : 3+2 : 4+2 : []
                       = 3 : 4 : 5 : 6 : []
                       = [3,4,5,6]
```

As you can see, xs in the pattern becomes the empty list when there is only one element left (which will get matched to x). This means the next time myMap is called, it will stop the recursion.

### Type Variables

So far you have only see concrete types in type signatures. For instance, our myMap only works on \[Int]. But why? Shouldn't it be able to work on all lists? Yes, it should. We can make it work on all lists by changing the function type signature to use type variables like this:

```
myMap :: (a -> a) -> [a] -> [a]  -- using variables; must be lowercase
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs
```

Now our myMap can work on a list of any type. However, the returned list must be of the same type of the input list. That is what the input and output both being of type \[a] means. For instance, you can't use myMap to convert a list of Int to a list of String. Also notice how our function input type also converts something of type a into something else of the same type. In other words:

```
myMap :: (a -> a) -> [a] -> [a]
-- can match against
myMap :: (Int -> Int) -> [Int] -> [Int]
myMap :: (String -> String) -> [String] -> [Int]
myMap :: (Float -> Float) -> [Float] -> [Float]
-- etc
```

And to put it concretely, if you tried the following two functions, only the first one will work:

```
myMap (+2) [1,2,3,4]
myMap myRead [1,2,3,4]   -- myRead from earlier
```

The (+2) function returns the same type as the input so that is fine. However, myRead converts an Int to a String and thus it won't type check. We can make the myMap even more general (and thus work on both) like this:

```
myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs
```

Now we can use myRead as well. Notice how the type a can be the same type as b but it doesn't need to be. Type variables are just basic algebraic substitution.&#x20;

### Importing Other Modules

There are 5 ways to import modules into source files:

```
import Data.Char  -- imports everything

import Data.Char hiding (isDigit,toUpper)  {- imports everything except isDigit
                                              and toUpper -}
                                           
import Data.Char (isDigit,toUpper)  -- only imports isDigit and toUpper

import Data.Char as C  -- allows prefixing functions and types from this with "C."

import qualified Data.Char as C  -- forces prefixing with "C."
```

You can combine the selective import methods with the prefixing ones. That's pretty much it for importing modules. If your module has function names that clash with yours, try hiding those functions or prefixing them. As an example of what we mean by prefixing, this is how to call toUpper using the prefix:

```
C.toUpper 'a'
```

### Show and Read

The show function converts any type to a String and the read function converts a String to another type. The one constraint is that the compiler needs to know how to do this for that type. For all the types and data structures included in Prelude, the compiler knows how to use show and read. As an example of how to use them, here is a new hello function that uses read and some of the functions we have defined:

```
newHello :: IO ()
newHello = do
    putStrLn "What is your name? Enter it below."
    name <- getLine
    putStrLn (greetings name)
    case isPalindrome name of   -- tests if palindrome
      True -> putStrLn "Your name is a palindrome!"
      False -> putStrLn "Your name is not a palindrome."
    putStrLn "\nWhat is your age? Enter it below."
    old <- getLine
    putStrLn (period old)
  where isPalindrome x = palindrome x 
        period x = age (read x)   -- read is used here
```

Since the function age in line 13 expects an Int, we did not need to tell read what type to convert x to; the compiler is smart enough to figure it out. However this is not always the case. If the compiler alerts you to an "ambiguous type" (which means it doesn't know what to convert the String to), try telling it what type to use like this:

```
period x = age (read x :: Int)
```
