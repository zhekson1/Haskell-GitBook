# Useful Tricks

* $ and .
* partially applied functions (buckets as functions)
* lambda expressions
* operators vs functions
* record syntax
* printf
* installing modules with cabal
* mapM\_ and mapM

### $ and .

Parenthesis can get messy, especially when you are constantly nesting them. For example, what if we had a list of names (first and last) and wanted to only get the last initial of all the names? We could do this:

```
map head (map last (map words ["Sarah Smith","Mike Johnson","John Doe"]))
```

The parenthesis are starting to get messy. There is a pattern that the closing parenthesis is at the end of the line. Whenever you have this pattern, you can replace the parenthesis with "$" like this:

```
map head $ map last $ map words ["Sarah Smith","Mike Johnson","John Doe"]
```

You place the $ right where the opening parenthesis would go and the compiler will know the closing parenthesis is the end of the line. This makes nesting in Haskell much nicer since you don't need the dozens of closing parenthesis at the end of lines.

We can go further than this though. We are mapping over the same list three separate times. It would be easier if we could combine the functions head, last, and words into one function and just map once with that function. We can! By using "." we can combine (compose) functions. So our above line becomes:

```
map (head . last . words) ["Sarah Smith","Mike Johnson","John Doe"]
```

We don't need the $ anymore since we aren't feeding the result into another function. Look how much nicer that is than the first version. Also this last version is now more efficient since you only need to map over the list once. As your functions get more complex, $ and . will become your best friends.

### Partially Applied Functions

Since Haskell allows functions as inputs to other functions, it follows that you can use partially applied functions too. Try this in GHCi:

```
*Main> x = head . last . words  -- waiting for a list
*Main> map x ["Sarah Smith","Mike Johnson","John Doe"]
*Main> y = (**2)  -- exponentiation; waiting for the base
*Main> z = (2**)  -- exponentiation; waiting for the exponent
*Main> y 5
*Main> z 5
```

For the functions y and z, you can use partially applied functions to control what order the next input will be. This method only works with operators though; you'll see a method for user defined functions in a bit.

Remember our Individual data type from the last section? The Person and Dog buckets are actually functions:

```
*Main> p = Person   -- waiting on everything
*Main> p' = p "Sarah" "Smith"   -- waiting on an age
*Main> p'' = p' 10   -- fully applied
```

For constructors that don't take anything (like True and False), they are just constants.

### Lambda Expressions

Lambda expressions are just functions without a name. They are used when you need a temporary function for something. We use them a lot for mapping and filtering on lists. For example, (taken from [here](https://wiki.haskell.org/Anonymous\_function)) while we could do this:

```
addOneList :: Num a => [a] -> [a]
addOneList xs = map addOne' xs
  where addOne' x = x + 1
```

It would be nicer to do:

```
addOneList :: Num a => [a] -> [a]
addOneList xs = map (\x -> x + 1) xs
```

The part in the parenthesis is the lambda expression. Lambda expressions start with the backslash and use the right facing arrow to separate variables from the functions. You can have many variables in a lambda expression:

```
*Main> (\x y -> x ** y) 2 5
```

The 2 gets linked to x and the 5 gets linked to y in the expression. If you find yourself creating one-off local functions, try using lambda expressions instead.

### Operators vs Functions

The major differences between operators and functions are summarized in the table below:

| Kind     |                                    Description                                   |         Example |
| -------- | :------------------------------------------------------------------------------: | --------------: |
| Operator | Infix (goes between variables); defined using non-letters wrapped in parenthesis |           1 + 2 |
| Function |               Prefix (goes before variables); defined using letters              | elem 1 \[1,2,3] |

You can make operators prefix by wrapping it in parenthesis like this:

```
*Main> (+) 1 2
```

And you can make functions infix by using back-ticks (the tilde key next to the 1 on QWERTY keyboards):

```
1 `elem` [1,2,3]
```

If you wanted to use partial application on functions to control the order of inputs, you would use the back-ticks. Here is an example of how to define your own operator:

```
(-!) :: (Num a, Eq a, Ord a) => a -> a -> a -- do you understand why the constraints
(-!) x y
  | x > y = x - y
  | otherwise = 0
```

This operator is a special subtraction that can never go below zero:

```
*Main> 1 -! 2
*Main> 2 -! 1
```

### Record Syntax

Before we showed you this:

```
data Individual = Person FirstName LastName Age   -- Dog removed for simplicity

fstName :: Individual -> FirstName
fstName (Person x _ _) = x

lstName :: Individual -> LastName
lstName (Person _ x _) = x

age :: Individual -> Age
age (Person _ _ x) = x
```

The basic idea here is to be able to extract different elements from Person. There is a way of using syntax to do this for us:

```
data Individual = Person { fstName :: FirstName,
                           lstName :: LastName,
                           age :: Age }
```

It could have all been on one line but we broke it up for readability. This is called Record Syntax and functions like a dictionary called Person. The first element is still FirstName, the second element is LastName, etc. But now we don't need the helper functions because we can do this:

```
*Main> p = Person "Sarah" "Smith" 13
*Main> age p
*Main> lstName p
```

In other words, the "keys" to the record are actually functions that the compiler generates automatically. You can use "|" with records too like this:

```
data Individual = Person { fstName :: FirstName,
                           lstName :: LastName,
                           age :: Age } | 
                  Dog { petName :: PetName,
                        breed :: Breed,
                        age :: Age }
```

The nice thing about this is that you can share "key" names as long as the records are defined together with "|". If they weren't, you would have to come up with unique "keys". A word of warning though, don't accidentally use a key not found in that record; it will type check but you will get a run-time error. For example, try this:

```
*Main> p = Person "Sarah" "Smith" 13
*Main> breed p
```

Records are one way of doing dictionaries in Haskell. And it is simple to convert records to JSON and CSV files as you will see in a later chapter.

### Printf

The "printf" function from module Text.Printf is very useful for formatting prompts. To use it, import the entire module. It functions very similarly to how it does in python. Here is an example usage taken from the [documentation](https://hackage.haskell.org/package/base-4.16.0.0/docs/Text-Printf.html):

```
printf "%s, %d, %.4f" "hello" 123 pi  -- pi is hardcoded in Haskell
```

You mark variables for substitution using the %. The s expects a String, the d expects either an Int or an Integer (a whole number), and the f expects a Float or Double. For the f, you can specify how many decimal places to include. There are other ways of formatting with printf too that you can see in the documentation. We will use some of the other formatting options later.

FYI, printf is a pure function which means if you want to print the result to the terminal you will need to pass the results to either putStr or putStrLn.

### Installing Modules with Cabal

All of the modules we have seen so far (Data.Char and Text.Printf) come pre-installed with Haskell. For modules that are not installed already, we can install them with cabal. Cabal was already installed by ghcup. To install a module with cabal, we first update the cabal database by executing this in the terminal (not GHCi):

```
cabal update
```

It might take a minute. Once installed, you can then install modules like this:

```
cabal install --lib <package>
```

The package is not necessarily the same name as the module itself. For example, the Data.Aeson module (used for interacting with JSON files) has the package name "aeson" and would be installed like this:

```
cabal install --lib aeson
```

Once installed, you will need to restart your GHCi if it is currently running. By restart, we mean close it with ":q" and reopen it. Once it reopens, GHCi should be able to see the new module.

How to find the package name:

1. Search the module name followed by Haskell using a search engine
2. The first link should be of the format "hackage.haskell.org/package/\<packageName>-\<versionNumber>/docs/\<moduleName>.html"

Use the package name before the "-\<versionNumber>". Make sure to keep the capitalization of the letters the same.

If you successfully installed the module, you should be able to import it in a source file and not have any issues loading the source file into GHCi.

If you ever get this error when trying to import a module:

```
Chapter3-Csv.hs:7:1: error:
    Could not load module ‘Data.Vector’
    It is a member of the hidden package ‘vector-0.12.3.1’.
    You can run ‘:set -package vector’ to expose it.
    (Note: this unloads all the modules in the current scope.)
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
```

You can fix this by just re-running "cabal install --lib \<package>" for that module. Again, make sure to restart GHCi after.

### mapM\_ and mapM

Depending on how adventurous you have been so far, you may have encountered an error when you tried to do something like this:

```
map print [1,2,3,4]
```

While you may think this should work, it will not. This is because the map function is only for mapping PURE functions and print is an IMPURE function. You can verify this by using ":t" on map in GHCi. Do you see IO anywhere in the type signature?

Fear not, there are two mapping functions that are meant to be used with impure functions. They are mapM\_ and mapM. The table below summarizes the differences:

| Function |                                              Description                                             |
| -------- | :--------------------------------------------------------------------------------------------------: |
| mapM\_   |                map an impure function over a data structure and throw away the results               |
| mapM     | map an impure function over a data structure and save the results in the same kind of data structure |

To keep it simple, if you are mapping an impure function that returns IO () use mapM\_. Otherwise use mapM. It may be hard to come up with uses for mapM for now but you'll see real world examples in later chapters.

So before when we tried mapping print over the list, we can successfully do it like this:

```
mapM_ print [1,2,3,4]
```
