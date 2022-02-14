# Chapter Exercises

### Your Own Functions

Write your own version of the following functions (make sure to use a different name). Try to use local variables when appropriate. If you aren't sure what they do, play with them a bit in GHCi.&#x20;

1. head :: \[a] -> a
2. take :: Int -> \[a] -> \[a]
3. drop :: Int -> \[a] -> \[a]
4. last :: \[a] -> a
5. init :: \[a] -> \[a]
6. tail :: \[a] -> \[a]
7. filter :: (a -> Bool) -> \[a] -> \[a]
8. zipWith :: (a -> a -> b) -> \[a] -> \[a] -> \[b]
9. length :: \[a] -> Int   -- Hint: local functions can take more inputs than the parent; use a local counter
10. even :: Int -> Bool
11. odd :: Int -> Bool
12. not :: Bool -> Bool
13. reverse :: \[a] -> \[a]
14. sum :: \[Int] -> Int

### Collatz Conjecture

The collatz conjecture simply stated is: if the number is even then divide by two, else triple it and add one. Then reapply the collatz to the new number. Write a collatz function that will keep applying this rule until 1 is reached (it will always reach 1). To divide an Int, you will need the "div" function from Prelude. Aggregate the results into a list so that you can see the full evolution of the number. For example:

```
collatz 7 => [7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]
```

### Nth Fib

The fibonacci sequence is where the next number is the sum of the preceding two numbers. Write a function to take the nth number from this sequence. The first few numbers in the sequence are:

```
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]
```

You will need two termination cases for this: one for fib 0 and one for fib 1. For an example usage:

```
fib 10 => 55
```

### How Many Vowels?

In Prelude, there is a function called "words" that takes a String and converts it to a list of Strings broken up by the words. For example:

```
words "Hello World" => ["Hello","World"]
```

Using words, create a function to count the number of vowels (not including 'y' or 'Y') in a sentence. While it can be done without using words, using words makes it more challenging. If you get stuck, try doing it without using words first. Here is an example usage:

```
vowels "Hello World" => 3
```

### Caesar Cipher

The caesar cipher shifts each letter in a message by a key. For example, if the key was 2, the message "apple" would become "crrng". The 'a' was shifted 2 places to 'c' and so on. Write a function that takes a message and a key and returns the encrypted message. You can either limit the possible letters to the alphabet or you can use "chr" and "ord" from Data.Char to convert a letter to the ascii code and then back after making a translation. You can use the function "mod" from Prelude to do modular arithmetic so that you can wrap around the end of the possible letters (not all ascii codes are printable characters).

### Program Menu

Write a recursive display function that takes a \[String] and displays each item one at a time with an index. The idea is to allow users to enter the index of the corresponding feature and have the program use that for deciding what to do. Here is an example of how it should work:

```
*Main> display ["Add Member","Query Database"]
0  Add Member
1  Query Database
```

**Hint 1:** Since you are passing along a counter for the index, you should probably use a local function.

**Hint 2:** Your termination case should probably execute "return ()" since there is nothing to do.

**Hint 3:** You can define local impure functions with where by using "do" like how you normally would.

### User Input

For the collatz conjecture, nth fib, vowels, and cipher create corresponding impure functions to get user input and display the results to the terminal. To print things other than String to the terminal, you can use the function "print" from Prelude. After that, try combining the impure functions into one by using your program menu from the previous exercise.
