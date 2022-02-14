# Type Behavior

### Deriving Default Behavior

When you first create a new type with the "data" key word, while you can pattern match on it, you can't do much else. You can't print it, you can't do equality checks, you can't order them, etc. It is best to see this in action. In a source file, add the Individual type from the last section and load it into GHCi. Here it is again for convenience:

```
type FirstName = String
type LastName = String
type Age = Int
type PetName = String
type Breed = String

data Individual = Person FirstName LastName Age | Dog PetName Breed Age
```

Now in GHCi, execute the following in order:

```
*Main> x = Person "Sarah" "Smith" 42
*Main> y = Dog "Max" "Husky" 4
*Main> z = Person "John" "Doe" 31
*Main> x
```

Everything will be fine until you try to call x (which in GHCi means to print it). You should get this error:

```
<interactive>:53:1: error:
    • No instance for (Show Individual) arising from a use of ‘print’
    • In a stmt of an interactive GHCi command: print it
```

The first bullet says "No instance for (Show Individual)". It layman terms, this means that the compiler doesn't know how to convert our new type to a String for printing. We can make the compiler use default printing settings by adding a "deriving" clause to our data type creation like this:

```
data Individual = Person FirstName LastName Age | Dog PetName Breed Age
  deriving (Show)
```

It could have been on the same line but we broke it over two to avoid using one long line. Now reload the source file. Your set variables will be erased when you reload but there is a command history in GHCi so just hit the up arrow a few times to re-set them. Now printing x will work. Take a look to see what the default printing looks like.

If you want to use (==) or (/=) with your types, you will need to derive Eq and if you want to use any of the ordering (<,>,etc) you need to derive Ord. Here is how the syntax looks with multiple derivations:

```
data Individual = Person FirstName LastName Age | Dog PetName Breed Age
  deriving (Show,Eq,Ord)
```

FYI, for Sum types (either/or), the one defined first is always seen as less than the one defined after. So if we compared x to y from above, y would be greater because it is a Dog bucket. If they were equal, then the first item in the bucket is compared, then the next one and so on. So based on this reasoning, False is less than True. Feel free to confirm this.

### Custom Behavior

To create custom behavior, you need to understand a bit about type classes. Classes in Haskell are nothing like classes in other languages. The basic idea for Haskell classes is this: I want my function to behave differently depending on what the input type is. That's it. Whenever you want a function to behave differently depending on the input type, you can create a type class. As an example, the Show class just has the function show. And yet the conversion from String to a type is unique for each type. Here is how the Show class is defined:

```
class Show a where
  show :: a -> String
```

The a in line 1 is the same a in line 2. We just do algebraic substitution once a type becomes a member of the class. We can make a type a member like this:

```
instance Show Individual where  -- the a is now set to Individual
  show (Person fst lst age) = "Person: " ++ fst ++ " " ++ lst ++ "\nAge: " ++ show age
  show (Dog name breed age) = name ++ " the " ++ breed ++ "\nAge: " ++ show age
```

To use this, make sure to remove the deriving Show; you can't both derive a class and have an instance for it. Add the instance to your source file and try printing the variables from before. They should be different. We did not need to define the type signature for show since it is defined in the class Show. The Individual type get substituted in for a so the type of show in our instance is Individual -> String.

For other classes already included in Prelude, when you create the custom instances, only a subset of the functions in the class needs to be defined; the rest of the functions derive their behavior from the subset. For example, to create an Eq instance, only (==) or (/=) needs to be defined even though they are both functions in the Eq class. The reason for this is that the other can be derived from the one you create. Here is how a custom Eq instance might look:

```
instance Eq Individual where
  (Person fst lst age) == (Person fst2 lst2 age2) = fst == fst2 && lst == lst2 && age == age2
  (Dog name breed age) == (Dog name2 breed2 age2) = name == name2 && breed == breed2 && age == age2
  _ == _ = False
```

So if the buckets are the same, they are equal as long as each element in the bucket is equal. Otherwise, they are not equal.

To create a custom Ord instance, the only function you need to define is (<=). The rest will be derived from (<=). For example:

```
instance Ord Individual where
  (Person _ _ age) <= (Person _ _ age2) = age <= age2
  (Dog _ _ age) <= (Dog _ _ age2) = age <= age2
  (Person _ _ _) <= (Dog _ _ _) = True
  (Dog _ _ _) <= (Person _ _ _) = False
```

So if the buckets are the same, we go by who is older. If they are different, Dogs are greater. One thing to note, in order to compare things with the Ord class, there must also be a way to test equality. This means that for something to be a member of the Ord class, it must also be a member of the Eq class. To see this in action, just comment out the Eq instance for Individual and try to reload the source file. Eq is a "superclass" for Ord. What this means in practice is that the Ord class definition looks like this:

```
class Eq a => Ord a where
  {- bunch of functions -}
```

Anything before the "=>" is used to constrain what comes after. This says that whatever gets passed to the variable a must also be a member of the Eq class.

There are other type classes predefined in Prelude like Num. To see information on how they are defined, what the necessary subset is for custom instances, and what types already have instances for that class, you can use ":info" in GHCi. For example, ":info Num" yields:

```
type Num :: * -> Constraint
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
  	-- Defined in ‘GHC.Num’
instance Num Word -- Defined in ‘GHC.Num’
instance Num Integer -- Defined in ‘GHC.Num’
instance Num Int -- Defined in ‘GHC.Num’
instance Num Float -- Defined in ‘GHC.Float’
instance Num Double -- Defined in ‘GHC.Float’
```

Line 10 is where the required subset is listed. So you would need to define (+), (\*), abs, signum, fromInteger, and either negate or (-).

### Constraining Behavior

You have already seen an example of this with the Ord class definition. The basic idea for constraining behavior is that you want your function to work on multiple types but using type variables would be too general. For example, what if you had this function:

```
addThenDouble :: a -> a -> a
addThenDouble x y = (x + y)*2
```

This function should work on all numbers. But the type signature still allows for other types like Strings and lists. In other words, the type variable a is too general but using concrete types like Int is too specific. We need a sweet spot.

Luckily, fixing this is easy:

```
addThenDouble :: Num a => a -> a -> a  -- this is the syntax; same as above
addThenDouble x y = (x + y)*2
```

The above type signature says that the type variable a MUST also be a member of the Num class. Notice how the syntax is the same as with the class definition constraint for Ord. This rules out String and lists now but still allows the function to be used with Int, Float, Double, etc. You can think of this method as another way of adding a function to a class definition like how the show function is in the Show class definition.

Why would you prefer this method over actually adding the function to the type class definition? To potentially make your program more concise. Let's say we added addThenDouble to the Num class when the class was created like this (don't do this since you will class with the real Num class):

```
class Num a where
  {- other functions -}
  addThenDouble :: a -> a -> a
```

Now we need to make an instance of Num for every type of number (Int,Integer,Float,etc):

```
instance Num Int where
  {- other function declarations -}
  addThenDouble x y = (x + y)*2
  
instance Num Integer where
  {- other function declarations -}
  addThenDouble x y = (x + y)*2
  
instance Num Float where
  {- other function declarations -}
  addThenDouble x y = (x + y)*2
  
instance Num Double where
  {- other function declarations -}
  addThenDouble x y = (x + y)*2
  
{- etc -}
```

The function declaration for addThenDouble is the same for every member of the class. Adding it to the class definition forces us to repeat the code for every member of the class. On the other hand, by creating the function outside of the class definition and just constraining it with the class, we only need to write the function once.

Here is a good rule of thumb: if you want the same behavior for EVERY member of a class, create the function outside the class definition and constrain it. If not, you will need to add the function to the class definition and create instances for every member.

What if your function needs to be constrained by more than one class? Here is an example of how to do that:

```
test :: (Num a, Eq a) => a -> a -> Bool   -- the () is necessary
test x y = (x + y)*2 == x*2 + y*2
```

The syntax is the same for constraining classes too. The compiler can help you figure out what constraints are needed based on what functions and operators are being used. For example, if you did not include both the Num and Eq constraints in the test function above, the compiler will give this error upon trying to reload:

```
Chapter2.hs:50:12: error:
    • No instance for (Num a) arising from a use of ‘*’
      Possible fix:
        add (Num a) to the context of
          the type signature for:
            test :: forall a. a -> a -> Bool
    • In the first argument of ‘(==)’, namely ‘(x + y) * 2’
      In the expression: (x + y) * 2 == x * 2 + y * 2
      In an equation for ‘test’: test x y = (x + y) * 2 == x * 2 + y * 2
   |
50 | test x y = (x + y)*2 == x*2 + y*2
   |            ^^^^^^^^^

Chapter2.hs:50:12: error:
    • No instance for (Eq a) arising from a use of ‘==’
      Possible fix:
        add (Eq a) to the context of
          the type signature for:
            test :: forall a. a -> a -> Bool
    • In the expression: (x + y) * 2 == x * 2 + y * 2
      In an equation for ‘test’: test x y = (x + y) * 2 == x * 2 + y * 2
   |
50 | test x y = (x + y)*2 == x*2 + y*2

```

In the first bullet of both errors, it says "Possible fix: add (\_ a) to the context of the type signature". Now you know how to do that.

### The Print Function

You saw the print function in the Chapter 1 Exercises. The definition of print is this:

```
print :: Show a => a -> IO ()
print x = putStrLn (show x)
```

You can confirm this by using ":t" on print in GHCi. As you can see, print can only be used on types that are members of the Show class. All of the types from Chapter 1 are members of the Show class. When you create your own data types, you will need to make them members of the Show class by creating Show instances in order to be able to print them with the print function. Also when you call a variable by itself in GHCi, it assumes you mean "apply print to this variable".
