# Creating Types

* Type synonyms
* your own data types
* Your own data types cont.
* types that take variables
* Recursive Types
* Pause

### Type Synonyms

Let's say you had a function that took a price and a wallet balance and returned whether you had enough money to buy the item. Your function could like like this:

```
haveEnough :: Float -> Float -> Bool
haveEnough price balance
  | price > balance = False
  | otherwise = True
```

The only potential drawback is that if you only look at the type signature, it isn't clear what this function is doing. It would be much better to able to glance at the type signature and get a basic idea of what the function is doing.

We can make the type signature more descriptive by using type synonyms. A type synonym you have already seen is String. You can use String and \[Char] interchangeably throughout your program. We can easily create our own type synonyms using the "type" key word like this:

```
type Price = Float
type Balance = Float
type CanBuy = Bool
```

Now with these synonyms in scope, we can change our haveEnough function to this:

```
haveEnough :: Price -> Float -> CanBuy  -- this is more descriptive
haveEnough price balance
  | price > balance = False
  | otherwise = True
```

The compiler will know what we mean by those types as long as the type synonyms are in scope.

### Your Own Types and Data Structures

The type synonyms work great as long as we can use a type that already exists. If not, we can create the type using the "data" key word. In Haskell, types and data structures are basically one in the same so they are defined the same way. Here is an example data type we can make (albeit a very useless one):

```
data Useless = VeryUseless
```

In the line above, "Useless" is the type name and "VeryUseless" is called the data constructor. The type and the constructor can be named the same thing but they are functionally different. The type name (Useless) goes in type signatures while the data constructor (VeryUseless) is used in actual functions. For example:

```
uselessFunc :: Useless -> String
uselessFunc x = case x of  -- notice how you can pattern match on the constructor
  VeryUseless -> "This was pointless."
  _ -> "Whatever."
```

This will compile but the compiler will warn you that the underscore is redundant since x can only every be VeryUseless. We can add some functionality by giving Useless more possibilities. In Haskell, you can create types that can be one form or another and yet still be the same type. For example, we can change our Useless type creation to this:

```
data Useless = VeryUseless | LessUseless  -- that is the pipe operator
```

Now our Useless type has two possible forms. To help illustrate this, here is how the Bool type is defined:

```
data Bool = False | True
```

If you can remember how the Bool data type is defined, you can use it as a guide for how to use your own data types: the name goes in type signatures while the constructors go in the function. Also just like how you can pattern match on True and False, you can also pattern match on your own constructors.

### Your Own Types and Data Structures Cont.

Your types aren't confined to just being either/or. You can also have a type that has one thing AND another. For example:

```
type FirstName = String
type LastName = String
type Age = Int

data Individual = Person FirstName LastName Age
```

Here our Individual type is a Person with a FirstName, a LastName, and an Age. You can think of the "Person" as a bucket that holds the FirstName, LastName, and Age. To go with this analogy, True and False are just empty buckets. You can also pattern match on the Individual type like this:

```
fstName :: Individual -> FirstName
fstName (Person x _ _) = x

lstName :: Individual -> LastName
lstName (Person _ x _) = x

age :: Individual -> Age
age (Person _ _ x) = x
```

Remember that FirstName, LastName, and Age ARE NOT CONSTRUCTORS. They are type synonyms which is why they can go in the function's type signature. As for the underscores, they mean "ignore this element". You can use underscores when you pattern match on other things too, including elements in lists and tuples. However you CANNOT use underscores in place of bucket names; it is either the entire bucket together or individual elements in the bucket. In other words, the compiler would throw and error if you placed an underscore where Person is. But this is okay:

```
age' :: Individual -> Age
age' _ = 12
```

You can combine either/or with the and types. Let's add a dog as another option:

```
type FirstName = String
type LastName = String
type Age = Int
type PetName = String
type Breed = String

data Individual = Person FirstName LastName Age | Dog PetName Breed Age
```

Now our Individual type can either be a Person with a first name, last name, and age OR a dog with a name, breed, and age. The fact that Haskell has the option for "either/or" and "and" in data types eliminates a lot of mess found in other languages. FYI, the technical name for "either/or" and "and" types are Sum and Product types, respectively.

### Types That Take Variables

What we have seen so far are constant types (i.e. they don't change from context to context). However, it is possible for a data type to take another type as input too. For example:

```
data Foo a = FooConst a
```

Now when we use this type, we pass the type for a in the function's type signature like this:

```
fooFunc :: Foo Int -> Int
fooFunc (FooConst x) = x
```

We could have also left the function more general by passing in a type variable like this:

```
fooFunc' :: Foo a -> a
fooFunc' (FooConst x) = x
```

Both are valid. Just make sure to include the type input if you are using a type that takes an input. To help illustrate this, let's see two data types included in Prelude that take input. The first one is the Maybe type and is defined like this:

```
data Maybe a = Nothing | Just a
```

The Maybe type is used when a process can fail. The Nothing constructor is used in place of nulls in other languages. If the process succeeds, the result is returned inside a Just bucket.&#x20;

The other type to see is also used if a process can fail. The type is the Either type and is defined like this:

```
data Either a b = Left a | Right b  -- notice how it takes two inputs
```

Here, the a would be the type of an error message while the b is the type of the successful result. While Either and Maybe are both used for errors, Either allows us to attach an error message to the failure while Maybe doesn't.

### Recursive Types

If you thought the fact types can take input was cool, you'll like this too. Types can be recursive. Here is how to think of the List data structure:

```
data List a = [] | a : List a
```

Or for a binary tree you can do:

```
data Tree a = Leaf a | Branch (Tree a) (Tree a)
```

Remember that recursion just means it calls itself until a termination case is reached.

### Pause and Reflect

Don't worry if you don't understand all of this. The only concepts you need to understand right now are type synonyms and declaring your own constant sum and product types. The rest of the concepts was just so that you are aware of them. Also if you haven't noticed by now, all type names and data constructors start with a capital letter while all all functions and variables start with a lower case.&#x20;
