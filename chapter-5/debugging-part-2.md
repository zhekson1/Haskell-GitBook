# Debugging - Part 2

* Property Based Testing with QuickCheck
* QuickCheck with your own types

### Property Based Testing with QuickCheck

Let's say you created your own even function like this:

```
myEven :: (Num a, Eq a, Integral a) => a -> Bool  -- the Integral is req for mod
myEven x = mod x 2 == 0
```

Is there an easy way to test this function to make sure it works as expected? Will it successfully differentiate between even and odd numbers? The only real way to test it is to just pass through a bunch of numbers and see if the results match what is expected. This is beyond tedious.&#x20;

It is situations like this where property based testing comes in. We can have the compiler generate random inputs of the necessary type and test the results against a rule we specify. Let's just see it in action. Here is the rule we want to be true:

```
prop_Even :: Int -> Bool
prop_Even x = myEven x == even x  -- myEven should output the same as Prelude's even
```

Using the module Test.QuickCheck (must be installed) we can load our function and rule into GHCi and test with:

```
*Main> quickCheck prop_Even
+++ OK, passed 100 tests.   -- the output
```

The quickCheck function generated 100 different inputs and ran them through prop\_Even. To see exactly what is being tested, you can do:

```
*Main> verboseCheck prop_Even
```

Let's now change myEven to be wrong and see if QuickCheck can catch it:

```
myEven :: (Num a, Eq a, Integral a) => a -> Bool
myEven x | x == 73   = True   -- will say that 73 is even
         | otherwise = mod x 2 == 0
```

Now re-run the quickCheck function 10 times (yes 10 times). You may have seen this:

```
*** Failed! Falsified (after 99 tests):                  
73
```

QuickCheck can catch the error if it actually tests the number 73 but that appears to be a big if. The sample of 100 tests is probably too small. We can change the sample size like this:

```
*Main> quickCheck (withMaxSuccess 1000 prop_Even)
```

Now how often does it catch the error? Try increasing the sample size to 10,000. How often does it catch the error now?

The point of the above exercise was to show you both the power and limitations of QuickCheck: it is only as good as your sample size. However, it still beats entering 10,000 numbers yourself...

### QuickCheck with Your Own Data Types

You can use QuickCheck as long as it knows how to create random samples for that type. QuickCheck can create random samples for all of the types already included in Prelude. To see it yourself, you can use either the sample or sample' functions with arbitrary (included in Test.QuickCheck) like this:

```
*Main> sample (arbitrary :: Gen Int)
*Main> sample' (arbitrary :: Gen [Int])
*Main> sample (arbitrary :: Gen String)
*Main> sample (arbitrary :: Gen Float)
```

Here are the type signatures for the three functions (feel free to verify this yourself):

```
sample :: Show a => Gen a -> IO ()  -- prints the results
sample' :: Gen a -> IO [a]  -- saves the results to a list
arbitrary :: Arbitrary a => Gen a  -- randomly generates values
```

So the heart of QuickCheck is the arbitrary function but it only works on data types that are members of the Arbitrary class. If you want to use QuickCheck on your own types, you will need to make them members of this class. The way to do this is pretty straight forward since most of your types will be built up from the basic types included in Prelude:

```
type Name = String
type Age = Int

data Individual = Person Name Age deriving Show

instance Arbitrary Individual where
  arbitrary = do
    name <- arbitrary   -- the compiler knows what type based on line 10
    age <- arbitrary
    return (Person name age)
```

Now you can load this into GHCi and test it:

```
*Main> sample (arbitrary :: Gen Individual)
```

You probably got some nonsensical Strings:

```
Person "" 0
Person "}h" (-2)
Person "\SIn\121117\n" 1
Person "\ETX\rO\63121" 5
Person "{e\t<A\1021870R" 1
Person "" 6
Person "\DEL\SYN" 3
Person "\119097\v)OnT" 12
Person "8\1051631r)5:d\CANgi" 0
Person "\1082325m\287\67842" 9
Person "\1090827\STX5cg%U\142865\1067341\1081510\31136" 12
```

Those are definitely not names. Let's confine the possible Strings:

```
instance Arbitrary Individual where
  arbitrary = do
    name <- genName  -- using the new helper function
    age <- arbitrary 
    return (Person name age)
    
genName :: Gen String
genName = oneof [return "Sarah",return "Tim",return "Scott"]
```

The oneof function will randomly pick one of the elements in the list. Now you should get realistic Persons:

```
Person "Scott" 0
Person "Scott" 0
Person "Sarah" (-2)
Person "Tim" 3
Person "Tim" (-1)
Person "Scott" 8
Person "Sarah" 4
Person "Scott" (-13)
Person "Tim" (-16)
Person "Sarah" (-3)
Person "Scott" 6
```

What if you are using an either/or (Sum) type like this:

```
type Name = String
type Age = Int

data Individual = Person Name Age | Dog Name Age deriving Show
```

Well you will need a few more helper functions but you can still do it:

```
instance Arbitrary Individual where
  arbitrary = oneof [genPerson,genDog]  -- randomly choose Person or Dog

genPerson :: Gen Individual  -- generate random Person
genPerson = do
  name <- genName
  age <- arbitrary
  return (Person name age)
  
genDog :: Gen Individual  -- generate random Dog
genDog = do
  name <- genName
  age <- arbitrary
  return (Dog name age)
  
genName :: Gen String
genName = oneof [return "Sarah",return "Tim",return "Scott"]
```

Now it will randomly generate a Person or a Dog.
