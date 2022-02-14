# Chapter Exercises

### Using Printf in Show

Change the Show instance for Individual and make it use printf.

### Whole Numbers Only Please...

You've been assigned the task of adding up all of the students in a given state. You'll be given an input of the form \[\[NumberOfStudents]] where the outer list represents all the schools and the inner list represents the number of students in that school. But someone screwed up and didn't tell you what type the list will be. You know it will be whole numbers but will it be \[\[Int]] or \[\[Integer]]? Write a function sumStudents that returns the total number of students but will only work on \[\[Int]] and \[\[Integer]]. Solve the problem using two different ways:

1. Create a WholeNumber class with the function and make Int and Integer members of the class.&#x20;
2. Create a WholeNumber class that is empty (there are no functions), make Int and Integer members of the class, and then write a function that uses the class as a constraint. To write empty classes and make types members of this empty class, you do:

```
class WholeNumber a    -- there where is not needed when there is nothing to follow

instance WholeNumber Int    -- the where is not needed when there is nothing to follow
instance WholeNumber Integer
```

In both methods, experiment with making Num a super-class of WholeNumber. If Num is a super-class of WholeNumber, is the Num constraint still needed in the second method?

### The Store

You run a store that sells either books with an author, title, and price or a video game with a company (like Ubisoft), a title and a price. For this exercise, you will write a program that does the following:

1. Print an Item catalog
2. Ask the user which item to buy
3. If the item is bought, remove it from the catalog
4. Display the new catalog

You must create a data structure that represents an Item and you must create a reasonable Show instance for it (it must be user presentable). For now, you will need to hard code a starting catalog.

**Hint 1**: you can represent your store catalog as the type \[Item] and then print the whole catalog using:

```
mapM_ print <catalog>
```

**Hint 2:** in order to filter out a bought item from the catalog, you will need to do an equality check.

**Hint 3**: since Haskell is immutable, you will need to create a brand new store variable whenever updates are made.

**Extra Challenge:** Represent your store like this (yes you can nest records):

```
data Store = Store { balance :: Double,
                     inventory :: [Item] } 
```

Integrate the new data structure into your Store program and add the following features:

1. At to the store's balance after a purchase is made
2. Allow the user to ask if something is in stock and check if it is
3. Ask the user whether they are interested in movies or video games or both and only show the desired items from the catalog. The following helper functions may prove useful for this:

```
isMovie :: Item -> Bool
isMovie (Movie _ _ _) = True
isMovie _ = False

isVideoGame :: Item -> Bool
isVideoGame (VideoGame _ _ _) = True
isVideoGame _ = False
```

### Reading a Module's Documentation

System.Directory comes pre-installed in Haskell. Write a program that asks the user for a project name and then have it create a directory with that name and an empty source file in that directory. You will need to read the module's [documentation](https://hackage.haskell.org/package/directory-1.3.7.0/docs/System-Directory.html) to figure out what functions will be needed. If you are not sure what something is (like FilePath) you can click on it and it will take you to how it is defined.
