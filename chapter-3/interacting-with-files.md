# Interacting with Files

### Basic Text Files

The module System.IO comes pre-installed with Haskell and can be used for reading and writing to basic files. There is a lot going on in the [documentation](https://hackage.haskell.org/package/base-4.16.0.0/docs/System-IO.html) but we will walk you through the basics. Also did you figure out what the type FilePath is?

The way file reading in Haskell works is to first create a handle and then read from or write to that handle. When finished, make sure to close the handle. The basic idea is this:

```
-- open Text.txt and prepare it for writing; if doesn't exist, it will create it
*Main> handle <- openFile "Text.txt" WriteMode

-- writing to the file
*Main> hPutStr handle "hello\nworld"

-- closing the file
*Main> hClose handle

-- open Text.txt and prepare it for reading; file must already exist
*Main> handle <- openFile "Text.txt" ReadMode

-- read the file
*Main> contents <- hGetContents handle

-- print contents
*Main> putStrLn contents

-- close file
*Main> hClose handle
```

When you write to the file, the contents are not actually dumped to the file until hClose is called. In a similar fashion, when you read from a file, the contents are not actually read until you go to do something with them like print them. To see what we mean, re-open the file for reading but this time close the file before printing the contents. You'll get this error:

```
*** Exception: Text.txt: hGetContents: illegal operation (delayed read on closed handle)
```

This is Haskell's laziness in action. As another example of laziness, if you try to write to a file before it has been fully read, you will get this error:

```
*** Exception: Test.txt: openFile: resource busy (file is locked)
```

Don't worry, it isn't that hard to get used to.

The other options you can use besides ReadMode and WriteMode are AppendMode and ReadWriteMode. If you try writing to a file in ReadMode or reading from a file in WriteMode you will get an error.&#x20;

Now that you know the basics, you can also use readFile, writeFile, and appendFile from System.IO too (they are also included in Prelude). They deal with opening and closing the handles for you. However be aware that they still use laziness. As an example usage of these functions:

```
*Main> contents <- readFile "Text.txt"
*Main> putStrLn contents  -- necessary step due to laziness
*Main> writeFile "Text.txt" "hello"
```

If you need to write to the file before it is "unlocked", you can either write your content to a tmp file, delete the old file, and then rename the tmp file (using functions from System.Directory) or you can use this hack:

```
foo :: IO ()
foo = do
  contents <- readFile "Text.txt"
  seq (length contents) $ writeFile "Text.txt" "Hello"
```

The "seq" function forces the first argument and then returns the second one. In other words, "length contents" is strictly evaluated which then allows you to successfully write to the same file being read. This hack is only necessary for dealing with text files. Later when we deal with JSON files and Csv files, there is a nicer solution you can use.

### Slight Tangent: Data Constructors as Options

Did you notice that ReadMode, WriteMode, AppendMode, and ReadWriteMode in the documentation are all data constructors? This is a common practice in Haskell. While you could use something like Int as the feature decider, there are probably more Ints than you will have in features. By creating data constructors specifically for feature selection, you can benefit from type safety without having to worry about too many possible inputs. If you also need an extra variable for one feature, you can have the corresponding data constructor contain the variable. For example:

```
data Option = AddOne | AddTwo | RaiseToPower Int -- takes an extra variable

foo :: Int -> Option -> Int
foo n x = case x of
  AddOne -> n + 1
  AddTwo -> n + 2
  (RaiseToPower y) -> n ^ y  -- exponentiation for Ints
```

So even though our function foo only takes 2 variables, we can pass in a third by putting it into the RaiseToPower bucket. This is still type safe.

### GHC Extensions

GHC extensions are basically add-ons that can be enabled for the compiler. For example, let's say you want to be able to use double quotes for other string-like types such as Text and ByteString. Well there is an extension you can easily enable to do that. At the top of the source file that you need it for, add this:

```
{-# LANGUAGE OverloadedStrings #-}
```

These extensions should go at the top so that the compiler turns on the features prior to parsing your code. With that extension enabled, you can now use double quotes for other string-like types. You should note though that the compiler may not be able to tell what type a double quote is now since it can't just assume String. So if you get an "ambiguous type" error, just tell the compiler what type the double quotes are being used for.

When you have multiple extensions to enable, you can add them one after the other like this:

```
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
```

However, just because your source file has these extensions enabled doesn't mean GHCi will enable them upon loading your source file. You need to enable them separately in GHCi like this:

```
*Main> :set -XOverloadedStrings
*Main> :set -XBangPatterns
*Main> :set -XDeriveGeneric
```

Once you enable the extensions in GHCi, they will remain enabled until you close GHCi. We covered this now because you will need extensions for working with JSON and CSV files.

### JSON Files

There are three new modules we will use: Data.Aeson, Data.ByteString.Lazy and GHC.Generics. Data.Aeson needs to be installed with cabal so do that now if you haven't yet. Converting to and from a JSON file is very simple when working with record syntax. All you will need are the two previously mentioned modules and the DeriveGeneric extension in your source file (we won't need it in GHCi). Here is an example source file:

```
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B  -- qualifying is necessary

data Item = Book { author :: String, title :: String, price :: Float } |
            VideoGame { company :: String, title :: String, price :: Float } deriving (Show,Generic)
             
-- JSON instances
instance ToJSON Item where 
  toEncoding = genericToEncoding defaultOptions
  
instance FromJSON Item

-- example store
store :: [Item]
store = [Book "Author1" "Book1" 12.99,
         Book "Author2" "Book2" 14.99,
         Book "Author3" "Book3" 17.99,
         VideoGame "Comp1" "Game1" 34.99,
         VideoGame "Comp2" "Game2" 45.99]
```

So we enabled the extension and then imported the modules. Data.ByteString.Lazy needs to be qualified because it has a lot of functions with the same name as functions in Prelude. Then we created our Item records and derived both Show and Generic. Look at the JSON instances. This is all you need to put when you derive Generic like this. The compiler will automatically fill in the instances for you. If you want to create the instances manually, you can check out the Data.Aeson documentation [here](https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson.html). We created the example store so we have something to play with.

To interact with JSON files, the basic idea is this:

```
*Main> B.writeFile "store.json" $ encode store
*Main> contents <- B.readFile "store.json"
*Main> decode contents :: Maybe [Item]
```

We use the writeFile and readFile functions from Data.ByteString.Lazy so we prefix the functions with B. This is due to JSON files being stored as ByteStrings. The functions encode and decode come from Data.Aeson. In line three, we need to tell decode what type to try decoding to. It must be wrapped in a Maybe since that is how decode handles cases where it fails to decode something. To see what we mean, first check out how your JSON file looks. Ours looks like this:

```
[{"tag":"Book","author":"Author1","title":"Book1","price":12.99},{"tag":"Book","author":"Author2","title":"Book2","price":14.99},{"tag":"Book","author":"Author3","title":"Book3","price":17.99},{"tag":"VideoGame","company":"Comp1","title":"Game1","price":34.99},{"tag":"VideoGame","company":"Comp2","title":"Game2","price":45.99}]
```

It is all on one line. Notice how the JSON dictionary labels the constructors as tags? To see what happens when decode fails, change one of the keys in the JSON file so that it is a typo. Then reread the file and try decoding again. We get:

```
Nothing
```

The JSON dictionary has to exactly match the record in your source file our else it will fail. Here is an example main function that uses all of this:

```
main :: IO ()
main = do
  contents <- B.readFile "store.json"
  let result = decode contents :: Maybe [Item]
  case result of
    (Just x) -> doSomeStuff x
    Nothing -> putStrLn "There was an error decoding the file."
```

Using the Maybe type makes handling errors trivial. Before we finish this section, there is one more thing to cover: the laziness of the functions. What if you had this:

```
main :: IO ()
main = do
  contents <- B.readFile "store.json"
  let result = decode contents :: Maybe [Item]
  B.writeFile "store.json" $ encode store
```

You are decoding the contents in line 4 so the file should be released right? Nope:

```
*** Exception: store.json: openBinaryFile: resource busy (file is locked)
```

The decoding is done lazily so it only decodes as things are needed which means we technically didn't read the contents yet. While we could use one of the solutions mentioned with text files, there is a third option here. We could enable the BangPatterns extension (see above) and then do this:

```
main :: IO ()
main = do
  contents <- B.readFile "store.json"
  let !result = decode contents :: Maybe [Item]  -- notice the !
  B.writeFile "store.json" $ encode store
```

The bang (!) on line 4 forces the decoding to be done strictly. Now we can write to the file immediately. The reason why we couldn't use BangPatterns before was due to how many layers of laziness are involved in reading text files. The bang can only eliminate one layer of laziness. Even the B.readFile has too many layers of laziness; this would fail:

```
main :: IO ()
main = do
  !contents <- B.readFile "store.json"
  B.writeFile "store.json" $ encode store
```

We know this might be annoying but there are benefits to laziness so we need to learn to accept the trade-offs. Just keep it simple, if you are using "decode", then you can also use BangPatterns, otherwise you'll probably have to use another method. Thankfully both JSON and Csv use a decode so that makes things simple.
