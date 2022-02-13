# Interacting with Files - Cont.

* Csv Files

### Csv Files

For Csv files, we will need the Data.Csv, Data.Vector, and Data.ByteString.Lazy modules as well as the OverloadedStrings and DeriveGeneric extension. You will need to install Data.Csv (if you don't remember how, refer back to [here](../chapter-2/useful-tricks.md#installing-modules-with-cabal)). Data.Csv has its own decode and encode so if you are using it in the same source file as Data.Aeson, one of them needs to be qualified. We will use the same store example to explain the concepts:

```
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Csv
import qualified Data.ByteString.Lazy as B
import Data.Vector (Vector,toList,fromList)  -- we only need these

data Item = Book { author :: String, title :: String, price :: Float } |
            VideoGame { company :: String, title :: String, price :: Float } deriving (Show,Generic)

-- CSV Instances
instance ToRecord Item
instance FromRecord Item

-- example store
store :: [Item]
store = [Book "Author1" "Book1" 12.99,
         Book "Author2" "Book2" 14.99,
         Book "Author3" "Book3" 17.99,
         VideoGame "Comp1" "Game1" 34.99,
         VideoGame "Comp2" "Game2" 45.99]
```

Notice how little we have to write for the instances? Now we can use csv files like this:

```
*Main> B.writeFile "store.csv" $ encode store
*Main> contents <- B.readFile "store.csv"
*Main> decode NoHeader contents :: Either String (Vector Item)
```

Using this method, the record will be converted without headers. When you go to decode, you need to specify that there are no headers. Don't be intimidated by the Vector, it is a recursive data type like the list. You can see more about it in the Data.Vector [documentation](https://hackage.haskell.org/package/vector-0.12.3.1/docs/Data-Vector.html). You have already seen the Either type but here is an example main function to see it in action:

```
main :: IO ()
main = do
  contents <- B.readFile "store.csv"
  let result = decode NoHeader contents :: Either String (Vector Item)
  case result of
    (Right x) -> doSomeStuff $ fromList x
    (Left x) -> putStrLn $ "There was an error: " ++ x
  B.writeFile "store.csv" $ encode store
```

What if you want a header for your csv file? This is slightly more complicated. First we will need two more instances:

```
instance ToNamedRecord Item
instance FromNamedRecord Item
```

Again, the compiler will fill in the rest for us since we are using DeriveGeneric. The end result of the next step is to do something like this:

```
B.writeFile "store.csv" $ encodeByName (fromList ["author","title","price"]) store
```

The list applied to fromList is the list of headers. THE HEADERS MUST MATCH THE RECORD KEYS EXACTLY. You will get an error if they do not match. But this causes an issue, we have two different records that have different keys. The simple fix is to filter them so you are only dealing with one record at a time. This will be an exercises for you. When you are done, you should be able to do:

```
B.writeFile "books.csv" $ encodeByName (fromList ["author","title","price"]) books
B.writeFile "videoGames.csv" $ encodeByName (fromList ["company","title","price"]) videoGames
```

If you would like to customize the headers so that they are different than the record keys (for example capitalize them), you can manually defined the ToNamedRecord instance like this:

```
instance ToNamedRecord Item where
  toNamedRecord (Book author title price) = 
    namedRecord ["Author" .= author, "Title" .= title, "Price" .= price]
    
  toNamedRecord (VideoGame company title price) = 
    namedRecord ["Company" .= company, "Title" .= title, "Price" .= price]
```

Now the headers are the capitalized versions of the record keys. You can then capitalize the headers being passed to "fromList" in the B.writeFile lines above. Everything should work (assuming you successfully filtered the store).
