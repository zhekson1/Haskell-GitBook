# Two More Concepts

### Printf Tables

We mentioned earlier that there were other ways of formatting with the printf function. One of those methods is to influence spacing just like you can with python's printf. Here is the first method:

```
*Main> putStrLn $ printf "%10s, %2d, %13.4f" "hello" 123 pi
```

The positive numbers prior to the type letter say "this many spaces between me and the previous guy". The other method is for the negative numbers:

```
*Main> putStrLn $ printf "%-10s, %-2d, %-13.4f" "hello" 123 pi
```

The negative numbers say "this many spaces between me and the next guy". This formatting lends itself to creating tables. For example, if you made your Show instance use printf like above, then this will print a table:

```
putStrLn tableHeader -- can use printf too
mapM_ print elementsList
```

### Lenses

Depending on how you have been exploring Haskell, you may have run into an issue like this (example take from [here](https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html)):

```
data Atom = Atom { _element :: String, _point :: Point }  -- nested record
  deriving Show
  
data Point = Point { _x :: Double, _y :: Double }
  deriving Show
```

How do you update the inner record? Since Haskell variables are immutable, you can't just change a piece of state. You must change the piece and return it as a whole new variable like this:

```
shiftAtomX :: Atom -> Atom
shiftAtomX (Atom e (Point x y)) = Atom e (Point (x + 1) y)
```

So we change the x variable for Point and create a brand new Atom with it. The above function's unpacking and repacking is messy and we are only one record deep. If you nested further, your code could quickly become unapproachable. This is the general problem that lenses try to solve. Lenses do all the unpacking and repacking for you so you can keep your code cleaner. Here is what we mean (to follow along, you will need to install the [Control.Lens](https://hackage.haskell.org/package/lens-5.1/docs/Control-Lens.html) module):

```
{-# LANGUAGE TemplateHaskell #-}  -- needs this extension

import Control.Lens hiding (element) -- hiding element since our record has an element

data Atom = Atom { _element :: String, _point :: Point }  -- nested record
  deriving Show
data Point = Point { _x :: Double, _y :: Double }
  deriving Show

makeLenses ''Atom
makeLenses ''Point
```

Now we can do this:

```
*Main> atom = Atom "Chlorine" (Point 2.0 3.0)
*Main> newAtomX = atom & point.x .~ 4.0  -- replaces value at x
*Main> newAtomX' = atom & point.x %~ (+4.0)  -- applies function to value at x
*Main> newAtomX^.point  -- view Point at point in newAtomX
*Main> newAtomX^.point.x -- view x value of Point at point in newAtomX
```

We can also do this:

```
*Main> ls = [newAtomX, newAtomX']
*Main> map (\z -> z^.point.x) ls
*Main> filter (\z -> z^.point.x == 4.0) ls
```

The makeLenses function will create this ability for any record keys that start with an underscore. While lenses can make error messages from GHCi a little harder, they are so useful that we felt they were necessary to still cover for beginners. To see a more in-depth tutorial for lenses, you can check out the link where this example came from.
