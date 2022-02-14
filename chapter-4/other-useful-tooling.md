# Other Useful Tooling

### Graphing

The best library we have found for plotting charts in haskell is actually matplotlib. It connects directy to python's matplotlib. For basic plotting with this library, you must first install the following packages: python3-pip, python3-matplotlib, and python3-scipy. You can install these with sudo like:

```
sudo apt install python3-pip python3-matplotlib python3-scipy
```

For more complicated graphs, you may need other packages. You can check out this [link](https://hackage.haskell.org/package/matplotlib) to see what else you might need. Once installed, you may need to upgrade the matplotlib that was installed to the latest version. You can do this with:

```
sudo pip3 install --upgrade matplotlib
```

Once that is done, you can install the module with cabal. The documentation for the module can be found [here](https://hackage.haskell.org/package/matplotlib-0.7.7/docs/Graphics-Matplotlib.html). The documentation can be a bit intimidating so we will walk you through a quick example:

```
import Graphics.Matplotlib

graph :: IO ()
graph = do
  let x = [0..10] :: [Int]
  let y = map (^2) x
  onscreen $ plot x y % xlabel "Xs" % ylabel "Ys" % title "Example"
```

Ta-da! Word of caution, if you try to display multiple plots in quick succession like this:

```
graph :: IO ()
graph = do
  let x = [0..10] :: [Int]
  let y = map (^2) x
  let z = map (^3) x
  let w = map (^4) x
  onscreen $ plot x y % xlabel "Xs" % ylabel "Ys" % title "Example"
  onscreen $ plot x z % xlabel "Xs" % ylabel "Zs" % title "Example2"
  onscreen $ plot x w % xlabel "Xs" % ylabel "Ws" % title "Example3"
```

you may not see all of your graphs displayed. This is due to the way "onscreen" works. It forks the thread to try displaying the graphs concurrently. However, if the main thread terminates before one of the other threads is finished, that thread will be terminated to. To fix this, we just need to add a delay to the main thread. We can do that like this:

```
import Graphics.Matplotlib
import Control.Concurrent  -- already installed

graph :: IO ()
graph = do
  let x = [0..10] :: [Int]
  let y = map (^2) x
  let z = map (^3) x
  let w = map (^4) x
  onscreen $ plot x y % xlabel "Xs" % ylabel "Ys" % title "Example"
  threadDelay 10  -- delay main thread
  onscreen $ plot x z % xlabel "Xs" % ylabel "Zs" % title "Example2"
  threadDelay 10  -- delay main thread
  onscreen $ plot x w % xlabel "Xs" % ylabel "Ws" % title "Example3"
  threadDelay 10  -- delay main thread
```

### Gzip Compression of Files

Adding compression to files is easy. You just need the module Codec.Compression.GZip (already installed). You can find its documentation [here](https://hackage.haskell.org/package/zlib-0.6.2.3/docs/Codec-Compression-GZip.html). It can only compress ByteStrings but that means it will work with JSON and Csv files. To compress and decompress, just call "compress" after encoding (but before writing) and call "decompress" before decoding (but after reading).

### Web Scraping

For web scraping, we can use Text.HTML.Scalpel. Here is the [documentation](https://hackage.haskell.org/package/scalpel-0.6.2/docs/Text-HTML-Scalpel.html) for the module. You will need to install the module. In order to use this module, you will need to enable the OverloadedStrings extension (and if you intend to test in GHCi, you will need it enabled there too). You will also probably need the Text data type from Data.Text (already installed). The basic use is this:

```
*Main> r <- scrapeURL "https://iohk.io/en/blog/posts/page-1/" $ texts (tagSelector "h1") :: IO (Maybe [Text])
```

It will return IO (Maybe \[Text]) which is why you need OverloadedStrings enabled. Notice how it returns a Maybe type since it can fail? The scrapeURL, texts, and tagSelector functions come from the Text.HTML.Scalpel module. The "h1" String we passed to tagSelector is the HTML tag for the text we wanted to scrape. The tagSelector converts our String to the Selector type for the scraper to use.

What if the "h1" tag is everywhere in your target web page but your specific "h1" tag is the only one in a "span" tag? You can combine Selectors into one by using (//) like this:

```
*Main> r <- scrapeURL "https://iohk.io/en/blog/posts/page-1/" $ texts (tagSelector "span" // tagSelector "h1") :: IO (Maybe [Text])
```

You can even write a recursive function that does this for you like this:

```
comments :: [String] -> Scraper Text [Text]
comments xs = texts $ comments' xs
  where comments' [x] = tagSelector x
        comments' (x:xs) = tagSelector x // comments' xs
```

Don't worry about the return type, that is just what the "texts" function returns. You can also search for a tag with a specific class like this:

```
TagString "h1" @: [hasClass "author"]
```

The TagString constructor comes from the Text.HTML.Scalpel module, too. The type of the above line is Selector which means it can be combined with other Selectors using (//):

```
withClass :: Scraper Text [Text]
withClass = texts $ tagSelector "span" // (TagString "h1" @: [hasClass "author"])
```
