# Chapter Exercises

* Github api call with readCreateProcess, convert to byteString and decode to a record
* blockfrost.io api calls and graph with matplotlib
* Web scraper and save past scrapes in a compressed json file; only display differing results; combine with github api calls from above using type classes

### Github Repo Latest Version

Github has a nice api interface for checking the latest release of open-source software. For example, here is how to check the latest cardano-node release from the command line:

```
"cardano-node" "curl -s https://api.github.com/repos/input-output-hk/cardano-node/releases/latest | jq '.tag_name'"
```

Create a program to check the latest releases for your favorite open-source software using the System.Process module. Try the following:

1. Include the jq pipe in the command passed to readCreateProcess
2. Don't include the jq pipe in the command and instead convert the resulting String to ByteString  using fromString in the module [Data.ByteString.Lazy.UTF8](https://hackage.haskell.org/package/utf8-string-1.0.2/docs/Data-ByteString-Lazy-UTF8.html) (needs to be installed) and then decode the result to a record using Data.Aeson. You may need to clean up the String result from curl.

### Blockfrost

Blockfrost is a free API service for querying things about the Cardano blockchain. Here is a link to the [documentation](https://docs.blockfrost.io) for their service. Meanwhile this [link](https://developers.cardano.org/docs/get-started/blockfrost/) gives a short tutorial for how to use it. Write a program to query different chain metrics and graph the data using matplotlib. Feel free to use either jq from the command line or convert it to a ByteString yourself like in the previous exercise.

### Basic Web Scraper

Write a program that scrapes you favorite sources for the latest postings. The job of the scraper is only to alert you to new posts. Store the past web scrapes in a compressed JSON file. Then when new scrapes are made, compare the new scrapes to the old scrapes and only display the differences.

Then combine your Github Repo program with this program using the following type class:

```
class Scrape a where
  scrape :: a -> IO [Text]
```

Since scraping blogs will use the function scrapeURL and querying github will use readCreateProcess, using type classes will allow the scrape function to behave differently based on what type the input is. For example, you can have a Github type and a Website type.&#x20;

To convert String to Text, you can use "pack" from Data.Text.
