# Debugging - Part 1

* creating modules
* using IO strategically

### Using Your Own Modules

The first step to debugging is always being able to read your own code. The more organized your code is, the easier it will be to read. Creating your own modules can go a long way for keeping your code organized. Imagine if your files were organized like this in your file system:

```
.
├── Chapter5.hs      -- source file with main
└── MyModule.hs      -- module file

0 directories, 2 files
```

If this is how you organized your files, the top of your module file should look like this:

```
module MyModule
(
  Foo (..)  -- how you export your data type and all its instances
  Syn       -- how you export type synonyms
  func      -- how you export functions
) where
```

The export list is in the parenthesis. If your module needs compiler extensions, they go above the "module MyModule". The module name must match the name of the file. To import this module, you just add "import MyModule" into your source file (all of the import rules apply to your own modules).

But what if this is how your files are organized:

```
.
├── Chapter5.hs   -- source file with main
└── Modules       -- directory where your modules are stored
    └── MyModule.hs   -- your module

1 directory, 2 files
```

Now the top of your module needs to look like this:

```
module Modules.MyModule  -- this line is different
(
  Foo (..)  -- how you export your data type and all its instances
  Syn       -- how you export type synonyms
  func      -- how you export functions
) where
```

In Haskell, the period is used instead of the slash in file paths. To import this module into your Chapter5.hs source file, you would need to to use "import Modules.MyModule".&#x20;

One last tip, what if you had a second module in the Modules directory and it needed to import MyModules? Can you just use "import MyModule" since they are in the same directory? You can try but it won't work. The compiler looks for files relative to the primary source file being compiled. So in this case, it would be looking for files from the perspective of Chapter5.hs (since this is what would be loaded into GHCi or converted to binary with ghc). This means you must use the file paths relative to Chapter5.hs even if the modules are in the same directory. In other words, to import you MyModule.hs into your other module, you would need to use "import Modules.MyModule".

### Using IO Strategically

While the compiler is great for catching errors, it can't catch all run-time errors or logical errors (your function just doesn't behave as expected). Isolating side effects can make debugging your functions difficult, especially since most of your logic will probably be made up of pure functions. How can you easily debug pure functions?

Ideally, you should print out the results of each step and see if the output is what you expect. But you can't print from pure functions. The naive approach would be to just use impure functions everywhere that way you can print after each step. The better method is to just make a copy of your pure function and make that one impure. Then you can test the impure version from GHCi. Once you find the bug, you can just fix it in the pure version. Here is an example, let's say the bug is in this function:

```
foo :: Int -> String -> Int
foo n xs = ...   -- does some stuff
```

We could just convert this function to an impure function like this:

```
foo :: Int -> String -> IO Int
foo x xs = do
  ...  -- does some stuff
```

but our program relied on the foo function being pure. The compiler would throw errors everywhere if we suddenly changed it to an impure function. You would need to fix these errors just to begin testing. Instead, we should do this:

```
foo :: Int -> String -> Int
foo n xs = ...   -- does some stuff

copyFoo :: Int -> String -> IO Int
copyFoo x xs = do
  ...  -- does same stuff as foo
```

You will need to alter the syntax slightly since you are going from a pure function to an impure function. Once that is done, we can debug copyFoo and then fix the logic in foo (and delete copyFoo). We can use print in copyFoo since it is impure and we don't have to worry about integrating the impure function into the rest of the program.
