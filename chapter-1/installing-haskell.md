# Installing Haskell

### Installing the Developer Environment

Installing Haskell is pretty straight forward thanks to [ghcup](https://www.haskell.org/ghcup/). You do not need to be root user to install Haskell with it and it will tell you what packages are needed on your computer (these packages will need to be installed by the root user). Make sure curl is installed on your system and then execute the following from your non-root terminal:

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Append changes.

Do not install Haskell Server.

Do not install Stack.

Do not hit ENTER yet. This is where ghcup tells you what packages are required for installation. On our minimal Ubuntu 20.04, it says the following packages are required:

```
build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
```

Install those packages with your root user before hitting ENTER to continue the ghcup install.

Once ghcup is finished, restart your terminal.

### GHCi

GHCi is Haskell's interactive developer environment. To start it, execute the following in your terminal:

```
ghci
```

Your prompt should now look like this:

```
Prelude>
```

To exit GHCi, you enter ":q" into the prompt:

```
Prelude> :q
```

One extra command that may be useful is ":!". It let's you execute terminal commands from within GHCi. For example:

```
Prelude> :! ls
```

That's it! We can now start learning Haskell!
