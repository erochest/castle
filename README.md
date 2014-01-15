
# Castle

I really like having sandboxes baked into [cabal-install][cabal] (see [Cabal
Sandboxes][sandboxes] for more information).

I got tired of waiting for big packages like [Yesod][yesod] and [Lens][lens] to
compile in project after project that used them. However, I still didn't want
to install them in the user database. I wanted to maintain *some* sandboxing
among a group of projects that all share a common set of packages, but I wanted
to be able to switch from them or upgrade them easily.

That's the itch I was trying to scratch with `castle`.

It allows you to share one Cabal sandbox between multiple projects. This keeps
the package versions for all of these projects in line. It also means that you
don't have to constantly be re-installing everything, but you still get the
ability to blow away a set of packages without borking your whole system.

## Usage

First, we need to create a new Castle for a new sandbox. We'll use this one for
Yesod projects.

```bash
castle new yesod
```

Now let's change into a project where we want to use that castle and set up the
configuration for it.

```bash
cd awesome-site
castle use yesod
```

This works by putting the `cabal.sandbox.config` file for the `yesod` Castle
into the current directory. From this point on, `cabal` should pick up on the
location of the sandbox and everything should work seamlessly.

If something happens and you want to use a local, not-shared sandbox for this
project, just use the `remove` command:

```bash
castle remove
```

And if you decide that you no longer need the Castle for any project, you can
delete it.

```bash
castle delete yesod
```

## Help

You can get an overview of the commands by executing `castle --help`:

```
$ castle --help
castle - manage shared cabal sandboxes.

Usage: castle COMMAND
  Manage shared cabal sandboxes.

Available options:
  -h,--help                Show this help text

Available commands:
  list                     List sand castles.
  new                      Create a new castle.
  use                      Use an existing castle.
  current                  Display the current castle name.
  remove                   Removes the sandbox from the current directory.
  delete                   Deletes the castle.
  clear                    Clears a castle by deleting and re-creating it.
  search                   Searches for a castle with a name containing the QUERY.
```

Likewise, for more information about a specific command, call `--help` on that
command.

```
$ castle new --help
Usage: castle new CASTLE_NAME
  Create a new castle.

Available options:
  -h,--help                Show this help text
  CASTLE_NAME              The name of the castle to create.
```

## Bug Reports

Please report any bugs to the [Github Issues page][issues] for this project.

[cabal]: http://hackage.haskell.org/package/cabal-install
[issues]: https://github.com/erochest/castle/issues
[lens]: http://hackage.haskell.org/package/lens
[sandboxes]: http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html
[yesod]: http://www.yesodweb.com/

