# keera-board-game-scaffold

This is a Haskell board game scaffold.

Table of Contents:
  * [Installation](#installation)
  * [Documentation](#documentation)
  * [Related projects](#related-projects)
  * [Curious?](#curious)

## Installation

For installing the game you need the following packages:
   * [GHC](https://www.haskell.org/ghc/)
   * [command-line interface for cabal](https://github.com/haskell/cabal/tree/master/cabal-install)
   * SDL2, SDL2-gfx

On debian/ubuntu, you can install them with:

   ```
   $ sudo apt-get install ghc cabal-install
   $ sudo apt-get install libsdl2-dev libsdl2-gfx-dev
   ```
I recommend using a [cabal sandbox](https://www.haskell.org/cabal/users-guide/installing-packages.html#developing-with-sandboxes) for installing the game.

Here are the instructions:

```
$ git clone https://github.com/keera-studios/haskell-game-programming
$ cd examples/game-board-scaffold
$ cabal update
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal run
```

If any of these instructions do not work for you, please let me know via [issue](https://github.com/keera-studios/haskell-game-programming/issues) or [mail](chriz@keera.co.uk).

## Documentation

To try and make things as clear as possible, the code includes as much haddock
documentation and comments as we could reasonably fit. You can compile
those with:

```
$ git clone https://github.com/keera-studios/haskell-game-programming
$ cd examples/game-board-scaffold
$ cabal update
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal configure && cabal haddock --executables --internal
```

## Related projects
  * [Magic Cookies](https://github.com/keera-studios/magic-cookies), a commercial FRP game written in Haskell using Yampa that's available on iTunes and Google Play.
  * [Haskanoid](https://github.com/ivanperez-haskell/haskanoid) a Haskell breakout game implemented using the Functional Reactive Programming library Yampa.
  * [Pang a Lambda](https://github.com/keera-studios/games-pang-a-lambda) a Haskell game implemented using the Functional Reactive Programming library Yampa.

## Curious?

It seems that I catched your attention. If so let's get in contact ([issue](https://github.com/keera-studios/haskell-game-programming/issues) or [mail](chriz@keera.co.uk))! Some (but none exclusive) examples: Are you interested in ...
  * ... improving this game? Like:
    * Improving the documentation.
    * Improving the code.
    * Adding new features.
    * ... ?
  * ... using this Haskell game for educational or other purposes?
  * ... letting me know what you think about this Haskell game?
  * ... writing your own Haskell game?
  * ...?
