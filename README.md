# caymon

A tool to generate Haskell monads from polynomial Cayley representations.

[![Caymon logo](https://github.com/maciejpirog/caymon/raw/master/website/logo.png)](https://pl-uwr.bitbucket.io/caymon/)

Try it in browser: [https://pl-uwr.bitbucket.io/caymon/caymon.html](https://pl-uwr.bitbucket.io/caymon/caymon.html)

You give it a polynomial, it gives you back:

- An equational theory (in the form of a type class),
- A monad that arises from this theory in the form of a composition of algebraic datatypes,
- An isomorphic monad in the form of a "bicodensity" monad.

The theory behind caymon is described in [
Equational theories and monads from polynomial Cayley representations](http://www.ii.uni.wroc.pl/~mpirog/papers/cayley.pdf) by Maciej Piróg, Piotr Polesiuk, and Filip Sieczkowski (FOSSACS 2019).

## In this repository

- A Haskell program that generates .hs files + haddock docs,
- A port to JavaScript vis GHCJS
- A website that uses the above (together with a tutorial)

## Be warned!

- :warning: CayMon is a research-level exploratory software. Reading its source code might help you understand some details, but its overall internal chaos will give you a headache. Read source code responsibly! It is because caymon is not an implementation of the algorithm described in the paper, but the paper is a result of a series of experiments that resulted in the code in this repo (and heavy QuickChecking)!

- :warning: Stack no longer supports GHCJS, so you won't build jscaymon with `stack build`. We'll need to port it to nix, but not before the release of caymon 2.0, which is coming sooner or later.

- :warning: The tutorial is not really fished yet.
