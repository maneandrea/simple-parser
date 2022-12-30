Simple Parser
---

This is a very basic Haskell library for parsing expressions of the form `head[part1, part2, ...]` or also infix expressions
like `a1 * a2 + a3`.

It can also act as a match-replacement library with patterns that can have fixed or variable lenght. A pattern
of the form `a?` matches a single subexpression, `a??` a sequence of one or more subexpressions and `a???` zero or more.
Currently this only works for the notation without infixes operators.

This was mostly an exercise for me to learn Haskell, but comments are always welcome.

---

To build use

```
runhaskell Setup configure --ghc-option='-dynamic' --prefix=$(pwd)
runhaskell Setup build
```

or just simply

```
cabal configure && cabal build
```

The (dynamically built) executables are provided in the repository for convenience.

