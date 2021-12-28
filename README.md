# tinyScheme

The beginnings of an humble implementation of a scheme compiler.
Heavily inspired by
[Write Yourself a Scheme](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

# QuickStart Guide
```bash
$runhaskell Lisp.hs "(+ 2 2 (* 3 3))"
Number 13
```

# Building

```bash
ghc --make Lisp.hs
```

# Running

```bash
./Lisp "\"a string\""
```

# TODO:
 - [ ] Eliminate string in LispError if possible.
       I prefer pure types.
