# A compiler for a subset of Haskell to Combinatory Logic
[![Build Status](https://travis-ci.org/siraben/mini-haskell.svg?branch=master)](https://travis-ci.org/siraben/mini-haskell)
## Adapted from the original version by Ben Lynn
This is an elaboration and annotation of [Ben Lynn's Haskell
compiler](https://crypto.stanford.edu/~blynn/compiler/type.html) and
[C VM](https://crypto.stanford.edu/~blynn/compiler/c.html).  The main
aim is to improve upon the compiler, in various layers (see [Future plans](#future-plans)).

## Features
- Valid subset of Haskell!  Code works on GHC as long as you have
  curly braces and delimiters as needed (see `classy.hs`)
- Type inference, type classes, algebraic data types
- Pure! The last function defined should be a function of type `String
  -> String`, which will receive input from stdin and whose output
  will be printed to the console
- VM (~350 SLOC) is written in ANSI C and works on 32 and 64-bit
  systems
- Compiles to combinatory logic (CL), evaluation proceeds via graph
  reduction

## Usage
```
./blynn <binary> <input> <output>
```
Where `binary` is the CL program to run, `input` is the
file whose contents are passed to the program, and `output` is the file
to write the output to.

## Building
### Requirements
- An ANSI C compiler and `make`.  That's it!

### Testing
To check self-compilation, run `./check.sh classy.hs`.  It does the
following:

- Run `classy` (compiler binary) on `classy.hs` (compiler source),
  producing `classy2`
- Run `classy2` on `classy.hs`, producing `classy3`
- Check that `classy2` and `classy3` are identical

If you've made a change to what `classy.hs` _outputs_, (e.g. an
optimization to code generation), run `./check_compile.sh classy.hs`
instead.  It adds another step to the same process in `check.sh` to
ensure that the changes propagate.

## <a name="future-plans">Future plans</a>
### Bootstrapping
- [ ] Create bootstrapping path from original classy compiler

### C runtime
- [ ] Monadic I/O
    - [ ] `putc`, `getc`, filesystems
    - [ ] Combinators should take a continuation, doing I/O then
          passing the result to the continuation
- [ ] Alternate VM in Forth?

### Compiler
Initial phase; parsing and totality, then reduce heap usage.

- [x] Use more typeclasses in this compiler
- [ ] Remove undefined, only use total functions
- [ ] "Don't pay for what you don't use" (only emit code for functions
      referenced from main)
- [ ] Convert to CPS and perform partial evaluation

### Parser
- [x] Rewrite in applicative style with typeclasses
- [x] Add block comments
- [x] Use Parsec-style parsing
- [ ] Better parser error messages
  - [ ] Writer show instance for `Msg`
- [ ] `do` notation
  - [ ] Implement layout parsing, as in [this
paper](https://www.cs.cmu.edu/~ckaestne/pdf/sle12.pdf)

### Types
- [ ] Separation of `Char` and `Int` types
- [ ] Add more types to the standard prelude
- [ ] Allow class constraint in class declaration
      like (`class Functor f => Applicative f where ...`)
- [ ] Multi-parameter typeclasses
- [ ] Dependent/linear types?
