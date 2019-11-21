# A compiler for a subset of Haskell to Combinatory Logic
## Adapted from the original version by Ben Lynn
This is an elaboration and annotation of [Ben Lynn's Haskell
compiler](Ben Lynn's Haskell compiler and ) and [C
VM](https://crypto.stanford.edu/~blynn/compiler/c.html).  The main aim
is to improve upon the compiler, firstly its structure and later
optimizations to the generated code.

## Usage
```
./blynn <binary> <input> <output>
```
Where `binary` is the combinatory logic program to run, `input` is the
file whose contents are passed to the binary, and `output` is the file
to write the output to.

## Building
### Requirements
- A C compiler and `make`.  That's it!

### Testing
To check self-compilation, run `check.sh`.  It does the following:

- Run `classy` (compiler binary) on `classy.hs` (compiler source),
  producing `classy2`
- Run `classy2` on `classy.hs`, producing `classy3`
- Check that `classy2` and `classy3` are identical.

If you've made a change to what `classy.hs` _outputs_, (e.g. an
optimization to code generation), run `check_compile.sh` instead.  It
adds another step to the same process in `check.sh` to ensure that the
changes propagate.

## Ideas for improvement
### C runtime
- [ ] Monadic I/O
    - [ ] putc, getc, filesystems
- [ ] Alternate VM in Forth?

### Compiler
Guiding principle: if a change makes the compiler self-compile with
less heap usage, it's probably a good change.
- [ ] Use more typeclasses in this compiler (but ensure bootstrapping
      paths still work)
  + Parser combinators should be a typeclass
  + Rewrite sections in monadic style?
- [ ] Remove undefined, only use total functions
- [ ] "Don't pay for what you don't use" (only emit code for functions
      referenced from main)
- [ ] Convert to CPS and perform partial evaluation

### Parser
- [x] Add block comments
- [ ] Better parser error messages
- [ ] do-notation

### Types
- [ ] Add more types to the standard prelude
- [ ] Allow class constraint in class declaration
      like (class Functor f => Applicative f where ...)
- [ ] Multi-parameter typeclasses
- [ ] Dependent/linear types?
