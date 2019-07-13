# While

Interpreter implementing the denotational semantics of the **While** language, as defined in Chapter 4 of the book *Semantics With Applications: A Formal Introduction*.

### Usage

Start by running
```sh
ghci while.hs
```

Parse a program, given as a string
```haskell
parse <parser> <program>
-- returns all possible parses as a list of (String, Stm) tuples
```

Evaluate the final state of a program
```haskell
eval <program>
```

Check the value of a variable at the end of the program
```haskell
value <var> <program>
```

Only single letters ('a' - 'z') can be used as variable names.

### Examples

<p float="left">
  <img src="https://imgur.com/GJ9b9Ng.gif" width="700" />
</p>
