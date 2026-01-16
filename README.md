## LambdaPC

This resository contains a work-in-progress implementation of LambdaPC, described in the following paper:

> Jennifer Paykin and Sam Winnick. Qudit Quantum Programming with Projective Cliffords. POPL 2026. https://arxiv.org/abs/2407.16801

The structure of the library is as follows:

* `lib/interface` - top-level interface to LambdaPC programs via a simple parser and typechecker
* `lib/examples` - example LambdaPC programs
* `lib/lambdaPC` - the top-level LambdaPC language, including evaluation
* `lib/typing` - type-checker for LambdaPC
* `lib/lambdaC` - the linear lambdaC language
* `lib/scalars` - operations on rings Z/Zd


### Syntax

The grammar for the LambdaPC parser is defined in lib/parser.mly. The major components of the syntax are summarized in the following grammars.

Pauli types:

```
    tau ::= Pauli | tau ** tau
```

LambdaC (vector) expressions:

```
    a ::= x                 (variables)
        | r                 (scalars in Z/Zd)
        | zero{tau}         (zero vector of type tau)
        | X | Y | Z | I     (single-qubit Pauli primitives)
        | a1 + a2           (addition of vectors)
        | a1 .* a2          (scalar multiplication)
        | [a1, a2]          (pairs)
        | ...
```

Pauli expressions:
```
    t ::= x | let x = t in t' 
        | a                                             (vectors with phase 0)
        | <a>t                                          (scale by phase a)
        | t1 * t2                                       (condensed product)
        | t ^ r                                         (scale by a power)
        | case t of {X -> tx | Z -> tz}                 (Pauli case analysis)
        | in1{tp} t | in2{tp} t                         (inject)
        | case t of {in1 x1 -> t1 | in2 x2 -> t2}       (multi-qubit case analysis)
        | c @ t                                         (apply a projective Clifford)
    c ::= lambda x : tau. t                             (projective Clifford expressions)
```

### Use

Here is an example interaction with PCLib:

```
    # open PCLib.Interface;;
    # set_dimension 4 (* default is 2 *);;
    # let qft = pc "lambda q : Pauli. case q of { Z -> X^{-1} | X -> Z }"
    # typecheck qft;;
    # eval (qft @ parse "Y");;
```

To interact with PCLib through the parser, use `open PCLib.Interface`. This exposes several important functions:

* `set_dimension : int -> unit` - Sets the dimension to be used by the below functions. Dimension must be >1. Currently only instantiated for dimensions 2,3,4, but could easily be expanded.
* `parse : string -> LambdaPC.Expr.t` - parse a string into a Pauli expression
* `pc : string -> LambdaPC.Expr.pc` - parse a string into a projective Clifford function
* `(@) : LambdaPC.Expr.pc -> LambdaPC.Expr.t -> LambdaPC.Expr.t` - apply a projective Clifford to a Pauli expression
* `eval : LambdaPC.Expr.t -> unit` - pretty-print the input expression, evaluate it to a value, and then pretty-print the result
* `leval : LambdaC.Expr.t -> unit` - pretty-print the input expression, evaluate it to a value, and then pretty-print the result
* `typecheck : LambdaPC.Expr.pc -> unit` - typecheck the input projective Clifford and prints the result.
* `omega : LambdaPC.Type.t -> LambdaPC.Expr.t -> LambdaPC.Expr.t -> unit` - evaluates omega applied to the psi components of the input LambdaPC expressions

## Installation

The repository uses the dune build system (https://dune.build/install). Operations include:

* `dune build` - build the library
* `dune exec LambdaPC` - run the `main` file, which currently calls out to `lib/examples.ml`
* `dune test` - run unit tests
* `dune utop lib` - open up the utop interpreter with lib in scope