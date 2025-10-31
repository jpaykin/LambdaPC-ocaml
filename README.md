## LambdaPC

This resository contains a work-in-progress implementation of LambdaPC, described in the following paper:

> Jennifer Paykin and Sam Winnick. Qudit Quantum Programming with Projective Cliffords. 2026. https://arxiv.org/abs/2407.16801

The structure of the library is as follows:

* `lib/lambdaPC` - the top-level LambdaPC language, including evaluation
* `lib/lambdaC` - the linear lambdaC language
* `lib/scalars` - operations in the rings Z/Zd
* `lib/examples` - example LambdaPC programs

## Installation

The repository uses the dune build system (https://dune.build/install). Operations include:

* `dune build` - build the library
* `dune exec LambdaPC` - run the `main` file, which currently calls out to `lib/examples.ml`
* `dune test` - run unit tests