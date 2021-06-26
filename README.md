# modulus

A SMT solver from the ground up

## Introduction

SMT solvers are magical tools that are massively used in various domains of Computer Science & especially in formal verification. This magic comes at a price : SMT solvers are incredibly complex creatures. This can be explained for 2 principal reasons :
1. Writing a solver (in general) is incredibly hard because their principal purpose  is to provide a generic way to solve difficult problems
2. SMT solvers are capable to deal with a lot of rich theories & are thereby composed of many domain-specific solvers which theme-selfs are hard to develop.
3. Assemble all the components required to build a SMT is a real software engineering challenge

Modulus is an attempt at developing a tiny SMT in OCaml from scratch with NO dependencies other than the OCaml standard library. It requires many components :

1. A **SAT** solver which is the core of the solver used to guide the resolution by pre-enumerating potential models
2. Various domain specific solvers to decide theories such as **LIA**, **EUF** or **FP**.

## Roadmap

Modulus is not indented to be an efficient nor complete solver. It is designed to be an educative project and I'm pretty sure it will be very bad once fully developed (even reaching a quarter of the State of the Art sounds reasonably unattainable).

### DEV STEP 1

The first step is to develop rapidly a tiny SAT solver without focusing on its efficiency.
It will be the base block of the solver.

### DEV STEP 2

The second step will be to embed a tinny logic language in OCaml. This language is probably going to be a subset of the first-order logic with no other function & predicate symbols than thus of the integer arithmetic with only constants, addition & basic comparisons.

```
term ::=
  x, y, z, .... (vars)
  0, 1, 2, .... (constants)
  term + term

form ::=
  form ∨ form, form ∧ form, ¬form ... (boolean relations)
  ⊤, ⊥  (truth values)
  term = term (equality)
  term ≤ term, term < term (comparisons)
```

### DEV STEP 3

Once we have a SAT solver & a first logic language to play with, we should be able to develop a specialized solver to decide the satisfiability of formulas of the form 
`term R term` where `R` is any relation symbol.

Putting it all together, given a full formula with logical connectors in it, the SAT solver will be able to pre-compute a set of atomic formulas to be satisfied & call the specialized arithmetic solver to build a model. If the specialized solver fails, we ask the SAT solver for another boolean model and call it again and so on.

### DEV STEP 4

WIP

## Compiling the project

To compile the project, the only requirement is to have a valid ocaml installation with `dune`, then simply type `dune build` at the root.