```ocaml
type value = ..
type value += Int of int
type value += Float of float

match
| _ -> raise TypeError
```

```ocaml
module implemInt : INTEGER_T = ...
module implemFloat : FLOAT_T = ...
module implemRational : RATIONAL_T = ...
type value = ImplemInt.t
```