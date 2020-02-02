# minipy
Minimalist Python-like language interpreter in OCaml.

This is a work in progress, most of the supported features are only partially implemented.

## Supported Features

- Python values:
    - Boolean.
    - Integer (represented with limited precision).
    - Float.
    - String.
    - List/Tuple.
- Function definitions (only with positional arguments for now), nested function definition.
- Variable assignments with tuple/list destructuring and assignements to a list element.
- Augmented assignments `+=`, `-=`, etc.
- Control flow:
    - Loops: `while` and `for` loops, with support for `break` and `continue`.
    - If conditionals with `elif` and `else`.
- Expressions:
    - Unary and binary operators, comparisons.
    - Ternary if operator.
    - Attributes, e.g. `x.foo`.
    - Subscripts, e.g. `x[foo]`.
    - Lambdas, `lambda`.
- Built-ins `print`, `range`.

## Todo

- List comprehensions.
- Fix the handling of captured variables in nested functions and lambdas.
- Dictionaries.
- Delete operator, `del`.
- Slices, e.g. `x[12:15]`.
- Keyword arguments for functions.

## Out of Scope (for now)

- Generators, `yield`.
- Objects, `class`, `self`, etc.
- Exceptions, try/with blocks, `raise`.
- With blocks.
- Module system, `import`.
- Starred expressions.
- Type annotations.
