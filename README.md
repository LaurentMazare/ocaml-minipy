# minipy
Minimalist Python-like language interpreter in OCaml.

![Main workflow](https://github.com/LaurentMazare/ocaml-torch/workflows/Main%20workflow/badge.svg)

Try the interpreter [online](http://laurentmazare.github.io/minipy/) or
use the [editor](http://laurentmazare.github.io/minipy/editor.html)
(these rely on [js_of_ocaml](https://ocsigen.org/js_of_ocaml/3.5.1/manual/overview)).

This is a work in progress, most of the supported features are only partially implemented.

## Supported Features

- Python values:
    - Boolean.
    - Integer (represented with arbitrary precision).
    - Float.
    - String.
    - List/Tuple.
- Function definitions (with keyword arguments, ...), nested function definition.
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
    - List comprehensions (only for lists, no support for dict/set).
- Built-ins `print`, `range`.
- Dictionaries.
- Delete operator, `del`.
- REPL example, javascript version with js-of-ocaml.
- Starred expressions.
- Basic object system.
- Exceptions, try/with blocks, `raise`.
- With blocks.
- Inheritance.

## Not implemented yet

- Sets.
- Slices, e.g. `x[12:15]`.
- Generators, `yield`.
- Module system, `import`.
- Type annotations.
- Bytecode compilation/interpreter.
