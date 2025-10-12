memory-spec
===========

A single source of truth for all your memory.x files.

## Expressions

All values are treated as `i64`. They can be specified either as an integer directly, or as a
string containing an expression to be evaluated.

## Operators
`+`, `-`, `*`, and `/` are implemented. Semantics and precedence are the same as C, except
overflows will result in an error.
