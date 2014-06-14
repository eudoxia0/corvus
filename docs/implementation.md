# Implementation

## Reader Macros

Reader macro expansion happens at read time, while the source tree is being
parsed.

## Macroexpansion

This is the second phase of compilation, before the AST is further
transformed. The compiler traverses the AST looking for calls to macros and
performs the macroexpansion process.
