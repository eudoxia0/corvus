# Implementation

## Reader Macros

Reader macro expansion happens at read time, while the source tree is being
parsed.

## Macroexpansion

This is the second phase of compilation, before the AST is further
transformed. The compiler traverses the AST looking for calls to macros and
performs the macroexpansion process.

## Blocks

After all macros have been expanded, the compiler tranverses the AST looking for
expressions that introduce scopes, verifying that their syntax is correct, and
transforms them to a more abstract form, blocks.

This structure is used to perform the various passes of the compiler.
