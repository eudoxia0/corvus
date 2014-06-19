## Blocks

After all macros have been expanded, the compiler tranverses the AST looking for
expressions that introduce scopes, verifying that their syntax is correct, and
transforms them to a more abstract form, blocks.

This structure is used to perform the various passes of the compiler.

## Function Lifting

The compiler recurs through the blocks, finding the definitions of lambdas and
functions, and verifying that the definition syntax is correct.

The compiler then goes through every block inside a function definition, looking
at the instructions to see if any of the variables can be found within the
function. Since macroexpansion has already been performed, all symbols that are
not in the first place in an argument list are guaranteed to be identifiers.

Identifiers that are within the definition of the function are
unimportant. Identifiers that are in a scope outside that of the function are
marked specially before the function is 'shipped' to the toplevel and its block
replaced with the function name.

The annotatated identifiers are then used to turn the function into a closure,
if necessary.
 
