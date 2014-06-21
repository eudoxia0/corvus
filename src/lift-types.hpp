/* Extract type definitions into a type environment */
#include "ast.hpp"
#include "types.hpp"

/* The public interface to the type lifter: Takes an S-expression and returns a
   type environment. Type definitions are replaced with the truth constant
   in-place. */
TypeEnv* extractTypes(SExp* sexp);
