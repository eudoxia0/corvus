/* Extract type definitions into a type environment */
#include "ast.hpp"
#include "types.hpp"

/* A recursive function that does the actual extraction. Recurs over sexp,
finding type definitions, validating them and adding them to the type
environment. */
void findTypes(SExp* sexp, TypeEnv* tenv);

/* The public interface to the type lifter: Takes an S-expression and returns a
   type environment. */
TypeEnv* extractTypes(SExp* sexp);
