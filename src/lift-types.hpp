/* Extract type definitions into a type environment */
#include "ast.hpp"
#include "types.hpp"

/* Validate a type definition form */
int isValid(SExp* form);

/* A recursive function that does the actual extraction. Recurs over sexp,
   replacing type definitions with truth constants and adding them to the type
   environment. */
void findTypes(SExp* sexp, TypeEnv* tenv);

/* The public interface to the type lifter: Takes an S-expression and returns a
   type environment. Type definitions are replaced with the truth constant
   in-place. */
TypeEnv* extractTypes(SExp* sexp);
