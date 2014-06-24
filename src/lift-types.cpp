#include "lift-types.hpp"

void findTypes(SExp* sexp, TypeEnv* tenv) {
  if(listp(sexp)) {
    /* Is the first element of the list the identifier "type"?  If so, parse the
       type definition. Otherwise, loop over the elements of the list, finding
       their types. */
  }
  /* Atoms are not type definitions, so do nothing */
}

TypeEnv* extractTypes(SExp* sexp) {
  TypeEnv* tenv = new TypeEnv;
  findTypes(sexp, tenv);
  return tenv;
}
