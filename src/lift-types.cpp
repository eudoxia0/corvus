#include "lift-types.hpp"

void findTypes(SExp* sexp, TypeEnv* tenv) {
  if(listp(sexp)) {
    if(atomp(first(sexp)) && atomeq(first(sexp), "type")) {
      /* Is the first element of the list the identifier "type"?  If so, parse the
         type definition. */
    } else {
      /* Otherwise, loop over the elements of the list, finding
         their types. */
      SExp* p = sexp;
      while(p != NULL) {
        findTypes(first(p), tenv);
        p = rest(p);
      }
    }
  }
  /* Atoms are not type definitions, so do nothing */
}

TypeEnv* extractTypes(SExp* sexp) {
  TypeEnv* tenv = new TypeEnv;
  findTypes(sexp, tenv);
  return tenv;
}
