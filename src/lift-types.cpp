#include "lift-types.hpp"

void findTypes(SExp* sexp, TypeEnv* tenv) {
  if(sexp) {
    if(listp(sexp)) {
      if(atomp(first(sexp)) && atomeq(first(sexp), "type")) {
        /* Parse the type definition. */
        const char* name = val(first(rest(sexp)));
        Type def = emitType(rest(rest(sexp)), tenv);
        tenv->types.insert(std::make_pair(name, TypeDef(def, "")));
        /* TODO: Validation, docstring */
        sexp = makeSExp("true", IDENTIFIER);
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
}

TypeEnv* extractTypes(SExp* sexp) {
  TypeEnv* tenv = createDefaultTEnv();
  findTypes(sexp, tenv);
  return tenv;
}
