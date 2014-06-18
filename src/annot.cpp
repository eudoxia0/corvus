#include "annot.hpp"

AnnotAST* annotateList(SExp* list) {
  return NULL;
}

AnnotAST* annotate(SExp* sexp) {
  if(sexp) {
    if(listp(sexp)) {
      return annotateList(sexp);
    } else {
      return new AtomAST(sexp->content.atom);
    }
  } else {
    /* A null pointer represents the null form, which designates the unit
       type. */
    return NULL;
  }
}
