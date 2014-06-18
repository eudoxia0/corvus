#include "annot.hpp"

int atomeq(SExp* atom, const char* text) {
  return val(atom) == text;
}

std::vector<AnnotAST*> annotate(std::vector<SExp*> list) {

}

AnnotAST* annotateList(SExp* list) {
  /* Note: If a list makes it this far (when called from 'annotate') it is
     guaranteed to be non-NULL, that is, to have at least one element. */
  SExp* first = first(list);
  if(listp(first)) {
    /* If the first element of a list is a list, then it's an expression that
       (Presumably) returns a function pointer */
    return new CallAST(annotate(first), annotate(sexpToVec(rest(list))));
  } else {
    /* The first element is an atom, which can be either a function, a variable,
       or a special form. We check the special forms first. If it's none of
       those, we just return a CallAST: Differentiating between variables and
       functions is done further ahead.

       This part of the process involves verifying the syntax of the special
       forms represented in the annotated AST: let, defn and lambda. */
    if(atomeq(first, "let")) {
      return NULL;
    } else if(atomeq(first, "defn")) {
      return NULL;
    } else if(atomeq(first, "lambda")) {
      return NULL;
    } else {
      return new CallAST(annotate(first), annotate(sexpToVec(rest(list))));
    }
  }
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
