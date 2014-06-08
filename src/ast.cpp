#include "ast.hpp"

SExp* makeSExp(const char* val, SExpType type) {
  SExp* sexp = new SExp;
  sexp->type = type;
  first(sexp) = NULL;
  rest(sexp)  = NULL;
  val(sexp)   = val;
  return sexp;
}

SExp* cons(SExp* first, SExp* rest) {
  SExp* sexp  = new SExp;
  sexp->type  = LIST;
  first(sexp) = first;
  rest(sexp)  = rest;
  return sexp;
}

void push(SExp* list, SExp* obj) {
  assert(listp(list));
  SExp* p = list;
  while(rest(p) != NULL) {
    p = rest(p);
  }
  rest(p) = obj;
}

std::string print(SExp* sexp) {
  if(sexp) {
    if(listp(sexp)) {
      std::string out = "(";
      SExp* p = sexp;
      do {
        if(p != sexp)
          out += ' ';
        out += print(first(p));
        p = rest(p);
      } while(p != NULL);
      return out += ')';
    } else {
      return std::string(val(sexp));
    }
  } else
    return std::string("()");
}

void freeSExp(SExp* sexp) {
  if(sexp) {
    if(listp(sexp)) {
      /* Recur */
      if(first(sexp))
        freeSExp(first(sexp));
      if(rest(sexp))
        freeSExp(rest(sexp));
    }
    delete sexp;
  }
}
