#include "ast.hpp"

SExp* makeSExp(std::string val, SExpType type) {
  SExp* sexp = new SExp;
  sexp->type = type;
  first(sexp) = NULL;
  rest(sexp)  = NULL;
  val(sexp)  = val.c_str();
  return sexp;
}

SExp* cons(SExp* first, SExp* rest) {
  SExp* sexp = new SExp;
  sexp->type = LIST;
  first(sexp) = first;
  rest(sexp)  = rest;
  return sexp;
}
