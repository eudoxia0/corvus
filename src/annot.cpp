#include "annot.hpp"

int atomeq(SExp* atom, const char* text) {
  return val(atom) == text;
}

std::vector<AnnotAST*> annotate(std::vector<SExp*> list) {
  std::vector<AnnotAST*> out;
  for(size_t i = 0; i < list.size(); i++) {
    out.push_back(annotate(list[i]));
  }
  return out;
}

void formError(Atom atom, std::string explanation) {
  throw Error(atom.line, atom.col,
              "Bad '" + std::string(atom.val) + "' form: " + explanation);
}

void noBodyError(Atom atom) {
  formError(atom, "No body.");
}

void letBadBindingsError(Atom atom) {
  formError(atom, "Odd number of arguments in bindings.");
}

void fnNoArgsError(Atom atom) {
  formError(atom, "No argument list in function definition.");
}

void fnNoRetError(Atom atom) {
  formError(atom, "No return type specifier in function definition.");
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
    Atom atom = first->content.atom;
    if(atomeq(first, "let")) {
      if(length(rest(list)) < 2) {
        formError(atom, "No body.");
      }
    } else if(atomeq(first, "defn")) {
      switch(length(rest(list))) {
        case 0:
          fnNoArgsError(atom);
        case 1:
          fnNoRetError(atom);
        case 2:
          noBodyError(atom);
        default:
            /* Process the function definition */
            return NULL;
        }
    } else if(atomeq(first, "lambda")) {
      switch(length(rest(list))) {
        case 0:
          fnNoArgsError(atom);
        case 1:
          noBodyError(atom);
        default:
            /* Process the lambda definition */
            return NULL;
        }
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
