/* Turn the AST into an annotated AST for function lifting. */
#include "ast.hpp"
#include <vector>
#include <utility>

/* Utility types */
typedef std::vector<std::pair<const char*, SExp*> > Bindings;

class AnnotAST { };

class LetAST : public AnnotAST {
  Bindings bindings;
public:
  LetAST(Bindings bind) : bindings(bind) {}
};

class LambdaAST : public AnnotAST {
public:
  Bindings arguments;
  AnnotAST* ret;
  AnnotAST* body;
  LambdaAST(Bindings args, AnnotAST* ret, AnnotAST* body) :
    arguments(args), ret(ret), body(body) {}
};

class FunctionAST : public LambdaAST {
  const char* name;
public:
  FunctionAST(const char* name, Bindings args, AnnotAST* ret, AnnotAST* body) :
    LambdaAST(args, ret, body) {
    this->name = name;
  }
};

/* Represents a call to a function or a special form. Since all macros have been
   expanded at this point. */
class CallAST : public AnnotAST {
  AnnotAST* fn;
  std::vector<AnnotAST*> args;
public:
  CallAST(AnnotAST* fn, std::vector<AnnotAST*> args) : fn(fn), args(args) { }
};

class AtomAST: public AnnotAST {
  Atom atom;
public:
  AtomAST(Atom atom) : atom(atom) { }
};

/* Transforms an unstructured S-expression into a more abstract syntax tree, or
   'annotated abstract syntax tree'. */
AnnotAST* annotate(SExp* sexp);
