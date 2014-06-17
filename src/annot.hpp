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
  SExp* ret;
  SExp* body;
  LambdaAST(Bindings args, SExp* ret, SExp* body) :
    arguments(args), ret(ret), body(body) {}
};

class FunctionAST : public LambdaAST {
  const char* name;
public:
  FunctionAST(const char* name, Bindings args, SExp* ret, SExp* body) :
    LambdaAST(args, ret, body) {
    this->name = name;
  }
};

/* Represents a call to a function or a special form. Since all macros have been
   expanded at this point. */
class CallAST : public AnnotAST {
  SExp* fn;
  SExp* args;
public:
  CallAST(SExp* fn, SExp* args) : fn(fn), args(args) { }
};
