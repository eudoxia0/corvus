/* Turn the AST into an annotated AST for function lifting. */
#include "ast.hpp"
#include <vector>
#include <utility>

/* The base class of all annotated ASTs */
class AnnotAST { };

class AtomAST: public AnnotAST {
  Atom atom;
public:
  AtomAST(Atom atom) : atom(atom) { }
};

/* We don't use a Map because order of assignment is important */
typedef std::vector<std::pair<const char*, SExp*> > Bindings;

/* Represents a let: A series of assignments in a new scope */
class LetAST : public AnnotAST {
  Bindings bindings;
public:
  LetAST(Bindings bind) : bindings(bind) {}
};

/* An anonymous function definition */
class LambdaAST : public AnnotAST {
public:
  Bindings arguments;
  AnnotAST* ret;
  AnnotAST* body;
  LambdaAST(Bindings args, AnnotAST* ret, AnnotAST* body) :
    arguments(args), ret(ret), body(body) {}
};

/* A named function definition */
class FunctionAST : public LambdaAST {
  const char* name;
public:
  FunctionAST(const char* name, Bindings args, AnnotAST* ret, AnnotAST* body) :
    LambdaAST(args, ret, body) {
    this->name = name;
  }
};

/* Represents a call to a function, special form or an expression that returns a
   function pointer. Since all macros have been expanded at this point the call cannot be to a macro. */
class CallAST : public AnnotAST {
  AnnotAST* fn;
  std::vector<AnnotAST*> args;
public:
  CallAST(AnnotAST* fn, std::vector<AnnotAST*> args) : fn(fn), args(args) { }
};

/* Test whether the text of 'atom' equals 'text' */
int atomeq(SExp* atom, const char* text);

/* Annotate a vector of S-expressions */
std::vector<AnnotAST*> annotate(std::vector<SExp*> list);

/* Transforms an unstructured S-expression into a more abstract syntax tree, or
   'annotated abstract syntax tree'. */
AnnotAST* annotate(SExp* sexp);
