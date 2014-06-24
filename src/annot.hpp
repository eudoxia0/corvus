/* Turn the AST into an annotated AST for function lifting. */
#include "ast.hpp"
#include "types.hpp"
#include "errors.hpp"

/* The base class of all annotated ASTs */
class AnnotAST { };

class AtomAST: public AnnotAST {
  Atom atom;
public:
  AtomAST(Atom at) : atom(at) { }
  Type emitType(TypeEnv* env);
};

/* We don't use a Map because order of assignment is important */
typedef std::pair<const char*, AnnotAST*> Binding;
typedef std::vector<Binding> Bindings;

/* Represents a let: A series of assignments in a new scope */
class LetAST : public AnnotAST {
  Bindings bindings;
  AnnotAST* body;
public:
  LetAST(Bindings bind, AnnotAST* lbody) : bindings(bind), body(lbody) {}
  Type emitType(TypeEnv* env);
};

/* An anonymous function definition */
class LambdaAST : public AnnotAST {
  Bindings arguments;
  AnnotAST* ret;
  AnnotAST* body;
public:
  LambdaAST(Bindings args, AnnotAST* ret_type, AnnotAST* lbody) :
    arguments(args), ret(ret_type), body(lbody) {}
  Type emitType(TypeEnv* env);
};

/* A named function definition */
class FunctionAST : public LambdaAST {
  const char* name;
public:
  FunctionAST(const char* fname, Bindings args, AnnotAST* ret_type,
              AnnotAST* fbody) :
    LambdaAST(args, ret_type, fbody) {
    this->name = fname;
  }
  Type emitType(TypeEnv* env);
};

/* Represents a call to a function, special form or an expression that returns a
   function pointer. Since all macros have been expanded at this point the call cannot be to a macro. */
class CallAST : public AnnotAST {
  AnnotAST* fn;
  std::vector<AnnotAST*> args;
public:
  CallAST(AnnotAST* fun, std::vector<AnnotAST*> arguments) :
    fn(fun), args(arguments) { }
  Type emitType(TypeEnv* env);
};

/* Annotate a vector of S-expressions */
std::vector<AnnotAST*> annotate(std::vector<SExp*> list);

/* Annotate a list of S-Expressions */
AnnotAST* annotateList(SExp* list);

/* Transforms an unstructured S-expression into a more abstract syntax tree, or
   'annotated abstract syntax tree'. */
AnnotAST* annotate(SExp* sexp);
