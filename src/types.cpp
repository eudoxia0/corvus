#include "annot.cpp"

Type FunctionAST::emitType(TypeEnv* env) {
  return Integer(Bool);
}
