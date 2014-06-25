#include "types.hpp"

TypeEnv* createDefaultTEnv() {
  TypeEnv* tenv = new TypeEnv();
  tenv->types.find("bool")->second = TypeDef(Bool(), "");
  tenv->types.find("i8")->second = TypeDef(Integer(Byte), "");
  tenv->types.find("i16")->second = TypeDef(Integer(Short), "");
  tenv->types.find("i32")->second = TypeDef(Integer(Int32), "");
  tenv->types.find("i64")->second = TypeDef(Integer(Int64), "");
  tenv->types.find("i128")->second = TypeDef(Integer(Int128), "");
  return tenv;
}

Type emitType(SExp* sexp, TypeEnv* tenv) {
  if(sexp) {
    if(atomp(sexp)) {
      /* A named type. Check whether it exists in the type environment. */
    } else {
      /* A list. Check whether the first element is an atom and that it is a
         member of the set of allowed type specifiers. Otherwise, signal an
         error. */
    }
  }
  /* The null form is the unit type. */
  return Unit();
}
