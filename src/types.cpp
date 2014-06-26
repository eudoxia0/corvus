#include "types.hpp"

TypeEnv* createDefaultTEnv() {
  TypeEnv* tenv = new TypeEnv();
  tenv->types.insert(std::make_pair("bool", TypeDef(Bool(), "")));
  tenv->types.insert(std::make_pair("i8", TypeDef(Integer(Byte), "")));
  tenv->types.insert(std::make_pair("i16", TypeDef(Integer(Short), "")));
  tenv->types.insert(std::make_pair("i32", TypeDef(Integer(Int32), "")));
  tenv->types.insert(std::make_pair("i64", TypeDef(Integer(Int64), "")));
  tenv->types.insert(std::make_pair("i128", TypeDef(Integer(Int128), "")));
  tenv->types.insert(std::make_pair("half", TypeDef(Float(Half), "")));
  tenv->types.insert(std::make_pair("single", TypeDef(Float(Single), "")));
  tenv->types.insert(std::make_pair("double", TypeDef(Float(Double), "")));
  tenv->types.insert(std::make_pair("quad", TypeDef(Float(Quad), "")));
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
      SExp* exp = first(sexp);
      SExp* args = rest(sexp);
      if(!(atomp(sexp) && (exp->type == IDENTIFIER))) {
        // Error
      }
      /* Is the expression a type constructor? */
      if(atomeq(sexp, "arr")) {
        Type base_type = emitType(first(args), tenv);
        return Array(base_type);
      } else if(atomeq(sexp, "tup")) {

      } else if(atomeq(sexp, "rec")) {

      } else if(atomeq(sexp, "fn")) {

      } else if(atomeq(sexp, "p") || atomeq(sexp, "pp") ||
                atomeq(sexp, "ppp") || atomeq(sexp, "pppp")) {
        size_t indirection = strlen(val(sexp));

      }
      /* Is the expression a type operation? */
      else if(atomeq(sexp, "base")) {

      }
    }
  }
  /* The null form is the unit type. */
  return Unit();
}
