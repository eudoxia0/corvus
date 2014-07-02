extern crate collections;

extern crate ast;

use collections::treemap;

enum IntegerType {
    Byte = 8,
    Short = 16,
    Int32 = 32,
    Int64 = 64,
    Int128 = 128
}

enum FloatType {
    Single = 32,
    Double = 64,
    Quad = 128
}

struct TypeField {
    name: String,
    definition: Box<Type>,
    docstring: String
}

enum Type {
    Unit,
    Bool,
    Integer(IntegerType),
    Float(FloatType),
    Array(Box<Type>),
    Tuple(Vec<Type>),
    Record(Vec<TypeField>),
    Function(Vec<TypeField>, TypeField),
    Pointer(Box<Type>, i32)
}

struct TypeDef {
    definition: Type,
    docstring: String
}

struct TypeEnv {
    types: treemap::TreeMap<String, TypeDef>
}

/* Return a type environment with the basic types. */
pub fn createDefaultTEnv() -> TypeEnv { }

/* Parse type specifiers */
pub fn emitType(sexp: ast::SExp, tenv: &TypeEnv) -> Type { }

