extern crate collections;
extern crate ast;

use ast::{SExp, Atom, Ident, List, Nil};
use std::collections::HashMap;

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

/* Represents a variant of a datatype */
struct Variant {
    name: String,
    definition: Box<Type>
}

enum Type {
    /* Scalar types */
    Unit,
    Bool,
    Integer(IntegerType),
    Float(FloatType),
    /* Aggregate types */
    Array(Box<Type>),
    Tuple(Vec<Type>),
    Record(Vec<TypeField>),
    Datatype(Vec<Variant>),
    Function(Vec<TypeField>, TypeField),
    /* Pointers */
    Pointer(Box<Type>, i32),
    /* Generics and kinds */
    Generic(Vec<TypeVar>, SExp),
    Kind(SExp)
}

struct TypeVar {
    name: String,
    kind: Type,
}

struct TypeDef {
    def: Type,
    doc: String
}

struct TypeEnv {
    types: HashMap<String, TypeDef>
}

/* Return a type environment with the basic types. */
pub fn createDefaultTEnv() -> TypeEnv {
    let mut tenv = HashMap::new();
    tenv.insert(String::from_str("bool"),
                TypeDef { def: Bool, doc: String::from_str("") });
    tenv.insert(String::from_str("i8"),
                TypeDef { def: Integer(Byte), doc: String::from_str("") });
    tenv.insert(String::from_str("i16"),
                TypeDef { def: Integer(Short), doc: String::from_str("") });
    tenv.insert(String::from_str("i32"),
                TypeDef { def: Integer(Int32), doc: String::from_str("") });
    tenv.insert(String::from_str("i64"),
                TypeDef { def: Integer(Int64), doc: String::from_str("") });
    tenv.insert(String::from_str("i128"),
                TypeDef { def: Integer(Int128), doc: String::from_str("") });
    tenv.insert(String::from_str("single"),
                TypeDef { def: Float(Single), doc: String::from_str("") });
    tenv.insert(String::from_str("double"),
                TypeDef { def: Float(Double), doc: String::from_str("") });
    tenv.insert(String::from_str("quad"),
                TypeDef { def: Float(Quad), doc: String::from_str("") });
    TypeEnv { types: tenv }
}

/* Parse type specifiers */
pub fn emitType(sexp: SExp, tenv: &mut TypeEnv) -> Type {
    match sexp {
        Atom(_,_,val) => {
            /* A named type. Look it up in the type environment. */
            match val {
                Ident(name) => {
                    match tenv.types.find(&name) {
                        Some(ref mut t) => t.def,
                        None => fail!("No typed named {}.", name)
                    }
                },
                _ => fail!("Named types must be identifiers.")
            }
        },
        List(vec) => {
            /* A type expression */
            Unit
        },
        Nil => Unit
    }
}

/* The public interface to the type lifter: Takes an S-expression and returns a
   type environment. */
//pub fn extractTypes(sexp: SExp, tenv: &TypeEnv) -> SExp { }
