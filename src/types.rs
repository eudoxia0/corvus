extern crate collections;
extern crate list;
extern crate ast;

use list::{List, Value, Cons, Nil};
use ast::{SExp, Atom, Ident};
use std::collections::HashMap;
use std::collections::dlist::DList;

/* Integer types */
enum IntegerType {
    Byte = 8,
    Short = 16,
    Int32 = 32,
    Int64 = 64,
    Int128 = 128
}

/* Floating-point types */
enum FloatType {
    Single = 32,
    Double = 64,
    Quad = 128
}

/* Represents an argument to a function or the field of a record */
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

/* The definition of a kind */
enum KindDef {
    /* The set of all types: Universal quantification */
    U,
    /* Whether a function is defined for a given type */
    Defines(String, List<Type>, Box<Type>),
}

/* A type variable */
struct TypeVar {
    /* The variable's name (Including the question mark) */
    name: String,
    /* The kind the variable belongs to. By default, the Universal set (U). */
    kind: KindDef,
}

enum Type {
    /* Scalar types */
    Unit,
    Bool,
    Integer(IntegerType),
    Float(FloatType),
    /* Aggregate types */
    Array(Box<Type>),
    Tuple(List<Type>),
    Record(List<TypeField>),
    Datatype(List<Variant>),
    Function(List<TypeField>, TypeField),
    /* Pointers */
    Pointer(Box<Type>, i32),
    /* Generics and kinds */
    Generic(List<TypeVar>, SExp),
    Kind(KindDef),
}

impl Clone for Type {
    fn clone(&self) -> Type {
        match *self {
            // Scalars
            Unit => Unit,
            Bool => Bool,
            Integer(t) => Integer(t),
            Float(t) => Float(t),
            // Aggregates
            Array(ref base) => Array(base.clone()),
            // Pointers
            Pointer(ref base, depth) => Pointer(base.clone(), depth),
            // To-do
            _ => Unit
        }
    }
}

/* A type definition: A type plus an optional docstring */
struct TypeDef {
    def: Type,
    doc: String
}

impl Clone for TypeDef {
    fn clone(&self) -> TypeDef {
        TypeDef { def: self.def.clone(),
                  doc: self.doc.clone() }
    }
}

/* A type environment associates names with type definitions */
struct TypeEnv {
    types: HashMap<String, TypeDef>
}

/* Return a type environment with the basic types. */
pub fn create_default_tenv() -> TypeEnv {
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

fn emit_exp(op: SExp, args: SExp, tenv: &mut TypeEnv) -> Type {
    let type_constructor = emit_type(op, tenv);
    Unit
}

/* Parse type specifiers */
pub fn emit_type(sexp: SExp, tenv: &mut TypeEnv) -> Type {
    match sexp {
        Cons(first, rest) => {
            /* A type expression */
            emit_exp(*first, *rest, tenv)
        },
        Value(atom) => {
            /* A named type. Look it up in the type environment. */
            match atom.val {
                Ident(name) => {
                    match tenv.types.find_copy(&name) {
                        Some(t) => t.def,
                        None => fail!("No typed named {}.", name)
                    }
                },
                _ => fail!("Named types must be identifiers.")
            }
        },
        Nil => Unit
    }
}

fn define_type(args: SExp, tenv: &mut TypeEnv) {
    match args {
        Cons(type_name, rest) => {
             match *rest {
                 Cons(definition, rest) => {
                     match *type_name {
                         Value(atom) => {
                             match atom.val {
                                Ident(name) => {
                                    let def = emit_type(*definition, tenv);
                                    let doc = String::from_str("");
                                    let typedef = TypeDef { def: def, doc: doc };
                                    tenv.types.insert(name, typedef);
                                },
                                _ => fail!("Bad type definition.")
                             }
                         },
                         _ => fail!("Bad type definition.")
                     }
                 }
                 _ => fail!("Bad type definition.")
             }
        }
        _ => fail!("Bad type definition.")
    }
}

/* The public interface to the type lifter: Takes an S-expression and fills a
   type environment. */
pub fn extract_types(sexp: SExp, tenv: &mut TypeEnv)  {
    match sexp {
        Cons(fun, args) => {
            // Is the first element of the expression the symbol type?
            match *fun {
                Value(atom) => {
                    match atom.val {
                        Ident(name) => {
                            if name == String::from_str("type") {
                                // Process the type definition.
                                define_type(*args, tenv);
                            };
                        },
                        _ => ()
                    }
                },
                _ => ()
            }
        },
        _ => ()
    }
}
