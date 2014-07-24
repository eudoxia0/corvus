extern crate list;
extern crate collections;
extern crate ast;

use list::{List, Value, Cons, Nil};
use ast::{SExp, Atom, AtomValue, Ident};
use std::collections::HashMap;

/* Definitions */

struct Module {
    imports: Vec<String>,
    exports: Vec<String>
}

struct ModuleEnv {
    modules: HashMap<String, Module>
}

/* Utilities */

fn has_prefix(str: &String) -> bool {
    str.as_slice().contains_char(':')
}

/* Take an atom value, and if it's an identifier without a module prefix, add
   that of the present module. */
fn modularize_atom_value(val: AtomValue, module_name: &String) -> AtomValue {
    match val {
        Ident(str) => {
            if has_prefix(&str) {
                Ident(str)
            } else {
                Ident(module_name + str)
            }
        },
        _ => val
    }
}

fn modularize_atom(atom: Atom, module_name: &String) -> Atom {
    Atom { line: atom.line, col: atom.col,
           val: modularize_atom_value(atom.val, module_name) }
}

pub fn modularize(ast: SExp, module_name: &String) -> SExp {
    match ast {
        Value(val) => Value(box modularize_atom(*val, module_name)),
        Cons(first, rest) => Cons(box modularize(*first, module_name),
                                  box modularize(*rest, module_name)),
        Nil => Nil
    }
}
