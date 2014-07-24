extern crate list;

use list::{List, Value, Cons, Nil, mapcar};

pub enum AtomValue {
    Integer(i64),
    Real(f64),
    Str(String),
    Ident(String)
}

pub struct Atom {
    pub line: i64,
    pub col: i64,
    pub val: AtomValue
}

pub type SExp = List<Atom>;

fn print_atom(atom: Atom) -> String {
    match atom.val {
        Integer(int) => int.to_str(),
        Real(real) => real.to_str(),
        Str(str) => str,
        Ident(str) => str
    }
}

pub fn print(sexp : SExp) -> String {
    match sexp {
        Value(atom) => print_atom(*atom),
        Cons(first, rest) => {
            let mut out = format!("({}", print(*first));
            mapcar(*rest, |elem: SExp| -> SExp {
                out = format!("{} {}", out, print(elem));
                Nil
            });
            out = format!("{})", out);
            out
        },
        Nil => String::from_str("()")
    }
}

pub fn classify(str: String) -> AtomValue {
    Ident(str)
}

pub fn atom_from_str(str : String, line: i64, col: i64) -> SExp {
    Value(box Atom { line: line, col: col, val: classify(str) })
}

#[test]
fn test() {
    let a = Value(box Atom { val: Integer(12) });
    let b = Value(box Atom { val: Real(3.14) });
    let c = Value(box Atom { val: Ident(String::from_str("test")) });
    let result_a = format!("{}", print(Nil));
    let result_b = format!("{}", print(a));
    let result_c = format!("{}", print(Cons(box b, box Cons(box c, box Nil))));
    assert!(result_a == String::from_str("()"));
    assert!(result_b == String::from_str("12"));
    assert!(result_c == String::from_str("(3.14 test)"));
}
