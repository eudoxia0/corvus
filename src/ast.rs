extern crate collections;

pub enum AtomValue {
    Integer(i64),
    Real(f64),
    Str(String),
    Ident(String)
}

pub enum SExp {
    Atom(i64, i64, AtomValue),
    Cons(Box<SExp>, Box<SExp>),
    Nil
}

pub fn mapcar(list: SExp, fun: |SExp| -> SExp) -> SExp {
    match list {
        Cons(first, rest) => {
            Cons(box fun(*first), box mapcar(*rest, fun))
        },
        _ => Nil
    }
}

fn print_atom(val: AtomValue) -> String {
    match val {
        Integer(int) => int.to_str(),
        Real(real) => real.to_str(),
        Str(str) => str,
        Ident(str) => str
    }
}

pub fn print(sexp : SExp) -> String {
    match sexp {
        Atom(_, _, val) => print_atom(val),
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
    Atom(line, col, classify(str))
}

#[test]
fn test() {
    let a = Atom(0, 0, Integer(12));
    let b = Atom(0, 0, Real(3.14));
    let c = Atom(0, 0, Ident(String::from_str("test")));
    let result_a = format!("{}", print(Nil));
    let result_b = format!("{}", print(a));
    let result_c = format!("{}", print(Cons(box b, box Cons(box c, box Nil))));
    assert!(result_a == String::from_str("()"));
    assert!(result_b == String::from_str("12"));
    assert!(result_c == String::from_str("(3.14 test)"));
}
