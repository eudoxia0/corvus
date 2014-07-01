enum AtomValue {
    Integer(i64),
    Real(f64),
    Str(String),
    Ident(String)
}

enum SExp {
    Atom(i64, i64, AtomValue),
    List(Vec<SExp>),
    Nil
}

fn printAtom(val: AtomValue) -> String {
    match val {
        Integer(int) => int.to_str(),
        Real(real) => real.to_str(),
        Str(str) => str,
        Ident(str) => str
    }
}

fn printList(list : Vec<SExp>) -> String {
    let mut out = String::from_str("(");
    for elem in list.move_iter() {
        out = format!("{} {}", out, print(elem));
    }
    format!("{})", out)
}

fn print(sexp : SExp) -> String {
    match sexp {
        Atom(_, _, val) => printAtom(val),
        List(list) => printList(list),
        Nil => String::from_str("()")
    }
}

fn classify(str: String) -> AtomValue {
    Ident(str)
}

fn atomFromStr(str : String, line: i64, col: i64) -> SExp {
    Atom(line, col, classify(str))
}

fn main() {
    let a = Atom(0, 0, Integer(12));
    let b = Atom(0, 0, Real(3.14));
    let c = Atom(0, 0, Ident(String::from_str("derp")));
    println!("{}", print(Nil));
    println!("{}", print(a));
    println!("{}", print(List(vec![b, c])));
}
