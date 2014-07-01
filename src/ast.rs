mod ast {
    pub enum AtomValue {
        Integer(i64),
        Real(f64),
        Str(String),
        Ident(String)
    }

    pub enum SExp {
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

    pub fn print(sexp : SExp) -> String {
        match sexp {
            Atom(_, _, val) => printAtom(val),
            List(list) => printList(list),
            Nil => String::from_str("()")
        }
    }

    pub fn classify(str: String) -> AtomValue {
        Ident(str)
    }

    pub fn atomFromStr(str : String, line: i64, col: i64) -> SExp {
        Atom(line, col, classify(str))
    }
}
