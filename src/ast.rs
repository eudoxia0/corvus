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

    #[test]
    fn test() {
        let a = Atom(0, 0, Integer(12));
        let b = Atom(0, 0, Real(3.14));
        let c = Atom(0, 0, Ident(String::from_str("derp")));
        let result_a = format!("{}", print(Nil));
        let result_b = format!("{}", print(a));
        let result_c = format!("{}", print(List(vec![b, c])));
        assert!(result_a == String::from_str("()"));
        assert!(result_b == String::from_str("12"));
        assert!(result_c == String::from_str("( 3.14 derp)"));
    }
}
