extern crate ast;

use ast::SExp;

pub struct Definitions {
    defs: Vec<(String, AST)>
}

pub enum AST {
    Let(Definitions),
    Call(Box<AST>, Vec<Box<AST>>),
    Atom(i64, i64, ast::AtomValue),
    Nil 
}

fn annotate_list(first: SExp, rest: SExp) -> AST {
    Nil
}

fn annotate(sexp: SExp) -> AST {
    match sexp {
        ast::Atom(l, c, v) => Atom(l, c, v),
        ast::Cons(first, rest) => annotate_list(*first, *rest),
        ast::Nil => Nil
    }
}
