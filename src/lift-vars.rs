extern crate annot;

use annot::{AST, Let, Call, Nil, Definitions};

struct Env {
    defs: Vec<(AST, i64)>
}

fn env_update(env: &Env, defs: Definitions) -> Box<Env> {
    box Env { defs: Vec::new() }
}

fn lift_ast(ast: AST, env: &mut Env) -> AST {
    match ast {
        Let(defs) => Nil,
        Call(fun, args) => {
            let largs = args.move_iter().map(
                |a| box lift_ast(*a, env) ).collect();
            Call(box lift_ast(*fun, env), largs)
        },
        Nil => Nil
    }
}

/* Public interface */

pub fn lift(ast: AST) -> AST {
    let mut env = Env { defs: Vec::new() };
    lift_ast(ast, &env)
}
