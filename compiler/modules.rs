extern crate list;
extern crate ast;

use list::{List};
use ast::SExp;

struct Module {
    imports: List<String>,
    exports: List<String>
}

/* Public interface */
