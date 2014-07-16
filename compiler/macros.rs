extern crate collections;
extern crate ast;

use ast::SExp;
use std::collections::HashMap;

/* Definitions */

/* Each macro case associates a pattern with a template. When finding a macro
   invocation, the pattern matcher goes through every case in the macro, trying
   to match the input to the template. If it succeeds, the template is expanded
   using the variables bound during pattern matching. */
struct MacroCase {
    pattern: SExp,
    template: SExp
}

struct Macro {
    cases: Vec<MacroCase>
}

struct MacroTable {
    macros: HashMap<String, Macro>
}

/* Macro extraction */

/* Macro expansion */

/* Weave the bindings into the template */
fn expand_macro_case(template: SExp, bindings: &Vec<SExp>) -> SExp {
  template
}

/* Try to expand a macro invocation. If it doesn't match any patterns, fail. */
fn expand_macro(pattern: SExp, macro: Macro) -> SExp {
    pattern
}

/* Tranverse an expression, looking for macro invocations and expanding them */
fn macroexpand(sexp: SExp, table: &MacroTable) -> SExp {
    sexp
}

/* Public interface */

pub fn extract_and_expand(sexp: SExp) -> SExp { sexp }
