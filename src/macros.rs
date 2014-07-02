extern crate collections;
extern crate ast;

use collections::treemap;

/* Each macro case associates a pattern with a template. When finding a macro
   invocation, the pattern matcher goes through every case in the macro, trying
   to match the input to the template. If it succeeds, the template is expanded
   using the variables bound during pattern matching. */
struct MacroCase {
    pattern: ast::SExp;
    template: ast::SExp
}

struct Macro {
    cases: Vec<MacroCase>
}

struct MacroTable {
    macros: treemap::TreeMap<String, Macro>
}

pub fn extractMacrosAndMacroExpand(sexp: SExp) -> SExp { sexp }
