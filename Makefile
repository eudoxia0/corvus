# Options
RUSTLIB = rustc -L . --crate-type=lib

# Crates
AST = src/ast.rs
READER = src/reader.rs
MODULES = src/modules.rs
MACROS = src/macros.rs
TYPES = src/types.rs
ANNOT = src/annot.rs
LIFT_VARS = src/lift-vars.rs

CRATES = ast reader modules macros types annot lift-vars

default: corvus

ast: $(AST)
	$(RUSTLIB) $?

reader: $(READER)
	$(RUSTLIB) $?

modules: $(MODULES)
	$(RUSTLIB) $?

macros: $(MACROS)
	$(RUSTLIB) $?

types: $(TYPES)
	$(RUSTLIB) $?

annot: $(ANNOT)
	$(RUSTLIB) $?

lift-vars: $(LIFT_VARS)
	$(RUSTLIB) $?

corvus: $(CRATES)
	rustc src/main.rs -L . -o $@

clean:
	rm *.rlib
