# Options
RUSTLIB = rustc -L . --crate-type=lib

SRC = compiler

# Crates
LIST = $(SRC)/list.rs
AST = $(SRC)/ast.rs
READER = $(SRC)/reader.rs
MODULES = $(SRC)/modules.rs
MACROS = $(SRC)/macros.rs
TYPES = $(SRC)/types.rs
ANNOT = $(SRC)/annot.rs
LIFT_VARS = $(SRC)/lift-vars.rs

CRATES = list ast reader modules macros types annot lift-vars

default: corvus

list: $(LIST)
	$(RUSTLIB) $?

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
	rustc $(SRC)/main.rs -L . -o $@

clean:
	rm *.rlib
