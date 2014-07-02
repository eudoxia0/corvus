# Options
RUSTLIB = rustc -L . --crate-type=lib

# Crates
AST = src/ast.rs
READER = src/reader.rs
TYPES = src/types.rs

default: corvus

ast: $(AST)
	$(RUSTLIB) $?

reader: $(READER)
	$(RUSTLIB) $?

types: $(TYPES)
	$(RUSTLIB) $?

corvus: ast reader types
	rustc src/main.rs -L . -o $@

clean:
	rm *.rlib
