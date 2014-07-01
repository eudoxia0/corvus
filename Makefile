# Options
RUSTLIB = rustc -L . --crate-type=lib

# Crates
AST = src/ast.rs
READER = src/reader.rs

default: corvus

ast: $(AST)
	$(RUSTLIB) $?

reader: $(READER)
	$(RUSTLIB) $?

corvus: ast reader
	rustc src/main.rs -L . -o $@

clean:
	rm *.rlib
