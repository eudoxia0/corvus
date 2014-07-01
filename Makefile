# Options
RUSTLIB = rustc --crate-type=lib

# Crates
AST = src/ast.rs

default: corvus

ast: $(AST)
	$(RUSTLIB) $?

corvus: ast
	rustc src/main.rs -L . -o $@

clean:
	rm *.rlib
