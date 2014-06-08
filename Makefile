# Compiler options
INCLUDE = -I src -I ext
CXXFLAGS = -Wall -pedantic -pedantic-errors -g -O0

# Modules
AST_HEAD = src/ast.hpp
AST_SRC = src/ast.cpp
AST = src/ast.o

READER_HEAD = src/reader.hpp
READER_SRC = src/reader.cpp
READER = src/reader.o

MACROS_HEAD = src/macros.hpp
MACROS_SRC = src/macros.cpp
MACROS = src/macros.o

NORMALIZE = src/normalize.hpp
NORMALIZE_SRC = src/normalize.cpp
NORMALIZE = src/normalize.o

MODULES = $(AST) $(READER) $(MACROS) $(NORMALIZE)

# Compile a module
CMODULE = $(CXX) $(CXXFLAGS) -fPIC $(INCLUDE) -c -o $@ $<
# Link modules into a shared executable
CLINK = $(CXX) $(CXXFLAGS) -shared

# Tests
TEST_SRC = tests/tests.cpp
TEST = test
