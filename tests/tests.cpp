#include "reader.hpp"
#include "framework.cpp"

SExp* a = makeSExp("a", IDENTIFIER);
SExp* b = makeSExp("b", IDENTIFIER);
SExp* c = makeSExp("c", IDENTIFIER);
SExp* scons = NULL;
SExp* mcons = NULL;

TEST(print_empty) {
  ASSERT(print(NULL) == "()");
}

TEST(print_atom) {
  ASSERT(print(a) == "a");
}

TEST(consing) {
  scons = cons(a, NULL);
  ASSERT(print(scons) == "(a)");
}

TEST(multiple_consing) {
  mcons = cons(a, cons(b, cons(c, NULL)));
  ASSERT(print(mcons) == "(a b c)");
}

SUITE(ast) {
  RUN_TEST(print_empty);
  RUN_TEST(print_atom);
  RUN_TEST(consing);
  RUN_TEST(multiple_consing);
}

int main(int argc, char **argv) {
  RUN_SUITE(ast);
  report();
}
