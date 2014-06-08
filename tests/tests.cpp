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

TEST(free_atoms) {
  freeSExp(a);
  pass();
  freeSExp(b);
  pass();
  freeSExp(c);
  pass();
}

TEST(free_lists) {
  freeSExp(scons);
  pass();
  freeSExp(mcons);
  pass();
}

SUITE(ast) {
  RUN_TEST(print_empty);
  RUN_TEST(print_atom);
  RUN_TEST(consing);
  RUN_TEST(multiple_consing);
  RUN_TEST(free_atoms);
  RUN_TEST(free_lists);
}

int main(int argc, char **argv) {
  RUN_SUITE(ast);
  report();
}
