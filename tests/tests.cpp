#include "reader.hpp"
#include "framework.cpp"
#include <iostream>

TEST(print_empty) {
  ASSERT(print(NULL) == "()");
}

TEST(print_atom) {
  SExp* a = makeSExp("a", IDENTIFIER);
  ASSERT(print(a) == "a");
}

TEST(consing) {
  SExp* a = makeSExp("a", IDENTIFIER);
  SExp* scons = cons(a, NULL);
  ASSERT(print(scons) == "(a)");
}

TEST(multiple_consing) {
  SExp* a = makeSExp("a", IDENTIFIER);
  SExp* b = makeSExp("b", IDENTIFIER);
  SExp* c = makeSExp("c", IDENTIFIER);
  SExp* mcons = cons(a, cons(b, cons(c, NULL)));
  ASSERT(print(mcons) == "(a b c)");
}

TEST(free_atom) {
  SExp* atom = makeSExp("test", IDENTIFIER);
  freeSExp(atom);
  pass();
}

TEST(free_list) {
  SExp* a = makeSExp("a", IDENTIFIER);
  SExp* b = makeSExp("b", IDENTIFIER);
  SExp* c = makeSExp("c", IDENTIFIER);
  SExp* mcons = cons(a, cons(b, cons(c, NULL)));
  freeSExp(mcons);
  pass();
}

SUITE(ast) {
  RUN_TEST(print_empty);
  RUN_TEST(print_atom);
  RUN_TEST(consing);
  RUN_TEST(multiple_consing);
  RUN_TEST(free_atom);
  RUN_TEST(free_list);
}

int main(int argc, char **argv) {
  RUN_SUITE(ast);
  report();
}
