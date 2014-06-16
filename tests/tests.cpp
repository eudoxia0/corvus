#include "reader.hpp"
#include "framework.cpp"
#include <iostream>

TEST(print_empty) {
  ASSERT(print(NULL) == "()");
}

TEST(print_atom) {
  SExp* a = makeSExp("a", IDENTIFIER);
  ASSERT(print(a) == "a");
  freeSExp(a);
  pass();
}

TEST(consing) {
  SExp* a = makeSExp("a", IDENTIFIER);
  SExp* scons = cons(a, NULL);
  ASSERT(print(scons) == "(a)");
  freeSExp(scons);
  pass();
}

TEST(multiple_consing) {
  SExp* a = makeSExp("a", IDENTIFIER);
  SExp* b = makeSExp("b", IDENTIFIER);
  SExp* c = makeSExp("c", IDENTIFIER);
  SExp* mcons = cons(a, cons(b, cons(c, NULL)));
  ASSERT(print(mcons) == "(a b c)");
  freeSExp(mcons);
  pass();
}

TEST(concatenating) {
  SExp* a = makeSExp("a", IDENTIFIER);
  SExp* b = makeSExp("b", IDENTIFIER);
  SExp* c = makeSExp("c", IDENTIFIER);
  SExp* s1 = makeSExp("1", IDENTIFIER);
  SExp* s2 = makeSExp("2", IDENTIFIER);
  SExp* s3 = makeSExp("3", IDENTIFIER);
  SExp* list1 = cons(a, cons(b, cons(c, NULL)));
  SExp* list2 = cons(s1, cons(s2, cons(s3, NULL)));
  SExp* list = concat(list1, list2);
  ASSERT(print(list) == "(a b c 1 2 3)");
  freeSExp(list);
  pass();
}

TEST(reversing) {
  SExp* a = makeSExp("a", IDENTIFIER);
  SExp* b = makeSExp("b", IDENTIFIER);
  SExp* c = makeSExp("c", IDENTIFIER);
  SExp* list = cons(a, cons(b, cons(c, NULL)));
  SExp* reversed = reverse(list);
  ASSERT(print(reversed) == "(c b a)");
  freeSExp(reversed);
  pass();
}

TEST(nested_list) {
  SExp* a = makeSExp("a", IDENTIFIER);
  SExp* b = makeSExp("b", IDENTIFIER);
  SExp* c = makeSExp("c", IDENTIFIER);
  SExp* nest1 = cons(a, cons(b, cons(c, NULL)));
  SExp* s1 = makeSExp("1", IDENTIFIER);
  SExp* s2 = makeSExp("1", IDENTIFIER);
  SExp* s3 = makeSExp("1", IDENTIFIER);
  SExp* nest2 = cons(cons(s1, cons(s2, cons(s3, NULL))), NULL);
  SExp* nest = cons(nest1, nest2);
  ASSERT(print(nest) == "((a b c) (1 1 1))");
  freeSExp(nest);
  pass();
}

SUITE(ast) {
  RUN_TEST(print_empty);
  RUN_TEST(print_atom);
  RUN_TEST(consing);
  RUN_TEST(multiple_consing);
  RUN_TEST(concatenating);
  RUN_TEST(reversing);
  RUN_TEST(nested_list);
}

TEST(read_atom) {
  FILE* file = fopen("tests/input.1.txt", "r");
  SExp* atom = readStream(file);
  ASSERT(print(atom) == "a");
  freeSExp(atom);
  fclose(file);
}

TEST(read_list) {
  FILE* file = fopen("tests/input.2.txt", "r");
  SExp* list = readStream(file);
  ASSERT(print(list) == std::string("(a b c)"));
  freeSExp(list);
  fclose(file);
}

TEST(read_nested_lists) {
  FILE* file = fopen("tests/input.3.txt", "r");
  SExp* list = readStream(file);
  ASSERT(print(list) == std::string("(((a) b) c)"));
  freeSExp(list);
  fclose(file);
  file = fopen("tests/input.4.txt", "r");
  list = readStream(file);
  ASSERT(print(list) == std::string("(a (b (c)))"));
  freeSExp(list);
  fclose(file);
}

SUITE(reader) {
  RUN_TEST(read_atom);
  RUN_TEST(read_list);
  RUN_TEST(read_nested_lists);
}

int main(int argc, char **argv) {
  RUN_SUITE(ast);
  RUN_SUITE(reader);
  report();
}
