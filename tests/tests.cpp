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

SExp* makeSimpleList(const char* sa, const char* sb, const char* sc) {
  SExp* a = makeSExp(sa, IDENTIFIER);
  SExp* b = makeSExp(sb, IDENTIFIER);
  SExp* c = makeSExp(sc, IDENTIFIER);
  return cons(a, cons(b, cons(c, NULL)));
}

void freeAndPass(SExp* sexp) {
  freeSExp(sexp);
  /* If we got here, it means we didn't segfault. */
  pass();
}

TEST(multiple_consing) {
  SExp* mcons = makeSimpleList("a", "b", "c");
  ASSERT(print(mcons) == "(a b c)");
  freeAndPass(mcons);
}

TEST(concatenating) {
  SExp* list1 = makeSimpleList("a", "b", "c");
  SExp* list2 = makeSimpleList("1", "2", "3");
  SExp* list = concat(list1, list2);
  ASSERT(print(list) == "(a b c 1 2 3)");
  freeAndPass(list);
}

TEST(reversing) {
  SExp* list = makeSimpleList("a", "b", "c");
  SExp* reversed = reverse(list);
  ASSERT(print(reversed) == "(c b a)");
  freeAndPass(reversed);
}

TEST(nested_list) {
  SExp* nest1 = makeSimpleList("a", "b", "c");
  SExp* nest2 = cons(makeSimpleList("1", "1", "1"), NULL);
  SExp* nest = cons(nest1, nest2);
  ASSERT(print(nest) == "((a b c) (1 1 1))");
  freeAndPass(nest);
}

TEST(length) {
  SExp* list = makeSimpleList("a", "b", "c");
  ASSERT(length(list) == 3);
  freeAndPass(list);
}

SUITE(ast) {
  RUN_TEST(print_empty);
  RUN_TEST(print_atom);
  RUN_TEST(consing);
  RUN_TEST(multiple_consing);
  RUN_TEST(concatenating);
  RUN_TEST(reversing);
  RUN_TEST(nested_list);
  RUN_TEST(length);
}

TEST(ints) {
  SExp* a = makeAtom("1234");
  SExp* b = makeAtom("+1");
  SExp* c = makeAtom("-01234");
  ASSERT(a->type == INTEGER);
  ASSERT(b->type == INTEGER);
  ASSERT(c->type == INTEGER);
  freeSExp(a);
  freeSExp(b);
  freeSExp(c);
}

TEST(floats) {
  SExp* a = makeAtom("3.14");
  SExp* b = makeAtom("0.16");
  SExp* c = makeAtom("+8.5e78");
  ASSERT(a->type == FLOAT);
  ASSERT(b->type == FLOAT);
  ASSERT(c->type == FLOAT);
  freeSExp(a);
  freeSExp(b);
  freeSExp(c);
}

TEST(identifiers) {
  SExp* a = makeAtom("a");
  SExp* b = makeAtom("@some-function");
  SExp* c = makeAtom("test?");
  ASSERT(a->type == IDENTIFIER);
  ASSERT(b->type == IDENTIFIER);
  ASSERT(c->type == IDENTIFIER);
  freeSExp(a);
  freeSExp(b);
  freeSExp(c);
}

SUITE(classifier) {
  RUN_TEST(ints);
  RUN_TEST(floats);
  RUN_TEST(identifiers);
}

TEST(iterate) {
  SExp* list = makeSimpleList("a", "b", "c");
  SExp* last = NULL;
  iter(list, [&last](SExp* elem) { last = elem; });
  ASSERT(print(last) == "c");
}

TEST(map) {
  SExp* list = makeSimpleList("a", "b", "c");
  SExp* newlist = mapcar(list, [](SExp* elem) { return makeSExp("1", IDENTIFIER); });
  ASSERT(print(newlist) == "(1 1 1)");
}

SUITE(iteration) {
  RUN_TEST(iterate);
  RUN_TEST(map);
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
  RUN_SUITE(classifier);
  RUN_SUITE(iteration);
  RUN_SUITE(reader);
  report();
}
