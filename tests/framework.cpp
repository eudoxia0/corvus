/* A simple test framework */
#include <cstdio>

long successes = 0;
long failures = 0;
long checks = 0;

#define TEST(fn) void fn()
#define RUN_TEST(fn) puts("  [ test " #fn " ]"); fn()
#define SUITE(name) void name()
#define RUN_SUITE(name) puts("[ suite " #name " ]"); name()

#define ASSERT(exp) ((exp) ? pass() : fail((const char*)#exp));

void pass() {
  puts("    [ OK ]");
  successes++;
  checks++;
}

void fail(const char* exp) {
  printf("    [ FAIL ] %s\n", exp);
  failures++;
  checks++;
}
