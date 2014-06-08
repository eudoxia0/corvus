/* A simple test framework */
#include <cstdio>

static long successes = 0;
static long failures = 0;
static long checks = 0;

void pass();
void fail(const char* exp);
float percentage(long n, long total);
void report();

#define TEST(fn) void fn(); void fn()
#define RUN_TEST(fn) puts("  [ test " #fn " ]"); fn()
#define SUITE(name) void name(); void name()
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

float percentage(long n, long total) {
  return (n*100)/total;
}

void report() {
  if(checks > 0) {
    printf("Ran %li checks, of which %li (%4.2f%%) succeeded and %li (%4.2f%%) failed.\n",
           checks,
           successes,
           percentage(successes, checks),
           failures,
           percentage(failures, checks));
  }
}

