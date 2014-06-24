#include "reader.hpp"

int main(int argc, char **argv) {
  for(;;) {
    printf("> ");
    SExp* input = readStream(stdin);
    printf("%s\n", print(input).c_str());
  }
  return 0;
}
