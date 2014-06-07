#include "reader.hpp"

int peekc(FILE *stream) {
    int c = fgetc(stream);
    ungetc(c, stream);
    return c;
}

SExp* vectorToList(std::vector<SExp*> list) {
  SExp* sexp = list[0];
  for(size_t i = 0; i < list.size(); i++) {
    push(sexp, list[i]);
  }
  return sexp;
}

bool completeToken(std::string tok_text) {
  return tok_text.length() > 0;
}
