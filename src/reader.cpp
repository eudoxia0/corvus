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

SExp* readStream(FILE* stream) {
  std::string token_text;
  char c = (char)getc(stream);
  while(c != EOF) {
    /* If c is a whitespace character, discard it and re-enter the loop, unless
       we are reading a token, in which case the whitespace terminates it. */
    if(isspace(c)) {
      if(completeToken(token_text))
        /* Return the complete token */
        return makeSExp(token_text.c_str(), IDENTIFIER);
      else
        continue;
    }
    /* If c is a dispatching or non-dispatching macro character, its associated
       function is called */
    /* Until I implement reader macros satisfactorily, we'll cheat and directly
       implement the 'macros' for parentheses and quotes. */
    if(c == '(') {
      return readDelimitedSequence(stream, ')');
    }
    /* Any character that is not a macro character is a constituent character
       of a token. At this point, a token begins to be accumulated */
    token_text += c;
    c = (char)getc(stream);
    if(peekc(stream) == ')')
      return makeSExp(token_text.c_str(), IDENTIFIER);
  }
  /* End-of-file was reached */
  if(completeToken(token_text))
    return makeSExp(token_text.c_str(), IDENTIFIER);
  else
    /* EOF was reached before a complete token was read */
    return NULL;
}

SExp* readDelimitedSequence(FILE* stream, char delimiter) {
  std::vector<SExp*> list;
  SExp* current = readStream(stream);
  do {
    list.push_back(current);
    current = readStream(stream);
  } while(peekc(stream) != delimiter);
  if(list.size() > 0)
    return vectorToList(list);
  else
    return NULL;
}
