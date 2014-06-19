#include "reader.hpp"

char peekc(FILE* stream) {
  char c = (char)fgetc(stream);
  ungetc(c, stream);
  return c;
}

/* The cursor: These global variables are increased throughout the execution of
   the compiler. */

unsigned long curline = 0;
unsigned long curcol = 0;

char nextchar(FILE* stream) {
  char c = (char)getc(stream);
  if(c == '\n') {
    curline++;
    curcol = 0;
  } else {
    curcol++;
  }
  return c;
}

SExp* concat(SExp* list1, SExp* list2) {
  if(list1 == NULL) {
    return list2;
  } else if (rest(list1) && !listp(rest(list1))) {
    return cons(first(list1), list2);
  } else {
    return cons(first(list1),
                concat(rest(list1), list2));
  }
}

SExp* reverse(SExp* list) {
  if(list) {
    return concat(reverse(rest(list)),
                  cons(first(list), NULL));
  } else {
    return NULL;
  }
}

bool completeToken(std::string tok_text) {
  return tok_text.length() > 0;
}

SExp* readStream(FILE* stream) {
  std::string token_text;
  char c = nextchar(stream);
  while(c != EOF) {
    /* If c is a whitespace character, discard it and re-enter the loop, unless
       we are reading a token, in which case the whitespace terminates it. */
    if(isspace(c)) {
      if(completeToken(token_text))
        /* Return the complete token */
        return makeAtom(token_text);
      else {
        c = nextchar(stream);
        continue;
      }
    }
    /* If c is a dispatching or non-dispatching macro character, its associated
       function is called */
    /* Until I implement reader macros satisfactorily, we'll cheat and directly
       implement the 'macros' for parentheses and quotes. */
    if(c == '(') {
      return readDelimitedSequence(stream, ')');
    }
    if(c == ')') {
      ungetc(c, stream);
      return makeAtom(token_text);
    }
    /* Any character that is not a macro character is a constituent character
       of a token. At this point, a token begins to be accumulated */
    token_text += c;
    c = nextchar(stream);
  }
  /* End-of-file was reached */
  if(completeToken(token_text))
    return makeAtom(token_text);
  else
    /* EOF was reached before a complete token was read */
    return NULL;
}

SExp* readDelimitedSequence(FILE* stream, char delimiter) {
  SExp* list = NULL;
  SExp* current = NULL;
  while(1) {
    char peek = peekc(stream);
    if((peek == delimiter) || (peek == EOF)) {
      nextchar(stream);
      return reverse(list);
    }
    current = readStream(stream);
    list = cons(current, list);
  }
  return NULL;
}
