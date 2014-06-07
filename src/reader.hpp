#include "ast.hpp"
#include <cstdio>
#include <cctype>
#include <vector>

/* Utilities */
int peekc(FILE *stream);
SExp* vectorToList(std::vector<SExp*> list);

/* This constant defines the maximum number of bytes a reader macro can have. By
   comparison, Common Lisp allows only two (Two-character macros are a single
   character prefixed by a 'dispatching macro character', typically #). This is,
   arbitrarily, set to six. */
#define MAX_MACRO_LEN 6

/* This is the prototype of a reader macro function: Takes a stream as its input
   and returns a pointer to a SExp. */
typedef SExp* (*readerFunction)(FILE*);

/* A reader macro is a series of matching bytes and its associated macro
   function. */
struct ReaderMacro {
  char bytes[MAX_MACRO_LEN];
  readerFunction fn;
};

typedef std::vector<ReaderMacro> MacroList;

/* A readtable is basically just a collection of reader macros. */
struct ReadTable {
  MacroList macros;
};

/* A helper function for readStream. A token is complete if it has at least one
   byte. */
bool completeToken(std::string tok_text);

/* readStream is roughly built along the lines of the Common Lisp reader.

   See: http://www.lispworks.com/documentation/lw51/CLHS/Body/02_b.htm */
SExp* readStream(FILE* stream);

/* A simple function to facilitate reading delimited sequences. It is used to
   read nested S-expressions, as well as array and tuple literals. */
SExp* readDelimitedSequence(FILE* stream, char delimiter);
