#include <string>
#include <vector>
#include <cassert>
#include <regex>

enum SExpType {
  LIST, INTEGER, FLOAT, STRING, IDENTIFIER
};

struct Atom {
  unsigned long line; /* 'line' and 'col' are exactly what it says on the
                         tin. */
  unsigned long col;
  const char* val;   /* The data in a SExp is meant to be immutable,
                        so we don't bother with an std::string.
                        Additionally, this lets us use a traditional
                        union rather than the OOP approach. */
};

/* SExp: The S-expression data structure. The `first` and `rest` fields
   correspond to `car` and `cdr`, respectively. */
struct SExp {
  SExpType type;
  union SExpContent {
    struct List {
      SExp* first;        /* The 'first' and 'rest' fields correspond to 'car'
                             and 'cdr', respectively. */
      SExp* rest;
    } list;
    Atom atom;
  } content;
};

/* The following macros are here to reduce the verbosity of accessing the
   various nested union members in the SExp structure. */
#define listp(sexp) (sexp->type == LIST)
#define atomp(sexp) !(listp(sexp))
#define first(sexp) (sexp->content.list.first)
#define rest(sexp) (sexp->content.list.rest)
#define val(sexp) (sexp->content.atom.val)

/* Create an atom from a byte array and a SExpType; */
SExp* makeSExp(std::string val, SExpType type);

/* The 'cons' function puts 'first' at the front of the 'rest' list. */
SExp* cons(SExp* first, SExp* rest);

/* Return a printed representation of an S-expression. */
std::string print(SExp* sexp);

/* Free a SExp. */
void freeSExp(SExp* sexp);

/* Return the length of a SExp */
size_t length(SExp* list);

/* Utility: Turn a SExp into a vector. */
std::vector<SExp*> sexpToVec(SExp* sexp);

/* Determine the atom type of a string of text */
SExpType classify(std::string str);

/* Create an atom from a string, automatically classifying it */
SExp* makeAtom(std::string val);

/* Test whether the text of 'atom' equals 'text' */
int atomeq(SExp* atom, const char* text);
