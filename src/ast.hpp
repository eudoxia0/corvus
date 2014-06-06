#include <string>
#include <cassert>

enum SExpType {
  LIST, INTEGER, REAL, STRING, IDENTIFIER
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
    struct Atom {
      unsigned long line; /* 'line' and 'col' are exactly what it says on the
                             tin. */
      unsigned long col;
      const char* val;   /* The data in a SExp is meant to be immutable,
                            so we don't bother with an std::string.
                            Additionally, this lets us use a traditional
                            union rather than the OOP approach. */
    } atom;
  } content;
};

/* The following macros are here to reduce the verbosity of accessing the
   various nested union members in the SExp structure. */
#define listp(sexp) (sexp->type == LIST)
#define first(sexp) (sexp->content.list.first)
#define rest(sexp) (sexp->content.list.rest)
#define line(sexp) (sexp->content.atom.line)
#define col(sexp) (sexp->content.atom.col)
#define val(sexp) (sexp->content.atom.val)

/* Create an atom from an std::string and a SExpType; */
SExp* makeSExp(std::string data, SExpType type);

/* The 'cons' function puts 'first' at the front of the 'rest' list. */
SExp* cons(SExp* first, SExp* rest);

/* The opposite of cons: Put 'obj' at the end of 'list'. */
void push(SExp* list, SExp* obj);

/* Return a printed representation of an S-expression. */
std::string print(SExp* sexp);

/* Free a SExp. */
void freeSExp(SExp* sexp);
