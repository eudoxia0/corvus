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
