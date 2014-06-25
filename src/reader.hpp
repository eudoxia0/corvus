#include "ast.hpp"
#include <cstdio>
#include <cctype>
#include <vector>
#include <algorithm>

/*
The readtable is essentially a table like this:

=====  ==============
Bytes  Macro Function
=====  ==============
;      readComment
#[     readArrayLiteral
#{     readRecordLiteral
=====  ==============

The readStream function reads bytes one at a time. The first byte it reads (At
the beginning of the input stream, or when it's just finished a previous token)
is searched in the readtable: If it's not in the first position of any entry,
it's just a regular token. If it is, then the function sees its about to receive
a reader macro.

Now, the readtable has two constraints:

* No reader macro's bytes can be a substring of another's. That is, two entries
  like '#{' and '#' can't exist in the same readtable, because the reader has no
  way to differentiate between the two. That is, all entries that start with a
  given byte must have the same length.
* Reader macros are sorted by character value of its bytes.

This helps search a lot. For one, if the reader is reading a macro that starts
with a given character % and there are three reader macros that start with that
character in the entry, the reader will find the first, and then

The matching algorithm is as follows:

* Read the first byte, and having found it in the readtable, record the position
  of the first matching reader macro in the table as 'pos'.

* Record the length of that first macro's match string (And, because of the
  constraint that all macros have the same length, the length of all subsequent
  macros) as 'len'.

* If 'len' equals 1, then the macro has been found, and its associated reader
  function is called.

* Otherwise, 'len' bytes are read from the input.

* If the stream ends before all bytes are read, or the bytes don't match any
  reader macro, an error is signalled.

* Otherwise, the associated reader macro function of the matched macro is
  called.

The S-Expression returned by the reader macro function is then recorded by the
reader, unless it is NULL, in which case an error is signaled.
*/

/* Utilities */

/* Peek at the next character in the stream */
char peekc(FILE* stream);
/* Get the next character in the stream, advancing the cursor */
char nextchar(FILE* stream);
/* Concatenate list1 and list2 */
SExp* concat(SExp* list1, SExp* list2);
/* Reverse list */
SExp* reverse(SExp* list);

/* This constant defines the maximum number of bytes a reader macro can have. By
   comparison, Common Lisp allows only two (Two-character macros are a single
   character prefixed by a 'dispatching macro character', typically #). This is
   set arbitrarily. */
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

/* For information on the reader algorithm, check the Reader chapter of the
documentation. */
SExp* readStream(FILE* stream);

/* A simple function to facilitate reading delimited sequences. It is used to
   read nested S-expressions, as well as array and tuple literals. */
SExp* readDelimitedSequence(FILE* stream, char delimiter);
