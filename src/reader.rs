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

mod reader {
    use std::io;

    struct Reader {
        line: i64,
        col: i64,
        buf: io::BufferedReader<Box<io::Reader>>
    }

    /* Get the next character in the stream, advancing the cursor */
    fn nextchar(reader: Reader) -> char { 'a' }

    /* This constant defines the maximum number of bytes a reader macro can
       have. By comparison, Common Lisp allows only two (Two-character macros
       are a single character prefixed by a 'dispatching macro character',
       typically #). This is set arbitrarily. */
    static max_macro_len : i8 = 6;

    /* A helper function for readStream. A token is complete if it has at least
       one byte. */
    fn completeToken(tok: String) -> bool { false }

    /* For information on the reader algorithm, check the Reader chapter of the
       documentation. */
    fn readStream(reader: Reader) -> SExp { }

    /* A simple function to facilitate reading delimited sequences. It is used
       to read nested S-expressions, as well as array and tuple literals. */
    fn readDelimitedSequence(reader: Reader, delimiter: char) -> SExp { }
}
