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

extern crate list;
extern crate ast;

use std::io::{BufferedReader, File};
use std::io;
use list::Nil;
use ast::{SExp, atom_from_str};

enum Stream {
    StdStream(BufferedReader<io::stdio::StdReader>),
    FileStream(BufferedReader<io::File>)
}

pub struct Reader {
    line: i64,
    col: i64,
    buf: Stream
}

/* Create a new Reader from stdin */
pub fn stdin_reader() -> Reader {
    Reader {
        line: 0,
        col: 0,
        buf: StdStream(io::stdin())
    }
}

pub fn file_reader(pathname: &str) -> Reader {
    let handle = File::open(&Path::new(pathname));
    match handle {
        Ok(file) => {
            let buf = BufferedReader::new(file);
            Reader {
                line: 0,
                col: 0,
                buf: FileStream(buf),
            }
        },
        Err(why) => fail!("Failed to open file {} ({}).",
                          pathname,
                          why.desc)
    }
}

/* Get the next character in the stream, advancing the cursor */
fn nextchar(reader: &mut Reader) -> Option<char> {
    let res = match reader.buf {
        StdStream(ref mut buf) => buf.read_char(),
        FileStream(ref mut buf) => buf.read_char()
    };
    reader.col = reader.col + 1;
    match res {
        Ok(c) => {
            if c == '\n' {
                reader.line = reader.line + 1
            }
            Some(c)
        },
        Err(_) => None
    }
}

/* This constant defines the maximum number of bytes a reader macro can have. By
   comparison, Common Lisp allows only two (Two-character macros are a single
   character prefixed by a 'dispatching macro character', typically #). This is
   set arbitrarily. */
static max_macro_len : i8 = 6;

/* A helper function for readStream. A token is complete if it has at least one
   byte. */
fn complete_token(tok: &String) -> bool {
    tok.len() > 0
}

/* For information on the reader algorithm, check the Reader chapter of the
   documentation. */
pub fn read_stream(reader: &mut Reader) -> SExp {
    let mut token_text = String::new();
    let mut c;
    loop {
        c = nextchar(reader);
        /* Match the char */
        match c {
            /* What kind of character is it? */
            Some(c) => {
                /* If c is a whitespace character, discard it and re-enter the
                   loop, unless we are reading a token, in which case the
                   whitespace terminates it. */
                if c.is_whitespace() {
                    if complete_token(&token_text) {
                        return atom_from_str(token_text,
                                             reader.line,
                                             reader.col);
                    } else {
                        continue;
                    }
                }

                /* If c is a dispatching or non-dispatching macro character, its
                   associated function is called */
                /* Until I implement reader macros satisfactorily, we'll cheat
                   and directly implement the 'macros' for parentheses and
                   quotes. */
                if c == '(' {
                    return read_delim_sequence(reader, ')');
                };

                /* Any character that is not a macro character is a constituent
                   character of a token. At this point, a token begins to be
                   accumulated */
                token_text.push_char(c);

                /* Handle terminating macro characters */
                if c == ')' {
                    return atom_from_str(token_text,
                                         reader.line,
                                         reader.col);
                };
            },
            None => {
                /* EOF */
                break;
            }
        }
    }
    if complete_token(&token_text) {
        atom_from_str(token_text, reader.line, reader.col)
    } else {
        Nil
    }
}

/* A simple function to facilitate reading delimited sequences. It is used to
   read nested S-expressions, as well as array and tuple literals. */
fn read_delim_sequence(reader: &mut Reader, delimiter: char) -> SExp {
    /* Read until finding a token that ends with a closing parenthesis */
    Nil
}
