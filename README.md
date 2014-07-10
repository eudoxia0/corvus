# If you are here, you were probably looking for Hylas Lisp

This project is being rewritten. Please check back in a few megaseconds.

# What is this?

A statically-typed, low-level dialect of Lisp that targets LLVM.

# Why?

The era of dynamic languages if over. There is currently a 'race to the bottom':
Languages like [Rust](http://www.rust-lang.org/) and
[Nimrod](http://nimrod-lang.org/) are merging the world of low-level programming
with high-level compiler features. [cmacro](https://github.com/eudoxia0/cmacro)
is another one of my contributions to this movement.

# Features

* Static typing with partial inference.
* Manual memory management with multiple safety guarantees.
* Common Lisp Conditions and Restarts for error handling.
* Macros and Common Lisp-style reader macros.
* Runtime optional.

# Anti-Features

* Garbage Collection.
* Exceptions.

# Status

See the [Trello board](https://trello.com/b/1W0DdGwx/corvus).

Please don't post this on Hacker News yet.

# Notes

This project was originally called Hylas Lisp. I have been working on it on and
off since early 2012, but I've wanted something along these lines for a much
longer time. It was originally written in C++, then in Common Lisp. Third time's
the charm 😹.

The original repository was overwritten because I wanted a clean slate. I hope
the code, in this third iteration, is to your liking. I have tried my best to
write clean, concise code and comments.

There will eventually be a language reference and manual. For now, most of the
documentation on the compiler implementation is available as comments throughout
the source code.

# Acknowledgments

* The [LLVM compiler infrastructure project](http://llvm.org/), an unparalleled
  contribution to the open source community.
* [Jookia](https://github.com/Jookia), for Unicode advice.
* Paul Graham, for [rekindling](http://paulgraham.com/onlisp.html) my interest
  in Lisp.
* [Danny Hillis](http://en.wikipedia.org/wiki/W._Daniel_Hillis) for the idea of
  Xectors and the [Connection Machine](http://en.wikipedia.org/wiki/Connection_Machine).
* [Thinking Machines Corporation](http://en.wikipedia.org/wiki/Thinking_Machines_Corporation)
  for the implementation of the above, and the concept of *shapes* in the
  [C*](http://en.wikipedia.org/wiki/C*) language.
* [Paul Khuong](http://www.pvk.ca/) for a Common Lisp
  [implementation](https://github.com/pkhuong/Xecto) of Xectors.
* [Mozilla](http://www.mozilla.org), for both [Rust](http://www.rust-lang.org/)
  and [sweet.js](http://sweetjs.org/).

# License

Copyright (c) 2014 Fernando Borretti

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
