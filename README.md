# Ulquikit #

Yet another
[literate programming](http://en.wikipedia.org/wiki/Literate_programming) tool
written in [Racket](http://racket-lang.org/).

## Literate programming at a glance ##

Literate programming is a term coined by
prof. [Donald E. Knuth](http://en.wikipedia.org/wiki/Donald_Knuth) in 1984,
describing a new way of **thinking about and writing** computer programs.

In literate programming:

* Writing programs is writing a story.

* Document is not optional but mandatory.

* A program is a real *flow* of words and text.

## Misunderstandings ##

Tools like [Docco](http://jashkenas.github.io/docco/) are **not** literate
programming tools.  They are essentially documentation tools with literate
programming *style*.

*Literate programming* is a way of writing (not just coding, but **writing**)
by producing documents (**not** documentation).  It's not about *reading*
source code and documentation but *writing*, *reusing* and *extending* them.
It's not about *generating beautiful documentation*.

You can't focus on reading the doc.  You can't define snippets and move them
around.  You can't reuse snippets.  Again, that's **not** literate
programming.

In traditional way of programming, programmers *write code and supply
documentation*.  In literate programming, programmers *write documents and
supply code*.

*Calling* tools like Docco literate programming tools causes huge
misunderstanding.  Not only do they not get the art of it, they spread wrong
ideas about Knuth's work as well.

## Why literate programming? ##

* Don't you like
  [Qt-style tutorials](http://qt-project.org/doc/qt-5.0/qtdoc/qtexamplesandtutorials.html)?

* Encourages high-quality programs, since every thought has to be expressed
  clearly, making design decision more obvious.

* Great help in **understanding, maintaining, extending** programs.

* Bugs that start from designing and thinking process are easier to detect and
  fixed.

* Programs are more well-constructed.

* It's
  [the act of communication between people](https://www.youtube.com/watch?v=Av0PQDVTP4A),
  not people and machines.

## Why *not* literate programming? ##

* Harder to debug using traditional tools.

* Rapid development (perhaps?  To me, I don't find any problems with literate
  programming in rapid development at all).

* Write doc, write doc, and write doc.

## Why Ulquikit? ##

* Using [Markdown](http://en.wikipedia.org/wiki/Markdown) as its markup
  language, rather than [LaTeX](http://en.wikipedia.org/wiki/LaTeX) the
  complex beast.

* Fully hackable and extensible.

* Making literate programming fun and productive again.

## Examples ##

## Trivia ##

* Ulquikit is named after
  [Ulquiorra Schiffer](http://en.wikipedia.org/wiki/Ulquiorra#Ulquiorra_Schiffer),
  the 4th and is considered the most dangerous
  [Espada](http://en.wikipedia.org/wiki/Ulquiorra#Espada).

* Ulquikit is written in literate programming itself using some small scripts
  to bootstrap residing in `src/bootstrap` directory.

## License ##

This project is distributed under the terms of the GNU General Public
License v3.  See `COPYING` for further information.

## Copyright ##

Copyright 2013-2014 © Duong H. Nguyen <cmpitg AT gmailDOTcom>
