# Ulquikit

Yet another
[literate programming](http://en.wikipedia.org/wiki/Literate_programming)
tool.

## Literate programming at a glance

Literate programming is a term coined by the great
Prof. [Donald E. Knuth](http://en.wikipedia.org/wiki/Donald_Knuth) in 1984,
describing a new way of writing computer programs.

In literate programming:

* Writing programs is writing a story.

* Documentation is not optional but mandatory.

* A program is a real *flow* of words and text.

## Misunderstandings

Some tools like [Docco](http://jashkenas.github.io/docco/) and its *partners
in crime* are **not** literate programming tools.

*Literate programming* is the term created by Prof. Knuth to indicate a way of
coding by writing document (**not** documentation).  It's not about *reading*
source code and documentation but *writing*, *reusing* and *extending* them.
It's not about *generating beautiful documentation* at all!

You can't focus on reading the doc.  You can't define snippets and move them
around.  You can't reuse snippets.  Again, that's **not** literate
programming for heaven's sake!

Nothing personal, but the author(s) Docco have made a terrible mistake by
*calling* those tools literate programming tools.  Not only do they not get
the art of it, they spread the wrong idea about Knuth's work as well.

Tools are supposed to get the job done, but please respect the orginal author.
Name them your way.

## Why literate programming?

* Don't you like
  [Qt-style tutorials](http://qt-project.org/doc/qt-5.0/qtdoc/qtexamplesandtutorials.html)?

* Encourages high-quality programs, since every thought has to be expressed
  clearly, making design decision more obvious.

* Great help in understanding, maintaining, extending programs.

* Bugs that start from designing and thinking process are easier to detect and
  fixed.

* Programs are more well-constructed.

## Why *not* literate programming?

* Harder to debug using traditional tools.

* Rapid development (perhaps?  To me, I don't find any problems with literate
  programming in rapid development at all).

* Write doc, write doc, and write doc.

## Why Ulquikit?

* Using [Markdown](http://en.wikipedia.org/wiki/Markdown) as its markup
  language, rather than [LaTeX](http://en.wikipedia.org/wiki/LaTeX) the
  complex beast.

* Fully hackable and extensible.

* Making literate programming fun and productive again.

## Examples

A typical example looks like the following:

    ```scheme
    === license-header ===
    ;; -*- mode: scheme -*-

    ;;
    ;; Copyright 2013 © Nguyễn Hà Dương (cmpitgATgmailDOTcom)
    ;;
    ;; This file is part of Foobarquus.
    ;;
    ;; Foobarquus is free software: you can redistribute it and/or modify it
    ;; under the terms of the GNU General Public License as published by
    ;; the Free Software Foundation, either version 3 of the License, or
    ;; (at your option) any later version.
    ;;
    ;; Foobarquus is distributed in the hope that it will be useful, but
    ;; WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    ;; General Public License for more details.
    ;;
    ;; You should have received a copy of the GNU General Public License
    ;; along with Foobarquus.  If not, see <http://www.gnu.org/licenses/>.
    ;;

    ======

    _____ file: main.scm _____
    <<< license-header <<<


    __________

## Trivia

* Ulquikit is named after
  [Ulquiorra Schiffer](http://en.wikipedia.org/wiki/Ulquiorra#Ulquiorra_Schiffer),
  the 4th and is considered the most dangerous
  [Espada](http://en.wikipedia.org/wiki/Ulquiorra#Espada).

* Ulquikit is written in literate programming itself using some small scripts
  to bootstrap residing in `src/bootstrap` directory.

## License

This project is distributed under the terms of the GNU General Public
License v3.  See `COPYING` for further information.

## Copyright

Copyright 2013 © Duong H. Nguyen <cmpitg AT gmailDOTcom>
