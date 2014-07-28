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

To quote prof. Knuth:

> Let us change our traditional attitude to the construction of programs:
> Instead of imagining that our main task is to instruct a computer what to
> do, let us concentrate rather on explaining to human beings what we want a
> computer to do.
>
> The practitioner of literate programming can be regarded as an essayist,
> whose main concern is with exposition and excellence of style. Such an
> author, with thesaurus in hand, chooses the names of variables carefully and
> explains what each variable means. He or she strives for a program that is
> comprehensible because its concepts have been introduced in an order that is
> best for human understanding, using a mixture of formal and informal methods
> that reinforce each other.

-- Donald Knuth. "Literate Programming (1984)" in Literate Programming. CSLI,
1992, pg. 99.

## Misunderstandings ##

Note that my personal idea is not included here.  Everything is drawn directly
from Knuth's work.

The most common misunderstanding is that tools like
[Docco](http://jashkenas.github.io/docco/) are considered literate programming
tools.  They are not.  They are documentation tools with literate programming
style.

*Literate programming* is a way of **programming**, not *generating beautiful
documentation*.

In traditional way of programming, programmers *write code and supply
documentation*.  In literate programming, programmers *write documents and
supply code*.  The source code is there as a side effect of the document, not
vice versa.

## Why literate programming? ##

* Don't you like
  [Qt-style tutorials](http://qt-project.org/doc/qt-5.0/qtdoc/qtexamplesandtutorials.html)?

* Encourages high-quality programs, since every thought has to be expressed
  clearly, making design decision more obvious.

* Great help in **understanding, maintaining, extending** programs.

* Bugs that start from designing and thinking process are easier to detect and
  fix.

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

* Using [AsciiDoc](http://asciidoc.org/) as its markup language, rather than
  [LaTeX](http://en.wikipedia.org/wiki/LaTeX) the complex beast:

  Why not Markdown?  AsciiDoc is more of a semantic markup language and its
  features are more sophisticated.

* APIs are **designed for human**, not machines.

* Fully hackable and extensible.

* Making literate programming fun and productive again.

## Examples ##

## Requirements ##

* Racket 6.0+

* Ruby 1.9.3+

* [AsciiDoctor](http://asciidoctor.org/)

## Installation ##

Make sure you have Racket and Ruby installed.  One of the best way to install
Ruby is to use RVM.  Visit:

* http://racket-lang.org for Racket installation instruction

* https://rvm.io/ for RVM and Ruby installation instruction

### Quick Installation (not yet available) ###

#### If you have Ruby installed ####

For Debian-based system:

```sh
raco install ulquikit
ulquikit install-dependencies
```

#### If you don't have Ruby installed ####

```sh
raco install ulquikit
ulquikit install-dependencies-with-ruby
```

This will install Ruby using RVM with single-user mode.  Behind the scene, the
following commands are executed:

```sh
\curl -sSL https://get.rvm.io | bash -s stable
echo "source $HOME/.rvm/scripts/rvm" >> ~/.profile
source $HOME/.rvm/scripts/rvm
rvm install 1.9
rvm use 1.9 --default
gem install asciidoctor
```

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
