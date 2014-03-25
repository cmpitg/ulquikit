---
project_name: Ulquikit
authors: Duong H. Nguyen <cmpitg AT gmailDOTcom>
title: Ulquikit
short_description: Another literate programming tool in Racket
version: 0.0.1
---

# Ulquikit #

This is full documentation and source code about Ulquikit.  Ulquikit is
written using Ulquikit itself.  On how to use Ulquikit in your project, see
[Ulquikit guide](guide.html).

## Philosophy ##

* Convention over configuration.

## Configuration ##

Some environment variables:

* `ULQUI_CONFIG` is the path to your local configuration directory of
  Ulquikit.  If it's empty, your local config dir is `$HOME/.config/ulquikit`
  by default.

## Subcommands ##

Ulquikit subcommands are an executables residing in either
`$ULQUI_HOME/subcommands/` or `$ULQUI_CONFIG/subcommands/<subcommand-name>`
directory.

## Extracting code snippet ##

Each code snippet

### [discuss] Why (not) use environment variable? ###

* Pros:

  - `SOMETHING=some-value /path/to/program` is very convenient for testing or
    creating multiple executables for different purposes.

* Cons

  - Environment variables are sometimes hard to control because the program
    may be run in different environments.  E.g.  Ulquikit is called from an
    Emacs session, which was started by Emacs server daemon that doesn't read
    `rc` files.

  - The same effects could be achieved simply by adding a command switch.
    E.g. `ulqui --config-dir /some/path/`.

**Conclusion:**  Don't use environment variables.

