---
project_name: Ulquikit
authors: Duong H. Nguyen <cmpitg AT gmailDOTcom>
title: Ulquikit
short_description: Another literate programming tool in Racket
version: 0.0.1
---

# Ulquikit Internals #

This document describes how Ulquikit works and how it's used to bootstrap
itself.  For how to use Ulquikit in your project, see
[Ulquikit guide](guide.html).

## Concepts and definitions used in Ulquikit ##

Please see [Ulquikit guide](guide.html#concepts).

## How Ulquikit bootstraps itself ##

Ulquikit's project structure:

```
ulquikit/
  generated_docs/
  generated_src/
  src/
    bootstrap/
      css/
      js/
      templates/
    config.yaml
    internals.md
    guide.md
```

Explanation:

* `generated_docs/` contains *generated docs*.
* `generated_src/` contains *generated source*.

## Notes ##

* All CSS and JavaScript files are minified with
  [Node Minify](https://github.com/srod/node-minify).

## License ##

Ulquikit is distributed under the terms of the GNU General Public License v3.
See [`COPYING`](COPYING) for further information.
