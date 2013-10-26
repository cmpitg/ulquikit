---
title: Ulquikit Guide
authors: Duong H. Nguyen <cmpitg AT gmailDOTcom>
short_description: How to use Ulquikit
---

# Ulquikit Guide

## Variables definition

* Format:

  - Begun and ended with a single line containing 3 dashes (`---`).

  - For each var: `var_name: value`.

  - Variable names can **only** contain letters, numbers, dash (`-`), or
    underscore (`_`).  This restriction is to keep the names readable and
    clean.

  - Variable values are string, except for `authors` (which is an array of
    strings).

* User-defined variables (TODO).

## Theming

The first compilation process produces *documents* and *source code*.  If the
documents are in HTML process, they can be themed.

### Structure

```
src/themes/<theme-name>
  images/
  scripts/
  styles/
  info.yaml
  page.html
```

* Images, in `<theme-name>/images/`

* JavaScript, or source code that compiles to JavaScript, in
  `<theme-name>/scripts/`

* CSS, or source code that compiles to CSS, in `<theme-name>/styles/`

* **Not more than one template**, that is used to structure the HTML output of
  your Markdown document(s): `<theme-name>/page.html`.  If the theme contains
  no template, the default one is used:

  ```html
  <!doctype html>
  <html>
    <head>
      <title>%{title}</title>
      %{css}
    </head>
    <body>
      %{content}
      %{js}
    </body>
  </html>
  ```

* Meta-information about the theme is placed in `<theme-name>/info.yaml` in
  [YAML](http://en.wikipedia.org/wiki/YAML) format.  The following fields are
  supported, opt-in:

  - `name`: human-readable name of the theme
  - `authors`: list of authors, separated by a comma (`,`)
  - `license`: the license that theme is released under
  - `url`: URL of the theme
  - `fetch_command`: command to fetch the theme
  - `update_command`: command to update the theme

  E.g. here are the meta-information of the `default` theme:

  ```yaml
  name: Github-like
  authors: Duong H. Nguyen (@cmpitg)
  license: GPL v3
  ```

### Creating a theme

To create a theme, simply create the directory structure mentioned above and
put it into `src/themes/<theme-name>`.  A theme name *should* only contain
alphanumberic characters and/or `_` and/or `-`.  The skeleton of a theme could
be created with the command:

```sh
ulqui theme new <theme-name>
```

## Templating

A template is used to insert the result of the compilation process of your
Markdown documents.

By default:

* If no template is defined, the following basic template is used:

  - `%{title}` will be replaced by the variable `title` in your Markdown
    document(s).

  - `%{css}` will be replaced by the list of CSS files produced in
    `src/themes/<theme-name>/css/`, *in alphabetical order*.

  - `%{js}` will be replaced by the list of JavaScript files produced in
    `src/themes/<theme-name>/js/`

  - `%{content}` will be replaced by HTML produced from your Markdown document(s).

## Command line

* To invoke help:

  ```sh
  ulqui help
  ```

* To invoke help on a specific command:

  ```sh
  ulqui help <command>
  ```

  or

  ```sh
  ulqui <command> --help
  ```

* To initialize a Ulquikit project:

  ```sh
  ulqui init <project-name>
  ```

  This will create an empty project structure with default theme (named `default`).

* To create a new theme:

  ```sh
  ulqui theme new <theme-name>
  ```

* To install a new theme from a URL (current supported Github):

  ```sh
  ulqui theme install <theme-name>
  ulqui theme install github.com/<user>/<project>
  ```

* To build documentation:

  ```sh
  ulqui build docs
  ```

* To extract source code:

  ```sh
  ulqui build source
  ```

* To build both docs and source code:

  ```sh
  ulqui build
  ```

  which is equivalent to:

  ```sh
  ulqui build docs
  ulqui build source
  ```
