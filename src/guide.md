---
title: Ulquikit Guide
authors: Duong H. Nguyen <cmpitg AT gmailDOTcom>
short_description: The Ulquikit guide
---

# Ulquikit Guide #

<a name="concept"></a>

## Concepts and definitions ##

- *Documents* or *original documents* or *literate source* refers to your
  literate documents.
- *Generated docs* are the generated HTML/PDF/ePub outputs from your literate
  documents.
- *Generated source* or *generated code* is the extracted source code from
  your literate documents.
- *File snippets* are snippets which define source files in your literate
  documents.
- *Code snippets* are snippets which define a piece of code in your literate
  documents.

## Notes ##

* If first part of an *original document* has the following format:

  ```
  ---
  Some text here
  ---
  ```

  Then that part is interpreted as the document's metadata.  `Some text here`
  should be valid [YAML](http://en.wikipedia.org/wiki/YAML).

### Design decisions ###

* References from source code files in *generated source* to *original
  documents* are relative paths to prevent path conflict between different
  machines.

* *Code snippets* must have *project-wide* unique names, since a snippet could
  be added from different *document*.

---

## Full directory structure

A full Ulquikit project directory structure would look like:

```
<project-name>/
  src/
    images/
    scripts/
    styles/
    templates/
    config/
      custom-commands.rb
      pre-processing.rb
      post-processing.rb
    somefile.md
  COPYING
  README
```

The directory names should speak for themselves.

For the most part, the simplest project looks like the following, and it's
probably *just* what people need:

```
<project-name>/
  src/
    somefile.md
  COPYING
  README.md
```

## Typical workflows

### Simple project

* Write code and doc straightforward by writing Markdown in `src/`.

* The default theme would be used.  Default theme looks like (TODO).

* Docs will be generated in `build_docs/`, code will be generated in
  `build_src/`.

```
<project-name>/
  src/
    somefile.md
  COPYING
  README
```

### Simple project with custom commands

Another simple workflow with Ulquikit would be something similar to:

* Generating docs and source code:

  ```sh
  ulqui build
  ```

* Compile the source code:

  ```sh
  cd build_src/

  # Run whatever command to compile the source code
  make
  # rake compile
  ```

* Run tests or try the program:

  ```sh
  # Still is build_src/ perhaps, depending on your source code structure
  make test
  # or
  # rake test

  # To run
  /path/to/executable
  ```

This is a repeated process and might be very troublesome, thus users might
want to *define one command to rule them all*:

```sh
# Generate docs and source code, then compile, and run tests all at once
ulqui build_all_then_test
```

This is when the project structure looks like:

```
<project-name>/
  src/
    config/
      custom-commands.rb
    somefile.md
  COPYING
  README
```

Please refer to the **Custom commands** section for further information.

### Project with theme

In Ulquikit, document output is themeable.  A simple themed project would look
similar to:

```
<project-name>/
  src/
    images/
    scripts/
    styles/
    templates/
      main.html
    somefile.md
  COPYING
  README
```

Structure explanation:

* `src/images/` contains images.  Images **should be added using relative URL**.

* `src/scripts/` contains files to be compiled to JavaScript, or JavaScript
  themselves.  These scripts will be include in your document(s) *in
  alphabetical order*.

* `src/styles/` contains files to be compiles to CSS, or CSS themselves.
  These CSSs will be include in your document(s) *in alphabetical order*.

* `src/templates/` contains templates to be compile to HTML, or HTML
  themselves.  These HTMLs define the structure of the document output.  The
  filenames should contain alphanumberic characters, and `_` and `-` only.

Users might create those directories manually or use Ulquikit command:

* To create an empty theme:

  ```sh
  ulqui theme init

  # You will then have:
  #
  # src/
  #   images/
  #   scripts/
  #   styles/
  #   templates/
  #     main.html
  ```

  See `src/templates/main.html` for basic template creation, or check Ulquikit
  guide, section **Theming**.

* To create a theme based on a defined theme, you simply *use* that theme:

  ```sh
  ulqui theme use <theme-name>
  ```


### Output directories

```
<project-name>/
  build_docs/
    css/
    img/
    js/
    somefile.html
    somefile.pdf
    somefile.epub
  build_src/
    file_a.rb
    file_b.rb
    file_c.rb
    Rakefile
```

Hope the file names speak for themselves.

## Document variable definition

* Format:

  - Begun and ended with a single line containing 3 dashes (`---`).

  - For each var: `var_name: value`.

  - Variable names can **only** contain letters, numbers, dash (`-`), or
    underscore (`_`).  This restriction is to keep the names readable and
    clean.

  - Variable values are string, except for `authors` (which is an array of
    strings).

* Default variables:

  - `project_name`: name of the project.

  - `authors`: list of authors as array; if there's only one author, `authors`
    is a normal string.

  - `title`: title of the document.

  - `short_description` (optional): a short description about the
    project/document.

  - `version` (optional): project version, should follow
    [semantic versioning](http://semver.org/).

  - `theme` (optional): name of the theme the document uses.  If no `theme` is
    specified, `default` is used.

  - `page_template`: name of the template file *without extension* that the
    document uses.

* User-defined variables (TODO).

## Theming

The first compilation process produces *documents* and *source code*.  If the
documents are in HTML process, they can be themed.

### Structure

```
themes/<theme-name>
  images/
  scripts/
  styles/
  templates/
    page.html
  info.yaml
```

* Images, in `<theme-name>/images/`

* JavaScript, or source code that compiles to JavaScript, in
  `<theme-name>/scripts/`

* CSS, or source code that compiles to CSS, in `<theme-name>/styles/`

* **Not more than one template**, that is used to structure the HTML output of
  your Markdown document(s): `<theme-name>/page.html`.  If the theme contains
  no template, the default one is used:

  ```html
  <!DOCTYPE html>
  <html>
    <head>
      <title>%{title}</title>
      %{css}
    </head>
    <body>
      %{toc}
      %{content}
      %{js}
    </body>
  </html>
  ```
* Meta-information about the theme is placed in `<theme-name>/info.yaml` in
  [YAML](http://en.wikipedia.org/wiki/YAML) format.  The following fields are
  supported, opt-in:

  - `name`: human-readable name of the theme, for displaying purpose only.
  - `authors`: list of authors, separated by a comma (`,`).
  - `license`: the license that theme is released under.
  - `url`: URL of the theme.
  - `demo_url`: demo URL so users can preview the theme.
  - `fetch_command`: command to fetch the theme.
  - `update_command`: command to update the theme.
  - `version`: theme version.

  E.g. here are the meta-information of the `default` theme:

  ```yaml
  name: Github-like
  authors: Duong H. Nguyen (@cmpitg)
  license: GPL v3
  ```

### Creating a theme

To create a theme, simply create the directory structure mentioned above and
put it into `themes/<theme-name>`.  A theme name *should* only contain
alphanumberic characters and/or `_` and/or `-`.  The skeleton of a theme could
be created with the command:

```sh
ulqui theme new <theme-name>
```

### Use a theme

Simple copy all the contents from `themes/<theme-name>/` to `src/`, or use
the built-in command:

```sh
ulqui theme use <theme-name>
```

If the theme name doesn't exist, nothing will change.

## Templating

A template is used to insert the result of the compilation process of your
Markdown documents.

By default:

* If no template is defined, the following basic template is used:

  - `%{title}` will be replaced by the variable `title` in your Markdown
    document(s).

  - `%{css}` will be replaced by the list of CSS files produced in
    `themes/<theme-name>/css/`, *in alphabetical order*.

  - `%{js}` will be replaced by the list of JavaScript files produced in
    `themes/<theme-name>/js/`.

  - `%{toc}` will be replaced by the document's table of contents.

  - `%{content}` will be replaced by HTML produced from your Markdown document(s).

## Command line references

Notes:

* Good convention: Main argument(s) go first, options go later:

  ```sh
  # Do this
  ulqui theme show-info foo-bar --field demo_url

  # Don't do this
  ulqui theme show-info --field demo_url foo-bar
  ```

### General commands

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

### Project management

* To initialize a Ulquikit project:

  ```sh
  ulqui init <project-name>
  ```

  This will create an empty project structure with default theme.  Or you may
  want to initialize the project right inside the current directory:

  ```sh
  ulqui init .
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

### Theme management

* To create a new theme:

  ```sh
  ulqui theme new <theme-name>
  ```

* To add a new theme from a URL (current supported Github):

  ```sh
  ulqui theme add <theme-name>
  ulqui theme add github.com/<user>/<project>
  ```

* To remove a theme:

  ```sh
  ulqui theme remove <theme-name>
  ```

* To use a theme:

  ```sh
  ulqui theme use <theme-name>
  ```

* To save the current theme as a theme stored in your local Ulquikit setting,
  if the theme name has already existed, a prompt will ask if you want to
  override.  You can also force overriding old theme using `--override`.

  ```sh
  # Run interactively
  ulqui theme save

  # Or
  ulqui theme save <theme-name>

  # Force overriding
  ulqui theme save <theme-name> --override
  ```

* To show information of a theme:

  ```sh
  # Interatively ask for a theme name
  ulqui theme show-info

  # Or specify yourself
  ulqui theme show-info <theme-name>
  ```

  The output would look like:

  ```
  Name: foo-bar
  Human-readable name: Foo Bar Github-style
  Authors: John Doe <jdoe@example.com>, Jane Unknown <jane@example.com>
  License: GPL v3
  URL: https://github.com/example/test
  Demo URL: http://example.com/ulquikit-themes/foo-bar/
  Fetch command: git clone https://github.com/example/test.git
  Update command: git pull
  Version: 0.1.0
  Local path: /home/johndoe/.config/ulquikit/themes/foo-bar
  ```

  Or to show only one field about the theme:

  ```sh
  ulqui theme show-info foo-bar --field demo_url
  Demo URL: http://example.com/ulquikit-themes/foo-bar/
  ```
