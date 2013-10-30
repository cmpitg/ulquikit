---
project_name: Ulquikit
authors: Duong H. Nguyen <cmpitg AT gmailDOTcom>
title: Ulquikit
short_description: A simple literate programming tool in Ruby
version: 0.0.1

---

# Ulquikit

This is full documentation and source code about Ulquikit.  Ulquikit is
written using Ulquikit itself.  On how to use Ulquikit in your project, see
[Ulquikit guide](guide.html).

## Introduction

Ulquikit started as a collection of Ruby scripts to:

* Generate source code from a
  [Markdown](http://en.wikipedia.org/wiki/Markdown) document

* Build the software using the generated source code

* Build the documentation from Markdown documents

* Build and run tests

Output is one of the following items:

* A single HTML page with styles

* A PDF file

* An ePub file

### Features

* **Fully hackable** source code

* [Convention over configuration](http://en.wikipedia.org/wiki/Convention_over_configuration)

* "Visually pleased"

* Output syntax highlighting

* Convention over configuration

* Generated source code has references to original documentation with the
  formats:

  - `/path/to/docs:line_number`
  - `/path/to/docs:line_number_from:line_number_to`
  - `/path/to/docs:/pattern/`
  - `/path/to/docs:/pattern_from/,/pattern_to/`

  Editors can make use of this pattern to implement code/doc navigation.

### Requirements

* Ruby 1.9+
* Gems:
  - [Rouge](https://github.com/jayferd/rouge), a syntax highlighter for Ruby.
  - [Redcarpet](https://github.com/vmg/redcarpet) for markdown processing.
  - [Nokogiri](http://nokogiri.org/) for HTML/XML processing.

#### Installation command

(TODO)

```sh
gem install -V rouge redcarpet nokogiri
```

### Concepts

* Snippets refer to *code snippets* that are used to structure source code.
  Each snippet has a unique name and either:

  - defines a potion of code, or
  - defines the structure of a code

### Constraints

* Filename contains no space and `:`.

* Snippet name contains no space.

* Snippet names are unique.

* **Note**: if the code might be polluted by the character sequences that are
  used to define snippets, their corresponding regular expressions in config
  should be redefined.

### Configuration

* All configuration is managed by a singleton named `UlquiConfig`:

* All configuration is modifiable directly by changing the singleton or by
  placing a YAML file in the `src/` directory.

  ```ruby
  === config-declaration ===
  class UlquiConfigSingleton
    include Singleton

    -{ config-project-structure             }-
    -{ config-define-snippet-regex          }-
    -{ config-default-template              }-
  end

  UlquiConfig = UlquiConfigSingleton.instance
  ======
  ```

* By default, all regular expressions are defined so that source code part of
  the document looks clean and uncluttered:

  ```ruby
  === config-define-snippet-regex ===
  attr_accessor :snippet_def_regex, :file_def_regex, :snippet_add_regex

  @snippet_def_regex = {
    :begin  => /=== ([^ ]+) ===$/,
    :end    => /======$/
  }

  @file_def_regex = {
    :begin  => /_____ file: ([^ ]+) _____$/,
    :end    => /__________$/
  }

  @snippet_add_regex = /-{ ([^ ]+) }-$/
  ===
  ```

### The Markdown language

#### How it works

* With HTML as Ulquikit's output, it needs a basic template which defines the
  structure of the HTML file.  The template is stored in `src/templates/`
  directory.

* A Markdown processor is used to render the Markdown file as the `<body>` of
  the result.

#### Process

* Firstly, Ulquikit determines which template is used for the final single
  HTML file.  If it finds `templates/main.html`, it would read the content of
  that file as its main template; otherwise, simple template is used
  (`@main_template_content`).

  ```ruby
  === config-default-template ===
  attr_accessor :main_template_file, :main_template_content

  @main_template_file = "main.html"

  @main_template_content = File.try_read_file @main_template_content
  @main_template_content = """<!DOCTYPE html>
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
  """ if @main_template_content == ""
  ======
  ```

* Also, the function to process main template is 

* Firstly, Ulquikit reads all templates in `src/templates/`.

* Then, all Markdown files are read and rendered using Redcarpet.  Variable
  definition part is parsed and all variables are stored in a hash.


### Directory structure

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

* The above structure is completely configurable (what's the point of being
  fully hackable if it's not? (-:).  However, it's strongly advised not to
  change:

  ```ruby
  === config-project-structure ===

  class UlquiConfigSingleton
    attr_accessor :project_structure

    project_structure = {
      :main_dirs => {
        :src         => 'src',
        :build       => 'build',
      }

      :template => {
        :path        => 'src/templates',
        :default     => 'main.html'
        :action      => Producer.method(:read_template_html)
      }

      :images => {
        :src         => 'images',
        :output      => 'img'
      },

      :js => {
        :src         => 'scripts',
        :output      => 'js',
        :action      => Producer.method(:compile_js),
        :file_regex  => /\.js$/
      },

      :css => {
        :src         => 'styles',
        :output      => 'css',
        :action      => Producer.method(:compile_css),
        :file_regex  => /\.css$/
      },

      :md => {
        :src         => './',
        :output      => './',
        :action      => Producer.method(:compile_md),
        :file_regex  => /\.(html|pdf|epub)$/
      }

    }
  ======

  ```

### Emacs supporting

* Multi-major mode

* Snippets

  ```
  -new-frag
  -new-file
  -add-frag
  ```

* Code navigation based on pattern
