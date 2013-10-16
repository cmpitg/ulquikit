---
project_name: Ulquikit
authors: Duong H. Nguyen <cmpitg AT gmailDOTcom>
short_description: A simple literate programming tool in Ruby
version: 0.0.1

---

# Ulquikit

## The literate programming tool

Started as a collection of Ruby scripts used to:

* Generate source code from a
  [Markdown](http://en.wikipedia.org/wiki/Markdown) document

* Build the software using the generated source code

* Build the documentation from Markdown documents

* Build and run tests

Output is HTML, epub or PDF.

### Features

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
  - [pygments.rb](https://github.com/tmm1/pygments.rb), wrapper for
    [Pygments](http://pygments.org/) syntax highlighter.
  - [redcarpet](https://github.com/vmg/redcarpet) for markdown processing.

#### Installation command

```sh
gem install -V pygments redcarpet
```

### Constraints

* Filename contains no space and `:`.
* Fragment name contains no space.
* Fragment names are unique.

### The Markdown language

The tool uses Redcarpet as its Markdown processor and Erb as its eRuby
processor.

* Erb is called first to transform all placeholders into actual values,
  producing a pure Markdown file.

* Finally, Redcarpet is called to turn the Markdown file into one of the
  supported outputs with the following options:
  - All headings will have name anchors.
  - A table of contents is built.
  - Using **CSS** from `src/styles/`, opt-in.
  - Using **JavaScript** from `src/scripts/`, opt-in.

### Directory structure

```
project/
  src/
    images/
    scripts/
    styles/
    main.md

  build/
    img/
    js/
    css/

  README.md
  ...
```

Some special directories to note:

* The `src/` directory:
  - `scripts/` includes JavaScript scripts to be compiled to JavaScript to use
    in the HTML output of the document.
  - `styles/` includes CSS or files to be compiled to CSS to use in the HTML
    output of the document.

* `build/` contains the result of the build process by Ulquikit:
  - The directory itself contains HTML and/or PDF output.
  - `img/` is a copy of `images/`.
  - `js/` contains all JavaScript from or generated from `src/scripts/`.
  - `css/` contains all CSS from or generated from `src/styles/`.

* The above structure is completely configurable (what's the point of being
  fully hackable if it's not? (-:).  However, it's strongly advised not to
  change:

  ```ruby
  === config-project-structure ===

  class ConfigSingleton
    include Singleton

    attr_accessor :project_structure

    project_structure = {
      :main_dir => {
        :src         => 'src',
        :build       => 'build',
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

### Process

* Erb
* Markdown

### Emacs supporting

* Multi-major mode

* Snippets

  ```
  -new-frag
  -new-file
  -add-frag
  ```

* Code navigation based on pattern

### Fragment definition

```ruby
FragmentDefinition = {
    :begin  => /=== ([^ ]+) ===$/,
    :end    => /======$/
}
```

### File definition

```ruby
FileDefinition = {
    :begin  => /_____ file: ([^ ]+) _____$/,
    :end    => /__________$/
}
```

### Adding fragment

```ruby
AddingFragmentRegex = /<<< ([^ ]+) <<<$/
```
