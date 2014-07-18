## Emacs Lisp ##

* [x] Write file with just this:

  ```racket
  ;; %%file: /tmp/base.rkt

  (displayln "Hello World")
  ```

  Supported patterns:
  - `;; /path/to/file`
  - `;; %%path /path/to/file`
  - `;; %write-to-file /path/to/file`

## Racket ##

* [ ] Define convention for simple project structure.

* [ ] Use basic Twitter Bootstrap theme with Github style.

* [ ] Read custom variable at the beginning of a document.

* [ ] Render document using Ruby's Redcarpet library.

* [ ] Write wrapper to call external process and wait for it to finish.

* [ ] Extract code snippet and store them in a hash.

* [ ] Extract file snippet, store them in a hash, and them to their
  corresponding files.

* [ ] Extract line number of code snippet from the docs. (`;; ../relative/path/`).

* [ ] Insert code snippet into their appropriate place (`-{ snippet-name }-`).

* [ ] Command line argument passing.

* [ ] Strip header which contains variables from literate documents.

* [ ] Terminal colors

* [ ] Support for Unicode characters in snippet name.

* [ ] Option to exclude `--> ... <---` and `___ ... ___` not as snippet names.

* [ ] Customizing snippet regexp.

* [ ] Customizing locations to generate code and docs (rationale: supporting
  projects that use their root directories for other purposes).

* [ ] Supporting other markup languages like reStructuredText and ASCIIDoc.

* [ ] Think of a good way to exclude snippet. **[critical]**

* [x] Write Ruby script to render Markdown.

* [x] Generate resources by copying from `./src/bootstrap/css` and
  `./src/bootstrap/js` to `./generate-docs/`:
  `./src/bootstrap/generate-resources.rkt`

### Dependencies ###

```racket
#lang racket

(require rackjure)
(require terminal-color)
(require (planet neil/html-template:2:2))
```

### Questions? ###

* How to support multiple HTML theme?

* Circular snippet dependency:
  - Snippet `A` includes `B`
  - And `B` includes `A`

### Optimization ###

* Using mutable hashes instead of immutable hashes?
* Only re-generate newer versions

---

## Old and outdated ##

* [x] Add Twitter Bootstrap theme

* [x] Assets are added using relative paths

* Complete bootstrap part:
  - [x] Generate fragments and store them: 
  - Recognize filenames
  - Generate files

* HTML output:
  - Better layout with Twitter Bootstrap
  - Each snippet has its own style

* Support different theme set

* Better code example @ `README.md`

* Support multiple docs:
  - There should be some convention to refer to snippets in another doc:
    * `doc-filename:snippet-name`?
    * Or snippets are global?
      - What about combining several projects into one?
        * Though unlikely to happen, it's possible
        * It's best to develop good convention from the beginning
          - Let's not think about it now, we're still bootstraping

* Should multiple-HTML output be supported? Yes!

* Rakefile: specify file prerequisites for `gen` tasks
  - Avoid regenerating generated files

* Rakefile: use directory task

* Directory structure?
  - Theme organization:
    * Install new theme
    * Using a theme
    * Modify a theme
    * **Important**: A theme should be **there** (aka. copied into project dir)

* Should one-line snippet is supported? (still thinking)
  - Case: conditional control flow

    ```elisp
    (if -{ the-number-is-even }-
        (message "Found it")
      -{ proceed-further-when-odd }-)
    ```
  - No.  In this case, the snippet should be a function call, so it is a
    semantic unit itself.

  - Yes.  Because of performance reason.

  - No.  If that the case, the whole statement/condition/whatsoever should be
    a snippet in its own right.
