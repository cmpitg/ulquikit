* Add Twitter Bootstrap theme

* Complete bootstrap part:
  - Generate fragments and store them
  - Recognize filenames
  - Generate files

* Support different theme set

* Better code example @ `README.md`

* Support multiple docs

* Should multiple-HTML output be supported?

* Rakefile: specify file prerequisites for `gen` tasks
  - Avoid regenerating generated files

* Rakefile: use directory task

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
