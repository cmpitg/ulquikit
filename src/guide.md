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

* Defining snippets:

  - *Code snippet*:

    ```language
    --> snippet-global-unique-name <--
    Snippet content
    ```

  - *File snippet*:

    ```
    ___ /relative/path/to/file ___
    ```

  - Including *code snippets*:

    ```language
    -{ snippet-global-unique-name }-
    ```

  - The syntax used to denote and include snippets is customizable.

### Design decisions ###

* References from source code files in *generated source* to *original
  documents* are relative paths to prevent path conflict between different
  machines.

* *Code snippets* must have *project-wide* unique names, since a snippet could
  be added from different *document*.
