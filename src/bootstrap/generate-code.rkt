;;
;; This file is part of Ulquikit project.
;;
;; Copyright (C) 2014 Duong H. Nguyen <cmpitg AT gmailDOTcom>
;;
;; Ulquikit is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; Ulquikit is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; Ulquikit.  If not, see <http://www.gnu.org/licenses/>.
;;

;;;
;;; Generate source code for Ulquikit from snippets residing at ./src/
;;;

#lang rackjure

(require racket/pretty)
(require srfi/1)
(require "utils-file.rkt")
(require "utils-path.rkt")
(require "utils-html.rkt")
(require "utils-alist.rkt")

(provide (all-defined-out))

(current-curly-dict hash)

(define +code-snippet-regexp+   #rx"^( *)--> ([a-zA-Z0-9_/.-]+) <-- *$")
(define +file-snippet-regexp+   #rx"^( *)___ ([a-zA-Z0-9_/.-]+) ___ *$")
(define +end-of-snippet-regexp+ #rx"^ *``` *$")
(define +comment-syntax+        ";;")
(define +include-regexp+        #rx"^( *)-{ +([a-zA-Z0-9_/.-]+) +}- *$")

(module+ test
  (require rackunit))

;;
;; Extract code snippet from a literate document in +docs-location+ and merge
;; it into a hash.  Return the merged hash.
;;
;; Each snippet is a hash with the following keys:
;; * `type`: type of snippet, either `code` or `file`
;; * `content`: content of the snippet
;; * `literate-path`: full path to the literate source file that defines the
;;   snippet
;; * `line-number`: the line number at which the snippet is defined in
;;   `literate-path`
;;
(define (extract-code-snippet-from-file filename snippets-hash)
  (local [(define doc-content (read-file filename))

          (define (extract-snippet snippet-regexp
                                   #:line line
                                   #:line-number line-number
                                   #:type type)
            (let* ([matches       (regexp-match snippet-regexp line)]
                   [indent-length (string-length (list-ref matches 1))]
                   [snippet-name  (list-ref matches 2)])

              ;; Add snippet to the hash of snippets
              (hash-ref! snippets-hash
                         snippet-name
                         {'type        type
                          'content     #f
                          'literate-path filename
                          'line-number line-number})

              ;; Return new snippet info
              {'current-snippet-name snippet-name
               'inside-snippet       #t
               'indent-length        indent-length}))

          (define (close-snippet)
            {'inside-snippet       #f
             'current-snippet-name ""
             'indent-length        0})

          (define (update-current-snippet snippet-info
                                          #:line line)
            (let* ([snippet-name  (snippet-info 'current-snippet-name)]
                   [indent-length (snippet-info 'indent-length)]
                   [code-line     (if (>= (string-length line) indent-length)
                                      (substring line indent-length)
                                      line)])
              (hash-update! snippets-hash
                            snippet-name
                            (λ (snippet)
                              (let* ([current-content (snippet 'content)]
                                     [new-content (if current-content
                                                      (string-append current-content
                                                                     "\n"
                                                                     code-line)
                                                      code-line)])
                                (snippet 'content new-content))))))]

    (~>> (string-split doc-content "\n")
      (foldl (λ (line snippet-info)
               (let ([old-line-number (snippet-info 'line-number)]
                     [snippet-info
                      (cond [ ;; Begining of code snippet
                             (regexp-match? +code-snippet-regexp+ line)
                             (extract-snippet +code-snippet-regexp+
                                              #:line line
                                              #:line-number (snippet-info 'line-number)
                                              #:type 'code)]

                            [ ;; Begining of file snippet
                             (regexp-match? +file-snippet-regexp+ line)
                             (extract-snippet +file-snippet-regexp+
                                              #:line line
                                              #:line-number (snippet-info 'line-number)
                                              #:type 'file)]

                            [ ;; End of snippet
                             (regexp-match? +end-of-snippet-regexp+ line)
                             (close-snippet)]

                            [else
                             (when (hash-ref snippet-info 'inside-snippet)
                               (update-current-snippet snippet-info
                                                       #:line line))
                             snippet-info])])
                 (snippet-info 'line-number (add1 old-line-number))))
             {'line-number          0   ; Should be counted from 1, as we when
                                        ; we generate reference back to
                                        ; literate doc, the Markdown code
                                        ; block fence should be counted.  The
                                        ; beginning of the code block is the
                                        ; previous line of the line containing
                                        ; snippet name
              'inside-snippet       #f
              'current-snippet-name ""
              'indent-length        0})))
  snippets-hash)

;;
;; Determine if a snippet is a file snippet.
;;
(define (is-file-snippet? snippet)
  (eq? 'file (snippet 'type)))

;;
;; This functions takes the hash that contains all snippets, include all code
;; snippet into their appropriate places in file snippets, and return the hash
;; of file snippets afterward.
;;
(define (include-code-snippets snippets-hash)
  (local [(define (indent-code code indentation)
            (string-join (~>> (string-split code "\n")
                           (map (λ (line) (string-append indentation line))))
                         "\n"))

          (define (get-snippet-indentation line)
            (let* ([matches       (regexp-match +include-regexp+ line)]
                   [indentation   (if matches
                                      (list-ref matches 1)
                                      "")])
              indentation))

          (define (get-included-snippet-name text)
            (if (contains-include-instruction? text)
                (list-ref (regexp-match +include-regexp+ text) 2)
                ""))

          (define (replace-line-with-snippet line)
            (let* ([indentation   (get-snippet-indentation line)]
                   [snippet-name  (get-included-snippet-name line)])
              (displayln (~a "-> Replacing " line))
              (indent-code (if (snippets-hash snippet-name)
                               ((snippets-hash snippet-name) 'content)
                               "{{ No snippet defined }}")
                           indentation)))

          (define (get-ref-to-literate-doc #:source-path   source-path
                                           #:literate-path literate-path
                                           #:line-number   line-number
                                           #:indentation   indentation)
            (string-append indentation
                           +comment-syntax+
                           " "
                           (~> (find-relative-path (expand-path literate-path)
                                                   (expand-path source-path))
                             path->string
                             (string-append "\n"))))

          (define (contains-include-instruction? text)
            (regexp-match? +include-regexp+ text))

          ;;
          ;; Find all lines that match +include-regexp+ and replace them with
          ;; the appropriate snippet.
          ;;
          ;; If the result snippet still contains other include instructions,
          ;; process with recursion.
          ;;
          ;; This function returns the content of the snippet after all
          ;; replacements.
          ;;
          ;; `current-file` is used to calculate the relative path of the
          ;; generated code that would refer back to its literate doc.
          ;;
          ;; Note that one-line snippets are not supported, so a line that has
          ;; multiple include instructions is not supported.
          ;;
          (define (process-snippet snippet
                                   #:source-path   source-path
                                   #:literate-path literate-path)
            (let* ([lines (~> (snippet 'content)
                            (string-split "\n"))]
                   [content 
                    (~> (map (λ (line)
                               (let ([processed-line
                                      (if (regexp-match? +include-regexp+ line)
                                          (~>> (replace-line-with-snippet line snippet)
                                            (string-append (get-ref-to-literate-doc
                                                            #:source-path   source-path
                                                            #:literate-path literate-path
                                                            #:line-number   (snippet 'line-number)
                                                            #:indentation   (get-snippet-indentation line))))
                                          line)])
                                 ;; (when (contains-include-instruction? processed-line))
                                 processed-line))
                             lines)
                      (string-join "\n"))])
              content))]
    (hash-map snippets-hash
              (λ (snippet-name snippet)
                (process-snippet snippet
                                 #:source-path "/tmp/file"
                                 #:literate-path (snippet 'literate-path))))))

(define (generate-code)
  (~>> (list-doc-filenames)
    (map (λ (filename) (get-doc-path filename)))
    (foldl extract-code-snippet-from-file (make-hash))

    include-code-snippets
    (map (λ (str)
           (displayln "---")
           (displayln str)))

    ;; hash->list
    ;; (map (λ (pair)
    ;;        (pretty-display "---")
    ;;        (pretty-display (~a "Name: |" (car pair) "|"))
    ;;        (pretty-display (~a "Type: " ((cdr pair) 'type)))
    ;;        (pretty-display (~a "Path: " ((cdr pair) 'literate-path) ":" ((cdr pair) 'line-number)))
    ;;        (pretty-display ((cdr pair) 'content))
    ;;        (newline)))
    ))

(define (main)
  (void (generate-code)))
