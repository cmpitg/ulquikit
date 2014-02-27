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
;;; In functional programming, data structure should all be immutable.
;;; However, some operations in this module are performed upon mutable data
;;; structure.  This is for only performance reason.
;;;

#lang rackjure

(require racket/pretty)
(require srfi/1)
(require "utils-file.rkt")
(require "utils-path.rkt")
(require "utils-html.rkt")
(require "utils-alist.rkt")

(provide (all-defined-out))

(current-curly-dict hasheq)

(define +code-snippet-regexp+ #rx"^( *)--> ([a-zA-Z0-9_/ -]+) <-- *$")
(define +file-snippet-regexp+ #rx"^( *)=== ([a-zA-Z0-9_/ -]+) === *$")
(define +end-of-snippet-regexp+ #rx"^ *``` *$")
(define +comment-syntax+ ";;")

(module+ test
  (require rackunit))

;;
;; Extract code snippet from a literate document in +docs-location+ and merge
;; it into a hash.  Return the merged hash.
;;
;; Each snippet is a hash with the following keys:
;; * `type`: type of snippet, either `code` or `file`
;; * `content`: content of the snippet
;; * `source-file`: full path to the literate source file that defines the
;;   snippet
;; * `source-line`: the line number at which the snippet is defined in
;;   `source-file`
;;
(define (extract-code-snippet-from-file filename snippets)
  (local [(define doc-content (read-file filename))
          (define (extract-snippet snippet-info snippet-regexp line type)
            (let* ([matches       (regexp-match snippet-regexp line)]
                   [indent-length (string-length (list-ref matches 1))]
                   [snippet-name  (list-ref matches 2)])
              (hash-set! snippet-info 'inside-snippet       #t)
              (hash-set! snippet-info 'current-snippet-name snippet-name)
              (hash-set! snippet-info 'indent-length        indent-length)

              (hash-ref! snippets
                         snippet-name
                         {'type        type
                          'content     ""
                          'source-file filename
                          'source-line (hash-ref snippet-info 'line-number)})))]
    (~>> (string-split doc-content "\n")
      (foldl (λ (line snippet-info)
               (cond [ ;; Begining of code snippet
                      (regexp-match +code-snippet-regexp+ line)
                      (extract-snippet snippet-info +code-snippet-regexp+ line 'code)
                      ]

                     [ ;; Begining of file snippet
                      (regexp-match +file-snippet-regexp+ line)
                      (extract-snippet snippet-info +file-snippet-regexp+ line 'file)]

                     [ ;; End of snippet
                      (regexp-match +end-of-snippet-regexp+ line)
                      (hash-set! snippet-info 'inside-snippet       #f)
                      (hash-set! snippet-info 'current-snippet-name "")
                      (hash-set! snippet-info 'indent-length        0)]

                     [else
                      (when (hash-ref snippet-info 'inside-snippet)
                        (let* ([snippet-name  (hash-ref snippet-info 'current-snippet-name)]
                               [indent-length (hash-ref snippet-info 'indent-length)]
                               [code-line     (if (>= (string-length line) indent-length)
                                                  (substring line indent-length)
                                                  line)])
                          (hash-update! snippets
                                        snippet-name
                                        (λ (snippet)
                                          (let ([new-content (string-append (snippet 'content)
                                                                            "\n"
                                                                            code-line)])
                                            (snippet 'content new-content))))))])

               ;; Next loop
               (hash-update! snippet-info 'line-number add1)
               snippet-info)
             (make-hasheq `((line-number          . 1)
                            (inside-snippet       . #f)
                            (current-snippet-name . "")
                            (indent-length        . 0))))))
  snippets)

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
(define (include-code-snippets-into-file-snippets snippets)
  (define file-snippets (make-hasheq))
  (hash-map snippets
            (λ (snippet)
              (when (is-file-snippet? snippet)
                #t))))

(define (generate-code)
  (~>> (list-doc-filenames)
    (map (λ (filename) (get-doc-path filename)))
    (foldl extract-code-snippet-from-file (make-hasheq))
    ;; include-code-snippets-into-file-snippets

    hash->list
    (map (λ (pair)
           (pretty-display "---")
           (pretty-display (~a "Name: " (car pair)))
           (pretty-display (~a "Type: "((cdr pair) 'type)))
           (pretty-display (~a "Path: " ((cdr pair) 'source-file) ":" ((cdr pair) 'source-line)))
           (pretty-display ((cdr pair) 'content))
           (newline)))))

(define (main)
  (void (generate-code)))
