#!/usr/bin/env racket

;;
;; This file is part of Ulquikit project.
;;
;; Copyright (C) 2014 Nguyễn Hà Dương <cmpitg AT gmailDOTcom>
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
;; You should have received a copy of the GNU General Public License along
;; with Ulquikit.  If not, see <http://www.gnu.org/licenses/>.
;;

;;
;; I know I know.  This file contains lots of bad practices.  However, for the
;; purpose of keeping the code clean, simple, and human-readable, I implement
;; it that way.  Drop me a line if you have better idea that could help.
;;

;;
;; This program is supposed to be really simple, so it doesn't handle circular
;; dependency problem of including code block.
;;

;;
;; Rules of thumb:
;; * Global vars are not accessed directly but through helpers
;; * Global vars which don't have global lifespan must be parameterize'd
;;

#lang rackjure

;; Using hashtable with curly-dict notation
(current-curly-dict hash)

(require racket/path)
(require racket/pretty)

(require srfi/1)

(require racket/runtime-path)
(define-runtime-path +this-directory+ ".")

(require "utils.rkt")
(require "utils-string.rkt")
(require "utils-path.rkt")

(module+ test
  (require rackunit))

(define *code-blocks* (make-parameter {}))
(define *file-blocks* (make-parameter {}))

(define read-file #λ(call-with-input-file % port->string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-code-block-name title)
  (list-ref (string-split title "::") 1))

(define (get-code-block-type title)
  (string->symbol (list-ref (string-split title "::") 0)))

(define (get-indentation str)
  (~>> (string->list str)
    (take-while #λ(equal? #\space %))
    length))

(module+ test
  (check-equal? (get-indentation "a") 0)
  (check-equal? (get-indentation " a") 1)
  (check-equal? (get-indentation "  a") 2)
  (check-equal? (get-indentation "   a") 3)
  (check-equal? (get-indentation "    a") 4))

(define (update-block #:content content
                      #:name    name
                      #:type    type
                      #:indentation indentation)
  (let* ([block {'type type
                 'name name
                 'content content
                 'indentation indentation}])
    (if (eq? type 'file)
        (*file-blocks* ((*file-blocks*) name block))
        (*code-blocks* ((*code-blocks*) name block)))))

(define (code-block-begins? str)
  (regexp-match? #rx"----" str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (display-code-block block)
  (displayln (~a "[[ " (block 'name) " -> " (block 'indentation) " ]]"))
  (displayln "-->")
  (displayln (block 'content))
  (displayln "<--")
  (newline))

(define (display-code-blocks)
  (hash-map (*code-blocks*) #λ(display-code-block %2)))

(define (display-file-block block)
  (displayln (~a "<< " (block 'name) " >>"))
  (displayln "-->")
  (displayln (block 'content))
  (displayln "<--")
  (newline))

(define (display-file-blocks)
  (hash-map (*file-blocks*) #λ(display-file-block %2)))

(define (extract-blocks)
  (for ([source-file (~> (get-relative-path +this-directory+ "../")
                       get-all-adocs)])
    (let* ([content (string-append "\n" ; Guard, make sure blocks start at
                                        ; least from line #2
                                   (read-file source-file))])
      (process-string content
        ;; Continuously look for "[source" block and extract code blocks
        (let search-and-update-block
            []
          (when (look-for "[source")
            (goto-next "[source")
            (next-line)

            (unless (code-block-begins? (get-line))
              (let* ([title       (trim (get-line))]
                     [name        (get-code-block-name title)]
                     [type        (get-code-block-type title)]
                     [indentation (get-indentation (get-line))]

                     ;; Positions
                     [start    (begin
                                 (next-line)
                                 (next-line)
                                 (to-beginning-of-line)
                                 (get-position))]
                     [end      (begin
                                 (goto-next "----")
                                 (prev-line)
                                 (to-end-of-line)
                                 (get-position))]

                     [content  (get-substring #:from start
                                              #:to   (inc end))])
                (update-block #:content content
                               #:name    name
                               #:type    type
                               #:indentation indentation))))

          (when (look-for "[source")
            (search-and-update-block)))))))

(define is-include-directive? (partial regexp-match? "include::"))

(define (get-block-content #:type type
                           #:name name)
  (let* ([blocks (if (eq? type 'code)
                     (*code-blocks*)
                     (*file-blocks*))])
    (~> blocks name 'content)))

(define get-included-block-name
  #λ(~> (trim %) (string-split "include::") second))

(define (include-code-block block)
  ;; (displayln (~a "Called with: " (block 'name)))
  ;; (displayln (~a "--> " block))
  (let* ([content (block 'content)]
         [ind     (block 'indentation)]
         [name    (block 'name)]
         [type    (block 'type)]
         [lines   (string-split content "\n")]
         [new-content (~> (for/list ([line lines])
                            (if (is-include-directive? line)
                                (let* ([included-block-name (get-included-block-name line)])
                                  (include-code-block ((*code-blocks*) included-block-name))
                                  (get-block-content #:type type
                                                     #:name included-block-name))
                                line))
                        (string-join "\n"))])
    (update-block #:content     new-content
                  #:name        name
                  #:type        type
                  #:indentation ind)))

(define (include-code-blocks)
  (hash-for-each (*code-blocks*) #λ(include-code-block %2)))

(define (include-file-blocks)
  (hash-for-each (*file-blocks*)
                 (λ (name block)
                   (let* ([content (block 'content)]
                          [ind     (block 'indentation)]
                          [lines   (string-split content "\n")]
                          [new-content (~> (for/list ([line lines])
                                             (if (is-include-directive? line)
                                                 (let* ([included-block-name (get-included-block-name line)])
                                                   (((*code-blocks*) included-block-name) 'content))
                                                 line))
                                         (string-join "\n"))])
                     (update-block #:content     new-content
                                   #:name        name
                                   #:type        'file
                                   #:indentation ind)))))

(module+ main
  (void (extract-blocks)
        (include-code-blocks)
        (include-file-blocks)

        (display-code-blocks)
        ;; (display-file-blocks)
        ))
