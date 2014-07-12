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
;; Rules of thumb:
;; * Global vars are not accessed directly but through helpers
;; * Global vars which don't have global lifespan must be parameterize'd
;;

#lang rackjure

(require racket/path)
(require racket/splicing)

(require racket/runtime-path)
(define-runtime-path +this-directory+ ".")

(module+ test
  (require rackunit))

(define *code-blocks* (make-parameter {}))

(define get-relative-path #λ(apply build-path %&))

(define read-file #λ(call-with-input-file % port->string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; process-string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; All functions in this section must be called within the effect of
;; process-string.  Otherwise, unexpected behaviors will occur.

(define _string_   (make-parameter ""))
(define _position_ (make-parameter 0))

(define-syntax-rule (process-string str body ...)
  (parameterize ([_string_    str]
                 [_position_  0])
    body ...))

(define (get-string)
  (_string_))

(define (get-position)
  (_position_))

(define (set-position new-position)
  (_position_ new-position))

(module+ test
  (process-string "hello world"
    (check-equal? (get-string)   "hello world")
    (check-equal? (get-position) 0)

    (set-position                10)
    (check-equal? (get-position) 10)))

(define (look-for str)
  (let ([pattern (regexp-quote str)])
    (regexp-match? pattern (_string_) (+ 1 (_position_)))))

(module+ test
  (process-string "hello world\n[source\ncode[source"
    (check-equal? (look-for "[source") #t)

    (set-position 15)
    (check-equal? (look-for "[source") #t)

    (set-position 29)
    (check-equal? (look-for "[source") #f))

  (process-string "hello world\n[source\ncode[source"
    (check-equal? (look-for "h") #f)))

(define (goto-next str)
  (let ([pattern (regexp-quote str)])
    (set-position (if (look-for str)
                      (~> (regexp-match-positions pattern
                                                  (_string_)
                                                  (+ 1 (_position_)))
                        first
                        car)
                      (~> (_string_)
                        string-length
                        (- 1))))))

(module+ test
  (process-string "hello world\n[source\ncode[source"
    (goto-next "h")
    (check-equal? (get-position) 30))

  (process-string "hello world\n[source\ncode[source"
    (goto-next "e")
    (check-equal? (get-position) 1)

    (goto-next "world")
    (check-equal? (get-position) 6)))

(define (next-line)
  (goto-next "\n")
  (when (< (+ 1 (get-position))
           (string-length (get-string)))
    (set-position (+ 1 (get-position)))))

(module+ test
  (process-string "hello world\n[source\ncode[source"
    (next-line)
    (check-equal? (get-position) 12)
    (next-line)
    (check-equal? (get-position) 20)
    (next-line)
    (check-equal? (get-position) (- (string-length (get-string)) 1))))

(define (goto-backward str)
  (let* ([length (string-length (get-string))]
         [current-position (get-position)])
    (set-position (- length (process-string (~> (get-string)
                                              string->list
                                              reverse
                                              list->string)
                              (set-position (- length current-position 1))
                              (goto-next str)
                              (get-position))
                     1))))

(module+ test
  (process-string "ab"
    (set-position 1)
    (goto-backward "a")
    (check-equal? (get-position) 0))
  
  (process-string "hello world\n[source\ncode[source"
    ;; End of string
    (set-position (- (string-length (get-string)) 1))

    (goto-backward "c")
    (check-equal? (get-position) 29)

    (goto-backward "[")
    (check-equal? (get-position) 24)

    (goto-backward "[")
    (check-equal? (get-position) 12)

    (goto-backward "-")
    (check-equal? (get-position) 0)))

(define (to-beginning-of-line)
  (goto-backward "\n")
  (when (equal? (~> (get-string)
                  (string-ref (get-position)))
                #\newline)
    (set-position (+ 1 (get-position)))))

(module+ test
  (process-string "hello world\n[source\ncode[source"
    (to-beginning-of-line)
    (check-equal? (get-position) 0)

    (next-line)
    (set-position (+ (get-position) 3))
    (to-beginning-of-line)
    (check-equal? (get-position) 12)

    (next-line)
    (set-position (+ (get-position) 3))
    (to-beginning-of-line)
    (check-equal? (get-position) 20)))

(define (to-end-of-line)
  (goto-next "\n")
  (when (equal? (~> (get-string)
                  (string-ref (get-position)))
                #\newline)
    (set-position (- (get-position) 1))))

(module+ test
  (process-string "hello world\n[source\ncode[source"
    (to-end-of-line)
    (check-equal? (get-position) 10)

    (next-line)
    (to-end-of-line)
    (check-equal? (get-position) 18)

    (next-line)
    (to-end-of-line)
    (check-equal? (get-position) 30)

    (next-line)
    (to-end-of-line)
    (check-equal? (get-position) 30)))

;; (define (get-line)
;;   (let* ([current-position (get-position)]
;;          [line-start       (begin
;;                              (to-beginning-of-line)
;;                              (get-position))]
;;          [line-end         (begin
;;                              (to-end-of-line)
;;                              (get-position))]
;;          [line             (substring (get-string line-start line-end))])
;;     (set-position current-position)
;;     line))

;; (module+ test
;;   (process-string "hello world\n[source\ncode[source"
;;     (check-equal? (get-line) "hello world")

;;     (next-line)
;;     (check-equal? (get-line) "[source")

;;     (next-line)
;;     (check-equal? (get-line) "code[source")

;;     (next-line)
;;     (check-equal? (get-line) "code[source")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (module+ main
;;   (let* ([source-file (get-relative-path +this-directory+ "../Main.adoc")]
;;          [content     (read-file source-file)])
;;     (process-string content
;;       ;; Continuously look for "[source" block and extract code blocks
;;       (let search-and-update-blocks
;;           []
;;         (when (look-for "[source")
;;           (goto-next "[source")
;;           (next-line)

;;           (unless (code-block-begins? (get-line))
;;             (let* ([title   (trim (get-line))]
;;                    [name    (get-code-block-name title)]
;;                    [type    (get-code-block-type title)]

;;                    ;; Positions
;;                    [start    (begin
;;                                (next-line)
;;                                (next-line)
;;                                (to-beginning-of-line)
;;                                (get-position))]
;;                    [end      (begin
;;                                (goto-next "----")
;;                                (prev-line)
;;                                (to-end-of-line)
;;                                (get-position))]

;;                    [content  (get-substring #:from start
;;                                             #:to   end)])
;;               (update-blocks #:content content
;;                              #:name    name
;;                              #:type    type))))

;;         (if (look-for "[source")
;;             (search-and-update-blocks)
;;             (get-blocks))))))
