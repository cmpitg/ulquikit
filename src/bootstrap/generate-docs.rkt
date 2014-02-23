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

#lang rackjure

(require srfi/1)
(require racket/runtime-path)
(require "utils-path.rkt")

(define-runtime-path +current-dir+ ".")

(module+ test
  (require rackunit))

(define (this-dir)
  (expand-path +current-dir+))

(define +doc-location+ (expand-path (string-append (this-dir) "/../")))

;;
;; Return full path to a literate document of Ulquikit
;;
(define (get-doc-path file)
  (expand-path (string-append +doc-location+ file)))

;;
;; Return file content as string
;;
(define (read-file path)
  (file->string path #:mode 'text))

;;
;; Strip header which contains variables in a content, delimitered by 2 `---`
;; lines.  Return values of var part and content part, respectively.  Var part
;; is a list of lines, content part is string.
;;
;; E.g.
;;
;; (define content "---\nfirst-value: 10\nsecond-value: 'hello-world\n---\n# Main content")
;; (define-values (var-list main-content) (strip-header-vars content))
;; (displayln var-list)
;; ;; => '("first-value: 10" "second-value: 'hello-world")
;; (displayln main-content)
;; ;; => "# Main content"
;;
(define (strip-header-vars text)
  (let ([lines (string-split text "\n")])
    (cond [(not (string=? "---" (first lines))) ; First line is not `---` =>
                                        ; no vars
           (values "" text)]

          [else
           (define var-part (take-while (λ (ele)
                                          (not (string=? "---" ele)))
                                        (rest lines)))
           (define content-part (drop-while (λ (ele)
                                              (not (string=? "---" ele)))
                                            (rest lines)))
           (values var-part (string-join (rest content-part) "\n"))])))

(module+ test
  (local [(define content  "---\nHellow world\nWorld hello\n---\naoeuaoeu\naoeuaoeu")
          (define-values (var-part content-part) (strip-header-vars content))]
    (check-equal? var-part      '("Hellow world" "World hello"))
    (check-equal? content-part  "aoeuaoeu\naoeuaoeu"))

  (local [(define content "---\nfirst-value: 10\nsecond-value: 'hello-world\n---\n# Main content")
          (define-values (var-list main-content) (strip-header-vars content))]
    (check-equal? var-list      '("first-value: 10" "second-value: 'hello-world"))
    (check-equal? main-content  "# Main content")))

;;
;; Create a temporary file with `content` as its content and return the file
;; path.  Overwrite existing file.
;;
(define (create-temp-file content)
  (define path (make-temporary-file))
  (display-to-file content path
                   #:mode 'text
                   #:exists 'update)
  path)

(define (main)
  (define-values (vars content)
    (strip-header-vars (read-file (get-doc-path "internals.md"))))
  (define temp-file-path (create-temp-file content))
  (displayln (~a "-> Generating " (get-doc-path "../generated-docs/internal.html")))
  (void (system (format "./render-markdown.rb < ~a > ~a"
                        temp-file-path
                        (get-doc-path "../generated-docs/internal.html")))))

(main)
