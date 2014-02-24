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

(provide main)

(require srfi/1)
(require "utils-file.rkt")
(require "utils-path.rkt")

(module+ test
  (require rackunit))

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
;; Generate doc file from its literate source into its appropriate path.
;;
(define (generate-doc literate-doc-file)
  (define-values (vars content)
    (strip-header-vars (read-file (get-doc-path literate-doc-file))))
  (displayln (~a "-> Generating " (get-output-doc-path literate-doc-file)))
  (define temp-file-path (create-temp-file content))
  (void (system (format "~a/render-markdown.rb < ~a > ~a"
                        (get-bootstrap-dir)
                        temp-file-path
                        (get-output-doc-path literate-doc-file)))))

(define (generate-docs)
  (~>> (directory-list +docs-location+)
    (filter (λ (path)
              (and (file-exists? (get-doc-path (path->string path)))
                   (regexp-match #rx"\\.md$" path))))
    (map (λ (path) (~> (path->string path)
                     generate-doc)))))

(define (main)
  (void (generate-docs)))
