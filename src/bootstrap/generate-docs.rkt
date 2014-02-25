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
(require "utils-file.rkt")
(require "utils-path.rkt")
(require "utils-html.rkt")

(provide (all-defined-out))

(module+ test
  (require rackunit))

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
;; Return all filenames of all CSS files in `./css/`.
;;
(define (get-css-filenames)
  (~>> (directory-list (string-append (get-bootstrap-dir) "css/"))
    (map path->string)))

;;
;; Return all filenames of all CSS files in `./css/`.
;;
(define (get-js-filenames)
  (~>> (directory-list (string-append (get-bootstrap-dir) "js/"))
    (map path->string)))

;;
;; Return a quasiquoted list as HTML template to be used with
;; `generate-html-from-template`.  This function generate a basic HTML
;; template with all CSS files from `./css` and JS files from `./js`.  CSS and
;; JS are included as relative paths: `css/file-name.css` and
;; `js/file-name.css`.  This template is used to generate HTML for all
;; literate documents.
;;
;; E.g.
;;
;; (generate-html-template "CONTENT" "TABLE-OF-CONTENT" #:title "TITLE")
;; ;; => '(html (head (link (@ (rel "stylesheet")
;; ;;                          (type "text/css")
;; ;;                          (href "css/styles.css")))
;; ;;                 (title "TITLE"))
;; ;;           (body (%verbatim "TABLE-OF-CONTENT")
;; ;;                 (%verbatim "CONTENT")
;; ;;                 (script (@ (src "js/jquery.js")))))
;;
(define (generate-html-template content toc
                                #:title [title ""])
  `(html (head ,@(~>> (get-css-filenames)
                   (map (λ (filename) (html/css (string-append "css/" filename)))))
               (title ,title))
         (body (%verbatim ,toc)
               (%verbatim ,content)
               ,@(~>> (get-js-filenames)
                   (map (λ (filename) (html/js (string-append "js/" filename)))))
               )))

;;
;; Render table of contents from a markdown document.
;;
(define (render-toc literate-doc-content)
  (define-values (vars content) (strip-header-vars literate-doc-content))
  (define temp-file-path (create-temp-file content))
  (with-output-to-string
    (λ ()
      (system (format "~a/render-markdown-toc.rb < ~a"
                      (get-bootstrap-dir)
                      temp-file-path)))))

;;
;; Render a Markdown document and return the generated HTML.
;;
;; Sample usage:
;;
;; (render-doc "# Hello World\n## An h2")
;; ;; => "<h1>Hello World</h1>\n</h2>An h2</h2>"
;;
(define (render-doc literate-doc-content)
  (define-values (vars content) (strip-header-vars literate-doc-content))
  (define temp-file-path (create-temp-file content))
  (with-output-to-string
    (λ ()
      (system (format "~a/render-markdown.rb < ~a"
                      (get-bootstrap-dir)
                      temp-file-path)))))

(define (generate-docs)
  (~>> (directory-list +docs-location+)
    (filter (λ (path)
              (and (file-exists? (get-doc-path (path->string path)))
                   (regexp-match #rx"\\.md$" path))))
    (map (λ (relative-path)
           (define filename        (path->string relative-path))
           (define doc-path        (get-doc-path filename))
           (define output-doc-path (get-output-doc-path filename))

           (displayln (~a "-> Processing " doc-path))
           (define content         (read-file doc-path))

           (displayln "   Generating table of contents...")
           (define toc             (render-toc content))

           (displayln "   Generating main content...")
           (define html            (render-doc content))

           (displayln (~a "   Writing " output-doc-path))
           (display-to-file (generate-html-from-template (generate-html-template html toc))
                            output-doc-path
                            #:mode 'text
                            #:exists 'truncate)))))

(define (main)
  (void (generate-docs)))
