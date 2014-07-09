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

(require (planet neil/html-template:2:2))

(provide generate-html
         generate-html-from-template
         (rename-out [js html/js]
                     [css html/css]))

(module+ test
  (require rackunit))

;;
;; Return a generated HTML as string using Neil Van Dyke's HTML template
;; library.  This is just a convenient macro.
;;
;; E.g.
;;
;; (define some-var 1202020)
;; (displayln (generate-html
;;             (html (head (h1 "Hello World"))
;;                   (body (@ (bgcolor "#FAFAFA"))
;;                         (div (@ (id "row")
;;                                 (class "bold-text"))
;;                              "Div content")))
;;             (div "OMG!")
;;             (div (%format "~a" some-var))))
;;
(define-syntax-rule (generate-html args ...)
  (with-output-to-string
   (λ () (html-template args ...))))

(module+ test
  (define some-var 1202020)
  (displayln (generate-html
              (html (head (h1 "Hello World"))
                    (body (@ (bgcolor "#FAFAFA"))
                          (div (@ (id "row")
                                  (class "bold-text"))
                               "Div content")))
              (div "OMG!")
              (div (%format "~a" some-var)))))

(define (generate-html-from-template . args)
  (generate-html (%verbatim args)))

(module+ test
  (let* ([first-template `(html (head ,(css "src/first-file.css")
                                      ,(css "src/second-file.css"))
                                (body (h1 "It's the beginning")
                                      (h2 "Say something buddy!")
                                      ,(js "src/first-file.js")
                                      ,(js "src/second-file.js")))]

         [list-of-js-files '("src/one-place.js"
                             "src-original/second-place.js"
                             "the-third-file.js")]
         [second-template `(html (body (h1 "It's the beginning")
                                       (h2 "Say something buddy!")
                                       ,@(map (λ (js-file) (js js-file))
                                              list-of-js-files)))])
    (displayln (generate-html-from-template first-template
                                            second-template))))

(define (css path)
  (generate-html (%sxml `(link (@ (rel "stylesheet")
                                  (type "text/css")
                                  (href ,path))))))

(define (js path)
  (generate-html (%sxml `(script (@ (type "text/javascript")
                                    (src ,path))))))

(module+ test
  (displayln (generate-html
              (html (head (%verbatim (css "src/styles.css")))
                    (body "Something")
                    (%verbatim (js "src/jquery.css"))))))

;; <link rel="stylesheet" type="text/css" href="mystyles.css" media="screen" />
