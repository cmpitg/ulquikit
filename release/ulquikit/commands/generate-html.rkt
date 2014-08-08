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

#lang rackjure

(current-curly-dict hash)

(require "../command-core.rkt")
(require "../utils/path.rkt")

(provide run)

(module+ test
  (require rackunit))

(define asciidoctor-format-command
  #λ(format "asciidoctor ~a -d book -o ~a" %1 %2))

(define (render-asciidoc input-file output-file)
  (displayln (str "-> " input-file " => " output-file))
  (system (asciidoctor-format-command input-file output-file)))


(define get-output-file
  #λ(~> (file-name-from-path %)
      path->string
      (string-replace ".adoc" ".html")))

(module+ test
  (check-equal? (get-output-file "/tmp/tmp.adoc")   "tmp.html")
  (check-equal? (get-output-file "/tmp/world.adoc") "world.html"))


(define (generate-html #:from      [from "src"]
                       #:to        [to "generated-html"])
  (let* ([from  (get-path from)]
         [to    (get-path to)]
         [docs  (if (file-exists? from)
                    (let ([file (list from)])
                      (set! from (path->directory from))
                      file)
                    (list-all-adocs from))])
    (parameterize ([current-directory from])
      (for ([doc docs])
        (render-asciidoc doc
                         (get-relative-path to (get-output-file doc)))))))


(define (run #:from [from "src"]
             #:to   [to   "generated-html"])
  (display-command "Generating HTML")
  (generate-html #:from from
                 #:to   to))
