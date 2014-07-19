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

#lang rackjure

(require racket/path)

(require "utils-path.rkt")

(require racket/runtime-path)
(define-runtime-path +this-directory+ ".")

(module+ test
  (require rackunit))

(define (get-output-file input-file)
  (~> (file-name-from-path input-file)
    path->string
    (string-replace ".adoc" ".html")))

(module+ test
  (check-equal? (get-output-file "/tmp/tmp.adoc") "tmp.html")
  (check-equal? (get-output-file "/tmp/d.adoc")   "d.html"))

;; (module+ main
;;   (let* ([project-dir        (get-relative-path +this-directory+ "../../")]
;;          [src-dir            (get-relative-path project-dir "./src")]
;;          [generated-docs-dir (get-relative-path project-dir "./generated-docs/")])
;;     (parameterize ([current-directory project-dir])
;;       (for ([input-file (get-all-adocs src-dir)])
;;         (render-asciidoc input-file
;;                          (get-relative-path generated-docs-dir
;;                                             (get-output-file input-file)))))))
