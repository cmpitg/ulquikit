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

(require racket/runtime-path)
(require "utils-path.rkt")

(define-runtime-path +current-dir+ ".")

(define (this-dir)
  (expand-path +current-dir+))

(define +doc-location+ (expand-path (string-append (this-dir) "/../")))

(define (main)
  (define-values (vars content)
    (strip-header-vars (read-file (expand-path (string-append +doc-location+ "internal.md"))))))

(main)
