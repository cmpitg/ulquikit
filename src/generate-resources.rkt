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

(require racket/runtime-path)
(require "utils-path.rkt")

(define-runtime-path +current-dir+ "./")

;;
;; Return directory of current file
;;
(define (this-directory)
  (path->string +current-dir+))

(define (main)
  (make-directory* +generated-docs-location+)
  (copy-and-overwrite (string-append (this-directory) "css")
                      (string-append (this-directory) "js")
                      +generated-docs-location+))

(module+ main
  (main))
