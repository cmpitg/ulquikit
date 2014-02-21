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

(define-runtime-path +current-dir+ "./")

;;
;; Retrieve only file name or last directory name from a path.
;;
(define (get-name-from-path path)
  (~> path
    (string-split "/")
    last))

;;
;; Copy files or directories, overwrite older versions.
;;
(define (copy-and-overwrite . paths)
  (let* ([destination (last paths)]
         [sources     (drop-right paths 1)])
    (for ([source sources])
      (let* ([name-only (get-name-from-path source)])
        (delete-directory/files (string-join destination name-only)
                                #:must-exist? #f)
        (copy-directory/files source destination)))))

(define (main)
  (make-directory* (string-join +current-dir+ "../generated_docs/"))
  (copy-and-overwrite (string-join +current-dir+ "css")
                      (string-join +current-dir+ "js")
                      (string-join +current-dir+ "../generated_docs/")))
