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

(provide get-name-from-path
         expand-path
         copy-and-overwrite

         (rename-out [this-dir get-bootstrap-dir])

         +docs-location+
         +generated-docs-location+)

(module+ test
  (require rackunit))

;;
;; Retrieve only file name or last directory name from a path.
;;
(define (get-name-from-path path)
  (~> path
    (string-split "/")
    last))

(module+ test
  (check-equal? (get-name-from-path "/mnt/")        "mnt")
  (check-equal? (get-name-from-path "/mnt")         "mnt")
  (check-equal? (get-name-from-path "/mnt/abc.rkt") "abc.rkt"))

;;
;; Expand and simplify path and convert it to string
;;
(define (expand-path path)
  (~> (expand-user-path path)
    simplify-path
    path->string))

;;
;; Copy files or directories, overwrite older versions.
;;
(define (copy-and-overwrite . paths)
  (let* ([destination (last paths)]
         [sources     (drop-right paths 1)])
    (for ([source sources])
      (let* ([name-only        (get-name-from-path source)]
             [full-destination (expand-path (string-append destination name-only))]
             [source           (expand-path source)])
        (when (or (file-exists? full-destination)
                  (directory-exists? full-destination))
          (displayln (~a "-> Deleting: " full-destination))
          (delete-directory/files full-destination))
        (displayln (~a "-> Copying " source " to " full-destination))
        (copy-directory/files source full-destination)))))

;;
;; Other constants
;;

(define-runtime-path +current-dir+ ".")

(define (this-dir)
  (expand-path +current-dir+))

(define +docs-location+
  (expand-path (string-append (this-dir) "/../")))

(define +generated-docs-location+
  (expand-path (string-append (this-dir) "/../../generated-docs/")))
