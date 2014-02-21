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

(module+ test
  (require rackunit))

(define-runtime-path +current-dir+ "./")

;;
;; Return directory of current file
;;
(define (this-directory)
  (path->string +current-dir+))

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
;; Copy files or directories, overwrite older versions.
;;
(define (copy-and-overwrite . paths)
  (let* ([destination (last paths)]
         [sources     (drop-right paths 1)])
    (for ([source sources])
      (let* ([name-only        (get-name-from-path source)]
             [full-destination (string-append destination name-only)])
        (displayln (~a "-> Deleting: " (string-append destination name-only)))
        (delete-directory/files full-destination
                                #:must-exist? #f)
        (displayln (~a "-> Copying " source " to " destination))
        (copy-directory/files source full-destination)))))

(define (main)
  (make-directory* (string-append (this-directory) "../generated_docs/"))
  (copy-and-overwrite (string-append (this-directory) "css")
                      (string-append (this-directory) "js")
                      (string-append (this-directory) "../generated_docs/")))

(main)
