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

;; Using hashtable with curly-dict notation
(current-curly-dict hash)

(require racket/path)

(require "string.rkt")

(provide (all-defined-out))

(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define read-file #λ(call-with-input-file % port->string))

(define standardize-path simple-form-path)

(define get-relative-path
  #λ(simple-form-path (apply build-path %&)))

(define get-temp-dir #λ(find-system-path 'temp-dir))

(define remove-dir
  #λ(delete-directory/files % #:must-exist? #f))

(define create-dir make-directory*)

(define create-empty-file
  #λ(with-output-to-file %1
      (λ () (display ""))
      #:mode 'text
      #:exists 'truncate/replace))

(module+ test
  (let ([random-file (get-relative-path (get-temp-dir)
                                        "___random-file.txt")])
    (create-empty-file random-file)
    (check-equal? (file-exists? random-file) #t)
    (check-equal? (read-file random-file) "")))

(define (list-all-adocs path)
  (with-handlers ([exn:fail? (λ (exn) '())])
    (~>> (find-files #λ(string-ends-with? % ".adoc") (expand-user-path path))
      (map path->string))))

(module+ test
  (let* ([temp-dir (get-relative-path (get-temp-dir)
                                      "./ulquikit-tmp")]
         [filenames '("hello-world.adoc"
                      "hola-mundo.adoc"
                      "mostly-harmless.adoc"
                      "42.adoc")]

         [filenames/fullpath (for/list ([name (in-list filenames)])
                               (format "~a/~a" temp-dir name))])
    (with-handlers ([exn:fail? #λ(remove-dir temp-dir)])
      (remove-dir temp-dir)
      (create-dir temp-dir)
      (for ([path (in-list filenames/fullpath)])
        (create-empty-file path))

      (check-equal? (sort (list-all-adocs temp-dir) string<?)
                    (sort filenames/fullpath string<?))
      (remove-dir temp-dir))))

(define (path->directory path)
  (path->string
   (call-with-values #λ(split-path path)
                     (λ (dir file file-is-dir?)
                       (if file-is-dir?
                           (build-path dir (str file "/"))
                           dir)))))
(module+ test
  (check-equal? (path->directory "/tmp/tmp.rkt") "/tmp/")
  (check-equal? (path->directory "/tmp/tmp/")    "/tmp/tmp/")
  (check-equal? (path->directory "/m/src/ulquikit/generated-src/ulqui")
                "/m/src/ulquikit/generated-src/"))

(define (get-path . args)
  (let* ([paths/expanded (map expand-user-path args)]
         [paths/reversed (reverse (cons (current-directory) paths/expanded))]
         
         [paths/no-absolute (~> paths/reversed
                              (takef #λ(not (absolute-path? %)))
                              reverse)]
         [path/absolute     (~> paths/reversed
                              (dropf #λ(not (absolute-path? %)))
                              first)]
         [paths             (cons path/absolute paths/no-absolute)])
    (~> (apply build-path paths)
      simplify-path
      path->string)))

(module+ test
  (check-equal? (get-path "/tmp") "/tmp")
  (check-equal? (get-path "/tmp/") "/tmp/")
  (check-equal? (get-path "/tmp" "/hello") "/hello")
  (check-equal? (get-path "/tmp" "./hello") "/tmp/hello")
  (check-equal? (get-path "/tmp" "/hello" "~")
                (path->string (find-system-path 'home-dir)))
  (check-equal? (get-path "/tmp" "/hello" "/world" "./superman/")
                "/world/superman/")
  (check-equal? (get-path "world/")
                (path->string (build-path (current-directory) "world/"))))
