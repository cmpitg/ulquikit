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

(provide (all-defined-out))

(module+ test
  (require rackunit))

;;
;; Create a temporary file with `content` as its content and return the file
;; path.  Overwrite existing file.
;;
(define (create-temp-file content)
  (define path (make-temporary-file))
  (display-to-file content path
                   #:mode 'text
                   #:exists 'update)
  path)

;;
;; Replace file extension.
;;
;; E.g.
;;
;; (replace-file-extension "hello.md" "html")
;; ;; => "hello.html"
;; (replace-file-extension "/tmp/hello.md" "html")
;; ;; => "/tmp/hello.html"
;; (replace-file-extension "/tmp/hello." "html")
;; ;; => "/tmp/hello.html"
;; (replace-file-extension "/tmp/hello" "html")
;; ;; => "/tmp/hello.html"
;;
(define (replace-file-extension path new-extension)
  (define char-list (reverse (string->list path)))
  (define ensure-dot
    (let ([try-removing-dot (drop-while (lambda (ele)
                                          (and (not (char=? #\. ele))
                                               (not (char=? #\/ ele))))
                                        char-list)])
      (if (or (empty? try-removing-dot)
              (char=? #\/ (first try-removing-dot)))
          (cons #\. char-list)
          char-list)))
  (define after-removing-dot (drop-while (lambda (ele) (not (char=? ele #\.)))
                                         ensure-dot))
  (~> (reverse after-removing-dot)
    list->string
    (string-append new-extension)))

(module+ test
  (check-equal? (replace-file-extension "hello.md" "html") "hello.html")
  (check-equal? (replace-file-extension "/tmp/hello.md" "html") "/tmp/hello.html")
  (check-equal? (replace-file-extension "/tmp/hello." "html") "/tmp/hello.html")
  (check-equal? (replace-file-extension "/tmp/hello" "html") "/tmp/hello.html"))

;;
;; Return file content as string
;;
(define (read-file path)
  (file->string path #:mode 'text))
