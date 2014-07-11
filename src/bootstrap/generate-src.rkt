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

;;
;; I know I know.  This file contains lots of bad practices.  However, for the
;; purpose of keeping the code clean, simple, and human-readable, I implement
;; it that way.  Drop me a line if you have better idea that could help.
;;

;;
;; Rules of thumb:
;; * Global vars are not accessed directly but through helpers
;; * Global vars which don't have global lifespan must be parameterize'd
;;

#lang rackjure

(require racket/runtime-path)
(define-runtime-path +this-directory+ ".")

;; Code blocks
(define *code-blocks* (make-parameter {}))

(module+ main
  (let* ([source-file (get-relative-path +this-directory+ "../Main.adoc")]
         [content     (read-file source-file)])
    (process-string content
      ;; Continuously look for "[source" block and extract code blocks
      (let search-and-update-blocks
          []
        (when (look-for "[source")
          (goto-next "[source")
          (next-line)

          (unless (code-block-begins? (get-line))
            (let* ([title   (trim (get-line))]
                   [name    (get-code-block-name title)]
                   [type    (get-code-block-type title)]

                   ;; Positions
                   [start    (begin
                               (next-line)
                               (next-line)
                               (to-beginning-of-line)
                               (get-position))]
                   [end      (begin
                               (goto-next "----")
                               (prev-line)
                               (to-end-of-line)
                               (get-position))]

                   [content  (get-substring #:from start
                                            #:to   end)])
              (update-blocks #:content content
                             #:name    name
                             #:type    type))))

        (if (look-for "[source")
            (search-and-update-blocks)
            (get-blocks))))))
