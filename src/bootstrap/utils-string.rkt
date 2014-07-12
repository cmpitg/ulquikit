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

;; Using hashtable with curly-dict notation
(current-curly-dict hash)

(require srfi/1)
(require "utils.rkt")

(module+ test
  (require rackunit))

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (trim str)
  (let* ([str/list                  (string->list str)]
         [str/list/trim-begin       (drop-while #λ(equal? % #\space)
                                                str/list)]
         [str/list/reversed         (reverse str/list/trim-begin)]
         [str/list/reversed/trimmed (drop-while #λ(equal? % #\space)
                                                str/list/reversed)])
    (~> str/list/reversed/trimmed
      reverse
      list->string)))
