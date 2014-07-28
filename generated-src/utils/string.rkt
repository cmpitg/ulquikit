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

(require srfi/1)

(require "utils.rkt")

(provide (all-defined-out))

(module+ test
  (require rackunit))

;; #lang racket

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

(module+ test
  (check-equal? (trim "     ") "")
  (check-equal? (trim "  -  ") "-"))

(define string-starts-with?
  #λ(regexp-match? (regexp (str "^" (regexp-quote %2))) %1))

(module+ test
  (check-equal? (string-starts-with? "" "") #t)
  (check-equal? (string-starts-with? "a" "") #t)
  (check-equal? (string-starts-with? "a" "a") #t)
  (check-equal? (string-starts-with? "a" "b") #f)
  (check-equal? (string-starts-with? "abcdef" "abc") #t)
  (check-equal? (string-starts-with? "abcdef" "abb") #f)
  (check-equal? (string-starts-with? "$abc" "$a") #t))

(define string-ends-with?
  #λ(regexp-match? (regexp (str (regexp-quote %2) "$")) %1))

(module+ test
  (check-equal? (string-ends-with? "" "") #t)
  (check-equal? (string-ends-with? "a" "") #t)
  (check-equal? (string-ends-with? "a" "a") #t)
  (check-equal? (string-ends-with? "a" "b") #f)
  (check-equal? (string-ends-with? "abcdef" "def") #t)
  (check-equal? (string-ends-with? "abcdef" "dee") #f)
  (check-equal? (string-ends-with? "$ab$$c" "b$$c") #t))

(define string-rest #λ(~> (string->list %)
                        ((λ (lst)
                           (if (empty? lst)
                               lst
                               (rest lst))))
                        list->string))

(module+ test
  (check-equal? (string-rest "") "")
  (check-equal? (string-rest "a") "")
  (check-equal? (string-rest "ab") "b")
  (check-equal? (string-rest "abc") "bc"))

