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

#lang racket

(provide (all-defined-out))

(module+ test
  (require rackunit))

(define (alist-get key an-alist)
  (cdr (assoc key an-alist)))

(module+ test
  (let ([an-alist '((a . 1) (b . hello) (c . "World"))])
   (check-equal? (alist-get 'a an-alist) 1)
   (check-equal? (alist-get 'b an-alist) 'hello)
   (check-equal? (alist-get 'c an-alist) "World")))