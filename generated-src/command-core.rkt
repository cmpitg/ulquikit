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

(require srfi/1)

(require "utils/string.rkt")

(provide (all-defined-out))

(module+ test
  (require rackunit))

(define (display-command title)
  (displayln (str "----> " title)))

(define (run-help command)
  (eval `(local-require ,(str command ".rkt")))
  (eval '(help)))

;; #lang racket

(define is-argument? #λ(not (string-starts-with? % "-")))

(module+ test
  (check-equal? (is-argument? "")     #t)
  (check-equal? (is-argument? "a")    #t)
  (check-equal? (is-argument? "-a")   #f)
  (check-equal? (is-argument? "--a")  #f)
  (check-equal? (is-argument? "-")    #f))

(define is-option? #λ(not (is-argument? %)))

(module+ test
  (check-equal? (is-option? "")     #f)
  (check-equal? (is-option? "a")    #f)
  (check-equal? (is-option? "-a")   #t)
  (check-equal? (is-option? "--a")  #t)
  (check-equal? (is-option? "-")    #t))

(define parse-keyword
  #λ(string->keyword (~>> (string->list %)
                       (drop-while (λ (ch) (eq? #\- ch)))
                       list->string)))

(module+ test
  (check-equal? (parse-keyword "-h")      '#:h)
  (check-equal? (parse-keyword "--help")  '#:help)
  (check-equal? (parse-keyword "---help") '#:help))

(define (parse-command-args args)
  (let parse ([last-keyword null]
              [args         (string-split args " ")]
              [result       '()])
    (if (empty? args)
        (if (null? last-keyword)
            result
            (append result (list (list last-keyword #t))))
        (let* ([current-arg (first args)]
               [more        (rest args)]

               [next-keyword (if (is-argument? current-arg)
                                 null
                                 (parse-keyword current-arg))]
               [arg/converted (if (is-argument? current-arg)
                                  (if-let [value (string->number current-arg)]
                                    value
                                    current-arg)
                                  (parse-keyword current-arg))]
               [next-result-value
                (or (and (is-argument? current-arg)
                         (or (and (null? last-keyword)
                                  (list arg/converted))
                             (list (list last-keyword arg/converted))))

                    (and (is-option? current-arg)
                         (or (and (null? last-keyword)
                                  '())
                             (list (list last-keyword #t)))))])
          (parse next-keyword
                 more
                 (append result next-result-value))))))

(module+ test
  (check-equal? (parse-command-args "") '())

  (check-equal? (parse-command-args "hello-world")
                '("hello-world"))

  (check-equal? (parse-command-args "hello world")
                '("hello" "world"))

  (check-equal? (parse-command-args "--help")
                '([#:help #t]))

  (check-equal? (parse-command-args "hello --help")
                '("hello" [#:help #t]))

  (check-equal? (parse-command-args "--help hello")
                '([#:help "hello"]))

  (check-equal? (parse-command-args "hello world --help --set-tab 4")
                '("hello" "world"
                  [#:help #t]
                  [#:set-tab 4])))


;; (define (run-command . arguments)
;;   (void)
;;   )
