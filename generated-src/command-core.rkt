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

(current-curly-dict hash)

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

(define option->keyword
  #λ(string->keyword (~> (string->list %)
                       (dropf (λ (ch) (eq? #\- ch)))
                       list->string)))

(module+ test
  (check-equal? (option->keyword "-h")      '#:h)
  (check-equal? (option->keyword "--help")  '#:help)
  (check-equal? (option->keyword "---help") '#:help))

(define try-convert-value
  #λ(if-let [value (string->number %)]
      value
      %))

(module+ test
  (check-equal? (try-convert-value "1") 1)
  (check-equal? (try-convert-value "a") "a"))

(define (parse-command-args args)
  (let ([arguments (takef args is-argument?)]
        [rest-args (dropf args is-argument?)])
    (let parse-options ([rest-args  rest-args]
                        [options    {}])
      (if (empty? rest-args)
          {'arguments arguments
           'options   options}
          (let* ([option-name   (first rest-args)]
                 [option-values (takef (drop rest-args 1) is-argument?)]
                 [rest-args     (dropf (rest rest-args)   is-argument?)]

                 [option-values/converted (map try-convert-value option-values)]

                 [name   (option->keyword option-name)]
                 [values (cond [(zero? (length option-values/converted))
                                #t]
                               [(= (length option-values/converted) 1)
                                (first option-values/converted)]
                               [else
                                option-values/converted])])
            (parse-options rest-args
                           (options name values)))))))

(module+ test
  (check-equal? (parse-command-args '())
                {'arguments '()
                 'options   {}})

  (check-equal? (parse-command-args '("hello-world"))
                {'arguments '("hello-world")
                 'options   {}})

  (check-equal? (parse-command-args '("hello" "world"))
                {'arguments '("hello" "world")
                 'options   {}})

  (check-equal? (parse-command-args '("--help"))
                {'arguments '()
                 'options   {'#:help #t}})

  (check-equal? (parse-command-args '("hello" "--help"))
                {'arguments '("hello")
                 'options   {'#:help #t}})

  (check-equal? (parse-command-args '("hello" "--help" "world" "args"))
                {'arguments '("hello")
                 'options   {'#:help '("world" "args")}})

  (check-equal? (parse-command-args '("--help" "hello"))
                {'arguments '()
                 'options   {'#:help "hello"}})

  (check-equal? (parse-command-args '("hello" "world" "--help" "--set-tab" "4"))
                {'arguments '("hello" "world")
                 'options   {'#:help #t
                             '#:set-tab 4}}))


;; TO-BE-IMPLEMENTED
