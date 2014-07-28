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

(require "../command-core.rkt")

(require "../utils/utils.rkt")
(require "../utils/path.rkt")
(require "../utils/string.rkt")

;; (provide run
;;          help)

(module+ test
  (require rackunit))

;; #lang racket

;; #lang racket

(define (create-snippet #:type type
                        #:name name
                        #:linenum linenum
                        #:content content)
  (let ([type (if (symbol? type)
                  type
                  (string->symbol type))]
        [name (if (symbol? name)
                  name
                  (string->symbol name))])
    {'type type
     'name name
     'linenum linenum
     'content content}))

(module+ test
  (check-equal? (create-snippet #:type 'file
                                #:name 'hello-world
                                #:linenum 10
                                #:content "Hmm")
                {'type 'file
                 'name 'hello-world
                 'linenum 10
                 'content "Hmm"})
  (check-equal? (create-snippet #:type "string"
                                #:name "string"
                                #:linenum 100
                                #:content "string")
                {'type 'string
                 'name 'string
                 'linenum 100
                 'content "string"}))


;; lang racket

(define is-block-delimiter?
  #λ(regexp-match? #rx"^----( *)$" %))

(module+ test
  (check-equal? (is-block-delimiter? "----")    #t)
  (check-equal? (is-block-delimiter? " ----")   #f)
  (check-equal? (is-block-delimiter? "---- ")   #t)
  (check-equal? (is-block-delimiter? "----  ")  #t)
  (check-equal? (is-block-delimiter? "----a")   #f))

(define is-block-title?
  #λ(regexp-match? #rx"^\\.(file|code)::" %))

(module+ test
  (check-equal? (is-block-title? ".file::something")       #t)
  (check-equal? (is-block-title? ".file::something else")  #t)
  (check-equal? (is-block-title? ".file::")                #t)
  (check-equal? (is-block-title? ".file:something")        #f))

(define get-snippet-type
  #λ(~> (string-rest %)
      (string-split "::")
      (list-ref 0)
      string->symbol))

(module+ test
  (check-equal? (get-snippet-type ".file::")  'file)
  (check-equal? (get-snippet-type ".code::")  'code))

(define get-snippet-name
  #λ(~> (string-rest %)
      (string-split "::")
      (append '(""))
      (list-ref 1)))

(module+ test
  (check-equal? (get-snippet-name ".file::")     "")
  (check-equal? (get-snippet-name ".code::")     "")
  (check-equal? (get-snippet-name ".file::abc")  "abc")
  (check-equal? (get-snippet-name ".code::a b")  "a b"))

(define (add-snippets snippets snippet)
  (let* ([type (snippet 'type)]
         [name (snippet 'name)]

         [snippets/typed         (snippets type)]
         [snippets/typed/updated (snippets/typed name snippet)])
    (snippets type snippets/typed/updated)))

(module+ test
  (check-equal? (add-snippets {'file {}
                               'code {}}
                              (create-snippet #:type 'file
                                              #:name 'hello
                                              #:linenum 10
                                              #:content "Something"))
                {'file {'hello {'type 'file
                                'name 'hello
                                'linenum 10
                                'content "Something"}}
                 'code {}})

  (check-equal? (add-snippets {'file {'hello {'type 'file
                                              'name 'hello
                                              'linenum 10
                                              'content "Something"}}
                               'code {}}
                              (create-snippet #:type 'code
                                              #:name 'say-something
                                              #:linenum 100
                                              #:content "Something else"))
                {'file {'hello {'type 'file
                                'name 'hello
                                'linenum 10
                                'content "Something"}}
                 'code {'say-something {'type 'code
                                        'name 'say-something
                                        'linenum 100
                                        'content "Something else"}}}))


(define (extract-snippet path)
  (let* ([file-content (read-file path)]
         [lines        (string-split file-content "\n" #:trim? #f)]

         [snippets        (box {'file {}
                                'code {}})]

         [prev-prev-line  (box "")]
         [prev-line       (box "")]

         [snippet-type    (box null)]
         [snippet-content (box "")]
         [snippet-name    (box "")]
         [snippet-linenum (box 0)]
         [inside-snippet  (box #f)])

    (for ([line-num    (in-naturals 1)]
          [line        (in-list lines)])
      (if (is-block-delimiter? line)
          (cond [(unbox inside-snippet)

                 (box-set! inside-snippet #f)
                 (box-swap! snippets
                            add-snippets
                            (create-snippet #:type (unbox snippet-type)
                                            #:name (unbox snippet-name)
                                            #:linenum (unbox snippet-linenum)
                                            #:content (unbox snippet-content)))]

                [else
                 (when (is-block-title? (unbox prev-prev-line))
                   (box-set! inside-snippet #t)

                   (box-set! snippet-type (get-snippet-type (unbox prev-prev-line)))
                   (box-set! snippet-name (get-snippet-name (unbox prev-prev-line)))
                   (box-set! snippet-content "")
                   (box-set! snippet-linenum (dec line-num)))])
          (when (unbox inside-snippet)
            (box-swap! snippet-content string-append "\n" line)))

      ;; Always update previous line
      (box-set! prev-prev-line (unbox prev-line))
      (box-set! prev-line      line))

    (unbox snippets)))


(define (extract-snippets from-dir)
  (for/fold ([snippet {}])
      ([file (list-all-adocs (standardize-path from-dir))])
    (dict-merge snippet (extract-snippet file))))

(module+ test
  (let* ([temp-dir (get-relative-path (get-temp-dir)
                                      "./ulqui-extract-snippets")]

         [expected-code-snippets
          {"main-program" (string-join
                           '(";; include::utils"
                             ""
                             "(module+ main"
                             "  (displayln (string-reverse \"¡Hola mundo!\")))"
                             "")
                           "\n")
           "use-rackjure" (string-join
                           '("#lang rackjure"
                             "(current-curly-dict hash)")
                           "\n")
           "license-header" ";; Just a sample license header."
           "utils" ";; include::utils-string"
           "utils-string" (string-join
                           '("(define (string-reverse str)"
                             "  (~> (string->list str)"
                             "    reverse"
                             "    list->string))")
                           "\n")}]

         [expected-file-snippets
          {"/tmp/tmp.rkt" (string-join
                           '(";; include::license-header"
                             ""
                             ";; include::use-rackjure"
                             ""
                             ";; include::main-program"
                             ""
                             "== Main program"
                             "")
                           "\n")}]

         [file-list '("Main"
                      "License"
                      "inside/Utils"
                      "inside/Utils-String")]
         [source-files (for/list ([name (in-list file-list)])
                         (get-relative-path (format "~a./~a.adoc"
                                                    temp-dir
                                                    name)))]
         [content
          {"Main" (string-join
                   (list "= A document"
                         ""
                         "Just a hello world program"
                         ""
                         ".file::/tmp/tmp.rkt"
                         "[source,racket,linenums]"
                         "----"
                         (expected-file-snippets "/tmp/tmp.rkt")
                         "----"
                         ".code::main-program"
                         "[source,racket,linenums]"
                         "----"
                         (expected-code-snippets "main-program")
                         "----"
                         ".code::use-rackjure"
                         "[source]"
                         "----"
                         (expected-code-snippets "use-rackjure")
                         "----"
                         "")
                   "\n")
           "License" (string-join
                      (list "= License header"
                            ""
                            ".code::license-header"
                            "[source,racket]"
                            "----"
                            (expected-code-snippets "license-header")
                            "----")
                      "\n")
           "inside/Utils" (string-join
                           (list "= Utils"
                                 ""
                                 "Right now, we just want to include string utililities."
                                 ""
                                 ".code::utils"
                                 "[source,racket,linenums]"
                                 "----"
                                 (expected-code-snippets "utils")
                                 "----")
                           "\n")
           "inside/Utils-String" (string-join
                                  (list "= String Utilities"
                                        ""
                                        ".code::utils-string"
                                        "[source,racket,linenums]"
                                        "----"
                                        (expected-code-snippets "utils-string")
                                        "----")
                                  "\n")}])
    (with-handlers ([exn:fail? #λ(remove-dir temp-dir)])
      (remove-dir temp-dir)
      (create-dir (get-relative-path temp-dir
                                     "./inside"))
      (for ([(filename content) (in-hash content)])
        (let ([path (get-relative-path temp-dir
                                       (format "./~a.adoc" filename))])
          (display-to-file content path)))

      (let* ([snippets (extract-snippets temp-dir)]
             [code-snippet (snippets 'code)]
             [file-snippet (snippets 'file)])

        (for ([(name content) code-snippet])
          (check-equal? (code-snippet name) content))

        (for ([(name content) file-snippet])
          (check-equal? (file-snippet name) content)))
      (remove-dir temp-dir))))


;; (define (run #:from [from "src"]
;;              #:to   [to   "generated-src"])
;;   (display-command "generate-src")
;;   (~> (extract-snippets from)
;;     (include-file-snippets)
;;     (write-snippets-to-files to)))


;; TO-BE-IMPLEMENTED
