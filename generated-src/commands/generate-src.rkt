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

;; lang racket

(define get-snippet-content
  #λ(string-join (% 'lines) "\n"))


(define (create-snippet #:type type
                        #:name name
                        #:linenum linenum
                        #:lines lines
                        #:processed? [processed? #f])
  (let ([type (if (symbol? type)
                  type
                  (string->symbol type))]
        [name (if (symbol? name)
                  (symbol->string name)
                  name)]
        [lines (if (string? lines)
                   (string-split lines "\n" #:trim? #f)
                   lines)])
    {'type type
     'name name
     'linenum linenum
     'lines lines
     'processed? processed?}))

(module+ test
  (check-equal? (create-snippet #:type 'file
                                #:name 'hello-world
                                #:linenum 10
                                #:lines '("Hmm"))
                {'type 'file
                 'name "hello-world"
                 'linenum 10
                 'lines '("Hmm")
                 'processed? #f})
  (check-equal? (create-snippet #:type "string"
                                #:name "string"
                                #:linenum 100
                                #:lines "string")
                {'type 'string
                 'name "string"
                 'linenum 100
                 'lines '("string")
                 'processed? #f}))


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
                                              #:lines '("Something")))
                {'file {"hello" {'type 'file
                                 'name "hello"
                                 'linenum 10
                                 'lines '("Something")
                                 'processed? #f}}
                 'code {}})

  (check-equal? (add-snippets {'file {"hello" {'type 'file
                                               'name "hello"
                                               'linenum 10
                                               'lines '("Something")
                                               'processed? #f}}
                               'code {}}
                              (create-snippet #:type 'code
                                              #:name 'say-something
                                              #:linenum 100
                                              #:lines '("Something else")))
                {'file {"hello" {'type 'file
                                 'name "hello"
                                 'linenum 10
                                 'lines '("Something")
                                 'processed? #f}}
                 'code {"say-something" {'type 'code
                                         'name "say-something"
                                         'linenum 100
                                         'lines '("Something else")
                                         'processed? #f}}}))


(define (extract-snippets-from-file path)
  (let* ([file-content (read-file path)]
         [lines        (string-split file-content "\n" #:trim? #f)]

         [snippets        (box {'file {}
                               'code {}})]

         [prev-prev-line  (box "")]
         [prev-line       (box "")]

         [snippet-type    (box null)]
         [snippet-lines   (box '())]
         [snippet-name    (box "")]
         [snippet-linenum (box 0)]
         [inside-snippet  (box #f)])

    (for ([line-num    (in-naturals 1)]
          [line        (in-list lines)])

      (cond [(and (unbox inside-snippet)
                  (not (is-block-delimiter? line)))

             (box-swap! snippet-lines append (list line))]

            [(and (unbox inside-snippet)
                  (is-block-delimiter? line))

             (box-set! inside-snippet #f)
             (box-swap! snippets
                        add-snippets
                        (create-snippet #:type (unbox snippet-type)
                                        #:name (unbox snippet-name)
                                        #:linenum (unbox snippet-linenum)
                                        #:lines (unbox snippet-lines)))]
            [(is-block-delimiter? line)

             (when (is-block-title? (unbox prev-prev-line))
               (box-set! inside-snippet #t)

               (box-set! snippet-type (get-snippet-type (unbox prev-prev-line)))
               (box-set! snippet-name (get-snippet-name (unbox prev-prev-line)))
               (box-set! snippet-lines '())
               (box-set! snippet-linenum (dec line-num)))])

      ;; Always update previous line
      (box-set! prev-prev-line (unbox prev-line))
      (box-set! prev-line      line))

    (unbox snippets)))


(define (extract-snippets from-dir)
  (for/fold ([snippet {}])
      ([file (list-all-adocs (standardize-path from-dir))])
    (dict-merge snippet (extract-snippets-from-file file))))

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

        (for ([(name snippet) code-snippet])
          (check-equal? (get-snippet-content snippet)
                        (expected-code-snippets name)))

        (for ([(name snippet) file-snippet])
          (check-equal? (get-snippet-content snippet)
                        (expected-file-snippets name))))
      (remove-dir temp-dir))))

;; lang racket

(define (update-snippet/boxed snippets/box snippet)
  (let* ([type (snippet 'type)]
         [name (snippet 'name)])
    (box-swap! snippets/box
               (λ (snippets)
                 (let* ([snippets/typed (snippets type)]
                        [snippets/typed/updated (snippets/typed name snippet)]
                        [snippets/updated (snippets type snippets/typed/updated)])
                   snippets/updated)))))

(module+ test
  (let* ([snippets {'file {}
                    'code {"hello" {'type 'code
                                    'name "hello"
                                    'lines '("original")
                                    'linenum 20
                                    'processed? #f}}}]
         [snippets/box (box snippets)])
    (update-snippet/boxed snippets/box
                          {'type 'code
                           'name "hello"
                           'lines '("changed")
                           'linenum 10
                           'processed? #t})
    (check-equal? (unbox snippets/box)
                  {'file {}
                   'code {"hello" {'type 'code
                                   'name "hello"
                                   'lines '("changed")
                                   'linenum 10
                                   'processed? #t}}})))

(define is-include-directive?
  #λ(or (regexp-match? #px"^[#;/-]{2} include::.*" (trim %))
        (regexp-match? #px"^<!-- include::.* -->" (trim %))
        (regexp-match? #px"^/\\* include::.* \\*/" (trim %))))

(module+ test
  (check-equal? (is-include-directive? "  ;; include::") #t)
  (check-equal? (is-include-directive? ";; include::") #t)
  (check-equal? (is-include-directive? "a;; include::") #f)
  (check-equal? (is-include-directive? ";; include::something") #t)
  (check-equal? (is-include-directive? "## include::something") #t)
  (check-equal? (is-include-directive? "// include::something") #t)
  (check-equal? (is-include-directive? "/* include::something */") #t)
  (check-equal? (is-include-directive? "<!-- include::something -->") #t)
  (check-equal? (is-include-directive? "a <!-- include::something -->") #f))

(define (get-included-snippet-name line)
  (if (is-include-directive? line)
      (let* ([line (trim line)]
             [line-2 (if (string-ends-with? line " -->")
                         (first (string-split line " -->"))
                         line)]
             [line-3 (if (string-ends-with? line-2 " */")
                         (first (string-split line-2 " */"))
                         line-2)]
             [splitted (string-split line-3 "include::")])
        (if (> (length splitted) 1)
            (last splitted)
            ""))
      ""))

(module+ test
  (check-equal? (get-included-snippet-name "  ;; include::") "")
  (check-equal? (get-included-snippet-name ";; include::") "")
  (check-equal? (get-included-snippet-name ";; include::something") "something")
  (check-equal? (get-included-snippet-name "## include::something") "something")
  (check-equal? (get-included-snippet-name "// include::something") "something")
  (check-equal? (get-included-snippet-name "/* include::something */") "something")
  (check-equal? (get-included-snippet-name "<!-- include::something -->") "something")
  (check-equal? (get-included-snippet-name "a <!-- include::something -->") ""))

(define (get-snippet-by-name snippets
                             name
                             #:type [type 'code])
  (~> snippets type name))

(define (include-snippet boxed
                         target
                         [included? {}])
  (define updated-included? (included? (target 'name) #t))
  (unless (target 'processed?)
    (let* ([lines
            (for/list ([line (target 'lines)])
              (if (is-include-directive? line)
                  (let* ([included-snippet-name (get-included-snippet-name line)]
                         [snippet-to-include    (get-snippet-by-name (unbox boxed)
                                                                     included-snippet-name)])
                    (cond [(or (updated-included? included-snippet-name)
                               (not snippet-to-include))

                           ;; This snippet has already been included on the
                           ;; track or there's no such snippet ⇨ do nothing
                           line]

                          [(snippet-to-include 'processed?)

                           ;; When the snippet is already processed, simply
                           ;; return it
                           (string-join (snippet-to-include 'lines) "\n")]

                          [else

                           ;; When the snippet we're about to include exists
                           ;; and hasn't been processed
                           (include-snippet boxed
                                            snippet-to-include
                                            (updated-included? included-snippet-name #t))

                           ;; Of course, then we must return it after
                           ;; processed
                           (~> (get-snippet-by-name (unbox boxed)
                                                    included-snippet-name)
                             'lines
                             (string-join "\n"))]))

                  line))]

           [new-snippet (create-snippet #:type (target 'type)
                                        #:name (target 'name)
                                        #:linenum (target 'linenum)
                                        #:lines lines
                                        #:processed? #t)])
      (update-snippet/boxed boxed new-snippet))))

(module+ test
  (let* ([file-snippet-tmp {'name "/tmp/tmp.rkt"
                            'type 'file
                            'lines '(";; include::A")
                            'linenum 10}]
         [snippets {'file {"/tmp/tmp.rkt" file-snippet-tmp}
                    'code {"A" {'name "A"
                                'type 'code
                                'lines '("World" ";; include::B")
                                'linenum 20}
                           "B" {'name "B"
                                'type 'code
                                'lines '("Hello")
                                'linenum 30}
                           "C" {'name "C"
                                'type 'code
                                'lines '("Unprocessed")
                                'linenum 30
                                'processed? #f}}}]
         [boxed (box snippets)])
    (include-snippet boxed file-snippet-tmp {})
    (check-equal? (unbox boxed)
                  {'file {"/tmp/tmp.rkt" {'name "/tmp/tmp.rkt"
                                          'type 'file
                                          'lines '("World\nHello")
                                          'linenum 10
                                          'processed? #t}}
                   'code {"A" {'name "A"
                               'type 'code
                               'lines '("World" "Hello")
                               'linenum 20
                               'processed? #t}
                          "B" {'name "B"
                               'type 'code
                               'lines '("Hello")
                               'linenum 30
                               'processed? #t}
                          "C" {'name "C"
                               'type 'code
                               'lines '("Unprocessed")
                               'linenum 30
                               'processed? #f}}}))

  (let* ([snippet-a {'name "A"
                     'type 'code
                     'lines '("World" ";; include::B")
                     'linenum 20}]
         [snippets {'file {}
                    'code {"A" snippet-a
                           "B" {'name "B"
                                'type 'code
                                'lines '("Hello" ";; include::A")
                                'linenum 30}}}]
         [boxed (box snippets)])
    (include-snippet boxed snippet-a {})
    (check-equal? (unbox boxed)
                  {'file {}
                   'code {"A" {'name "A"
                               'type 'code
                               'lines '("World" "Hello\n;; include::A")
                               'linenum 20
                               'processed? #t}
                          "B" {'name "B"
                               'type 'code
                               'lines '("Hello" ";; include::A")
                               'linenum 30
                               'processed? #t}}}))

  (let* ([snippet-a {'name "A"
                     'type 'code
                     'lines '("World" ";; include::B")
                     'linenum 20}]
         [snippets {'file {}
                    'code {"A" snippet-a}}]
         [boxed (box snippets)])
    (include-snippet boxed snippet-a {})
    (check-equal? (unbox boxed)
                  {'file {}
                   'code {"A" {'name "A"
                               'type 'code
                               'lines '("World" ";; include::B")
                               'linenum 20
                               'processed? #t}}})))

;; (define (include-snippet snippets target)
;;   (let* ([boxed (box snippets)]
;;          [snippet-name (target 'name)])
;;     (include-snippet boxed
;;                              #:name snippet-name
;;                              #:included {})))


;; (define (run #:from [from "src"]
;;              #:to   [to   "generated-src"])
;;   (display-command "generate-src")
;;   (~> (extract-snippets from)
;;     (include-file-snippets)
;;     (write-snippets-to-files to)))


;; TO-BE-IMPLEMENTED
