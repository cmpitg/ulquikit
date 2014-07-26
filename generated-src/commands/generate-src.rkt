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

(require "../utils/path.rkt")
(require "../utils/string.rkt")

;; (provide run
;;          help)

(module+ test
  (require rackunit))

;; #lang racket

;; #lang racket

(define (create-block #:type type
                      #:name name
                      #:content content)
  (let ([type (if (symbol? type)
                  type
                  (string->symbol type))]
        [name (if (symbol? name)
                  name
                  (string->symbol name))])
    {'type type
     'name name
     'content content}))

(module+ test
  (check-equal? (create-block #:type 'file
                              #:name 'hello-world
                              #:content "Hmm")
                {'type 'file
                 'name 'hello-world
                 'content "Hmm"})
  (check-equal? (create-block #:type "string"
                              #:name "string"
                              #:content "string")
                {'type 'string
                 'name 'string
                 'content "string"}))


;; lang racket

(define (extract-blocks from-dir)
  (for/fold ([block {}])
      ([file (list-all-adocs (standardize-path from-dir))])
    (dict-merge block (extract-block file))))

(module+ test
  (let* ([temp-dir (get-relative-path (get-temp-dir)
                                      "./ulqui-extract-blocks/inside")]

         [expected-code-blocks
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

         [expected-file-blocks
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
                   '("= A document"
                     ""
                     "Just a hello world program"
                     ""
                     ".file::/tmp/tmp.rkt"
                     "[source,racket,linenums]"
                     "----"
                     (expected-file-blocks "/tmp/tmp.rkt")
                     "----"
                     ".code::main-program"
                     "[source,racket,linenums]"
                     "----"
                     (expected-code-blocks "main-program")
                     "----"
                     ".code::use-rackjure"
                     "[source]"
                     "----"
                     (expected-code-blocks "use-rackjure")
                     "----"
                     "")
                   "\n")
           "License" (string-join
                      '("= License header"
                        ""
                        ".code::license-header"
                        "[source,racket]"
                        "----"
                        (expected-code-blocks "license-header")
                        "----")
                      "\n")
           "inside/Utils" (string-join
                           '("= Utils"
                             ""
                             "Right now, we just want to include string utililities."
                             ""
                             ".code::utils"
                             "[source,racket,linenums]"
                             "----"
                             (expected-code-blocks "utils")
                             "----")
                           "\n")
           "inside/Utils-String" (string-join
                                  '("= String Utilities"
                                    ""
                                    ".code::utils-string"
                                    "[source,racket,linenums]"
                                    "----"
                                    (expected-code-blocks "utils-string")
                                    "----")
                                  "\n")}])
    (with-handlers ([exn:fail? #λ(remove-dir temp-dir)])
      (remove-dir temp-dir)
      (create-dir temp-dir)
      (for ([(filename content) (in-hash content)])
        (let ([path (format "~a./~a.adoc" temp-dir filename)])
          (display-to-file content path)))

      (let* ([blocks (extract-block temp-dir)]
             [code-block (blocks 'code)]
             [file-block (blocks 'file)])
        (check-equal? code-block expected-code-blocks)
        (check-equal? file-block expected-file-blocks))
      (remove-dir temp-dir))))


;; (define (run #:from [from "src"]
;;              #:to   [to   "generated-src"])
;;   (display-command "generate-src")
;;   (~> (extract-blocks from)
;;     (include-file-blocks)
;;     (write-blocks-to-files to)))


;; TO-BE-IMPLEMENTED
