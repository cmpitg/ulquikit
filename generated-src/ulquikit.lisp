;;
;; This file is part of Ulquikit project.
;;
;; Copyright (C) 2014-2015 Ha-Duong Nguyen <cmpitg AT gmailDOTcom>
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

(defpackage #:ulquikit
  (:use :cl
        :alexandria
        :split-sequence
        :cl-ppcre
        :cl-cwd
        :iterate
        :ulqui/utils)
  (:export *ulquikit-version*

           #:generate-src
           #:generate-html

           #:snippet->string
           #:sethash
           #:file?

           #:snippet
           #:create-snippet
           #:extract-snippets
           #:extract-snippets-from-file
           #:collect-snippet
           #:list-asciidocs))

(defpackage #:ulquikit-tests
  (:use :cl :ulquikit :cl-ppcre :lisp-unit :iterate))

(in-package #:ulquikit-tests)

;; Print failure details by default
(setf *print-failures* t)

(in-package #:ulquikit)

(defvar *ulquikit-version* "2.0.0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:ulquikit)

(defstruct (snippet (:conc-name snippet/))
  (type :code     :type keyword)
  (name ""        :type string)
  (linenum 0      :type integer)
  (lines (list)   :type list)
  (processed? nil :type boolean))


(in-package #:ulquikit)

(defstruct (snippets
             (:conc-name snippets/))
  (file (make-hash-table :test #'equal) :type hash-table)
  (code (make-hash-table :test #'equal) :type hash-table))

(in-package #:ulquikit)

(defun create-snippet (&key (type :code)
                         (name "")
                         (linenum 1)
                         (lines (list))
                         (processed? nil))
  "Helper to create snippet."
  (declare ((or string symbol) type name)
           ((or string list) lines)
           (integer linenum)
           (boolean processed?))
  (let ((type (->keyword type))
        (name (->string name))
        (lines (if (stringp lines)
                   (split-sequence #\Newline lines)
                 lines)))
    (the snippet (make-snippet :type type
                               :name name
                               :linenum linenum
                               :lines lines
                               :processed? processed?))))

(in-package #:ulquikit-tests)

(define-test test-snippet-creation
  (assert-equalp (make-snippet :type :file
                               :name "hello-world"
                               :linenum 10
                               :lines ("Hmm")
                               :processed? nil)
                 (create-snippet :type :file
                                 :name 'hello-world
                                 :linenum 10
                                 :lines '("Hmm")))
  (assert-equalp (make-snippet :type :string
                               :name "string"
                               :linenum 100
                               :lines ("string")
                               :processed? t)
                 (create-snippet :type "string"
                                 :name "string"
                                 :linenum 100
                                 :lines "string"
                                 :processed? t)))


(in-package #:ulquikit)

(defun get-snippet-content (snippet)
  "Returns the content of a snippet as string."
  (declare (snippet snippet))
  (join-lines (snippet/lines snippet)))


(in-package #:ulquikit)

(defun extract-snippets (path &key (recursive t))
  "Extracts all snippets from all AsciiDoc directory in `path'.  The AsciiDoc
files are searched recursively or non-recursively depending on `recursive'."
  (declare ((or string pathname) path))
  (labels ((merge-snippets (current-snippets adoc-file)
             (declare (snippets current-snippets)
                      ((or string pathname) adoc-file))
             (let ((new-snippets (extract-snippets-from-file adoc-file)))
               ;; Merging 2 snippets
               (maphash #'(lambda (key value)
                            (sethash key
                                     (snippets/file current-snippets)
                                     value))
                        (snippets/file new-snippets))
               (maphash #'(lambda (key value)
                            (sethash key
                                     (snippets/code current-snippets)
                                     value))
                        (snippets/code new-snippets))
               (the snippets current-snippets))))
    (reduce #'merge-snippets
            (list-asciidocs path :recursive recursive)
            :initial-value (make-snippets))))

(in-package #:ulquikit-tests)

(define-test test-extract-snippets
  (let* ((test-dir (uiop:merge-pathnames*
                    "ulquikit/test-extract-snippets/"
                    (uiop:ensure-directory-pathname (uiop:getenv "TMPDIR"))))

         (content `(("Main.adoc" . "= A sample program

This program consists of several snippets and a hello

== Main program

The main program includes function `say-hello` from `lib/Say-Hello.adoc` and
function `say-world` from `lib/Say-World.adoc` and calls them.

.file::/tmp/main.lisp
\----
;; include::say-hello

;; include::say-world

\(say-hello\)
\(say-world\)

\----
")
                    ("License" . "Do what you want to do with it!")
                    ("lib/Say-Hello.adoc" . "What do you actually expect in this
file?  Two snippets, one of which doesn't get captured.

.code::say-hello
[source,lisp,linenums]
\----
\(defun say-hello \(\)
  \(format t \"Hello \"\)\)
\----

The following snippet doesn't get captured as it has no title:

[source,lisp,linenums]
\----
\(defun throw-away \(\)
  \(error \"If you see me, there is at least one error happened!\"\)\)
\----
")
                    ("lib/Say-World.adoc" . "Another way to define code block title with AsciiDoc:

[source,lisp,linenums]
.code::say-world
\----
\(defun say-world \(\)
  \(format t \"world!~%\"\)\)
\----
")))
         (files (mapcar #'(lambda (content-pair)
                            (cons (uiop:merge-pathnames* (car content-pair) test-dir)
                                  (cdr content-pair)))
                        content)))
    (uiop:delete-directory-tree test-dir :if-does-not-exist :ignore)
    (format t "Test dir: ~A~%" test-dir)

    (dolist (path+content files)
      (let ((path    (car path+content))
            (content (cdr path+content)))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output)
          (princ content out))))

    (let* ((snippets (ulquikit::extract-snippets test-dir))
           (file-snippets (ulquikit::snippets/file snippets))
           (code-snippets (ulquikit::snippets/code snippets)))
      (assert-equal 1 (hash-table-count file-snippets))
      (assert-equal 2 (hash-table-count code-snippets))

      (assert-equal ";; include::say-hello

;; include::say-world

\(say-hello\)
\(say-world\)
"
                    (snippet->string (gethash "/tmp/main.lisp" file-snippets)))
      (assert-equal "\(defun say-hello \(\)
  \(format t \"Hello \"\)\)"
                    (snippet->string (gethash "say-hello" code-snippets)))
      (assert-equal "\(defun say-world \(\)
  \(format t \"world!~%\"\)\)"
                    (snippet->string (gethash "say-world" code-snippets))))))

;; (run-tests '(test-extract-snippets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:ulquikit)

(defun snippet->string (snippet)
  "Returns the string representation of a snippet."
  (declare (snippet snippet))
  (get-snippet-content snippet))

;;; (snippet->string (make-snippet))

(in-package #:ulquikit-tests)

(define-test test-snippet->string
  (assert-equal "" (snippet->string (make-snippet)))
  (assert-equal "aoeu" (snippet->string (make-snippet :lines '("aoeu"))))
  (assert-equal "aoeu
ueoa"
                (snippet->string (make-snippet :lines '("aoeu" "ueoa")))))

;; (run-tests '(test-snippet->string))


(in-package #:ulquikit)

(defun extract-snippets-from-file (path)
  "Extracts snippets from a file and return a `snippets' struct."
  (declare ((or string pathname) path))
  (let* ((text (string-trim '(#\Space #\Newline #\e #\t #\m) (read-file path)))
         (lines (split-sequence #\Newline text))

         (snippets (make-snippets))

         (prev-prev-line "")
         (prev-line      "")
         (linenum        0)             ; current line number
         (inside?        nil)           ; currently inside a snippet?

         (s/type       :code)
         (s/lines/rev  (list))
         (s/name       "")
         (s/linenum    0))
    (dolist (line lines)
      (incf linenum)

      ;; (format t "~A |> ~A~%" linenum line)
      ;; (format t "   |> block? ~A~%" (block-delimiter? line))

      (cond ((and inside? (not (block-delimiter? line)))

             (push line s/lines/rev))

            ((and inside? (block-delimiter? line))

             ;; Close the current snippet
             (setf inside?  nil
                   snippets (collect-snippet snippets
                                             (create-snippet
                                              :type s/type
                                              :name s/name
                                              :lines (nreverse s/lines/rev)
                                              :linenum s/linenum))))

            ((and (not inside?) (block-delimiter? line))
             ;; (format t "  found snippet > num: ~A~%" linenum)

             (when-let (title (cond ((block-title? prev-line) prev-line)
                                    ((block-title? prev-prev-line) prev-prev-line)
                                    (t nil)))
               (multiple-value-bind (type name) (parse-snippet-title title)
                 (setf inside?     t
                       s/type      type
                       s/name      name
                       s/lines/rev (list)
                       s/linenum   (1- linenum))))))

      ;; Update previous lines
      (unless (zerop (length (string-trim '(#\Space #\Newline #\e #\t #\m) line)))
        (setf prev-prev-line prev-line
              prev-line      line)))

    ;; (list linenum (length lines) snippets)
    snippets))

;; (extract-snippets-from-file "/m/src/ulquikit/src/Ulquikit.adoc")
;; (time (extract-snippets-from-file "/m/src/ulquikit/src/Ulquikit.adoc"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:ulquikit)

(defun block-delimiter? (str)
  "Determines if a string is a block delimiter.  TODO: Make this extensible."
  (declare (string str))
  (scan "^----( *)$" str))

(in-package #:ulquikit-tests)

(define-test test-block-delimiter
  (assert-true (ulquikit::block-delimiter? "----"))
  (assert-true (not (ulquikit::block-delimiter? " ----")))
  (assert-true (ulquikit::block-delimiter? "---- "))
  (assert-true (ulquikit::block-delimiter? "----  "))
  (assert-true (not (ulquikit::block-delimiter? "----a"))))

;; (run-tests '(test-block-delimiter))

(in-package #:ulquikit)

(defun block-title? (str)
  "Determines if a string is a block title.  TODO: Make this extensible."
  (declare (string str))
  (scan "^\\.(file|code)::" str))

(in-package #:ulquikit-tests)

(define-test test-block-title
  (assert-true (ulquikit::block-title? ".file::something"))
  (assert-true (ulquikit::block-title? ".file::something else"))
  (assert-true (ulquikit::block-title? ".file::"))
  (assert-true (null (ulquikit::block-title? ".file:something"))))

;; (run-tests '(test-block-title))

(in-package #:ulquikit)

(defun parse-snippet-title (title)
  "Parses a snippet title and returns `(values <snippet-type>
<snippet-name>)'."
  (declare (string title))
  (multiple-value-bind (_ res) (scan-to-strings "\.(file|code)::(.*)" title)
    (declare (ignore _))
    (values (the keyword (->keyword (aref res 0)))
            (the string  (aref res 1)))))

(in-package #:ulquikit-tests)

(define-test test-parse-snippet-title
  (dolist (el '((".file::"    . (:file ""))
                (".code::"    . (:code ""))
                (".file::abc" . (:file "abc"))
                (".code::a b" . (:code "a b"))))
    (let ((title    (first el))
          (expected (rest  el)))
      (multiple-value-bind (type name) (ulquikit::parse-snippet-title title)
        (assert-equal expected (list type name))))))

;; (run-tests '(test-parse-snippet-title))

(in-package #:ulquikit)

(defun collect-snippet (snippets snippet)
  "Collects `snippet' into the list of snippets."
  (declare (snippets snippets)
           (snippet  snippet))
  (let* ((type (snippet/type snippet))
         (name (snippet/name snippet))
         (current-file (snippets/file snippets))
         (current-code (snippets/code snippets))
         (file (case type
                 (:file     (sethash name current-file snippet)
                            current-file)
                 (otherwise current-file)))
         (code (case type
                 (:code     (sethash name current-code snippet)
                            current-code)
                 (otherwise current-code))))
    (the snippets (make-snippets :file file
                                 :code code))))

(in-package #:ulquikit-tests)

(define-test test-collect-snippets
  (assert-equalp (collect-snippet (ulquikit::make-snippets)
                                  (create-snippet :type :file
                                                  :name :hello
                                                  :linenum 10
                                                  :lines '("Something")))
                 (ulquikit::make-snippets
                  :file (alexandria:alist-hash-table `(("hello" . ,(ulquikit::make-snippet
                                                                    :type :file
                                                                    :name "hello"
                                                                    :linenum 10
                                                                    :lines ("Something")
                                                                    :processed? nil)))
                                                     :test #'equal)
                  :code (make-hash-table :test #'equal)))

  (assert-equalp (collect-snippet
                  (ulquikit::make-snippets
                   :file (alexandria:alist-hash-table `(("hello" . ,(ulquikit::make-snippet
                                                                     :type :file
                                                                     :name "hello"
                                                                     :linenum 10
                                                                     :lines ("Something")
                                                                     :processed? nil)))
                                                      :test #'equal)
                   :code (make-hash-table :test #'equal))
                  (create-snippet :type 'code
                                  :name 'say-something
                                  :linenum 100
                                  :lines '("Something else")))
                 (ulquikit::make-snippets :file (alexandria:alist-hash-table
                                                 `(("hello" . ,(ulquikit::make-snippet
                                                                :type :file
                                                                :name "hello"
                                                                :linenum 10
                                                                :lines ("Something")
                                                                :processed? nil)))
                                                 :test #'equal)
                                          :code (alexandria:alist-hash-table
                                                 `(("say-something" . (ulquikit::make-snippet
                                                                       :type :code
                                                                       :name "say-something"
                                                                       :linenum 100
                                                                       :lines ("Something else")
                                                                       :processed? nil)))
                                                 :test #'equal))))

;; (run-tests '(test-collect-snippets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package #:ulquikit)

(defun include-snippet! (snippets type+name)
  "Processes target snippet \(presented by `type+name'\) by replacing all of
its \"include\" directives with the corresponding code snippets found in
`snippets'.  If the target snippet introduces circular dependency, the result
is undefined.

This function modifies `snippets' in-place and returns it after processing."
  (declare (cons type+name) (snippets snippets))

  ;; (format t "→ including snippet ~A~%" type+name)

  ;; Ignore of the target snippet doesn't exist in the list of snippets
  (when (snippet-exists? type+name snippets)
    (let* ((target/type (car type+name))
           (target/name (cdr type+name))
           (target      (snippets/get-snippet snippets
                                              :type target/type
                                              :name target/name))
           (lines       (snippet/lines target))
           (lines-final (list)))
      ;; Also, we ignore if this snippet has already been processed
      (unless (snippet/processed? target)
        ;; Consider this snippet processed
        (setf (snippet/processed? target) t)

        ;; Now, recollect lines
        (dolist (line lines)
          ;; (format t "  Processing ~A~%" line)
          (if (include-directive? line)
              (let* ((includee-name (parse-include-directive line))
                     (includee      (snippets/get-snippet snippets
                                                          :type :code
                                                          :name includee-name)))
                (cond ((null includee)
                       ;; No such snippet to include
                       (push line lines-final))

                      ((snippet/processed? includee)
                       (push (snippet->string includee) lines-final))

                      ((not (snippet-exists? includee snippets))
                       (push line lines-final))

                      (t
                       (setf snippets (include-snippet!
                                       snippets
                                       `(:code . ,includee-name)))
                       (push (snippet->string includee) lines-final))))
            (push line lines-final)))

        ;; (format t "Snippet: ~A; result: ~A~%"
        ;;         (cdr type+name)
        ;;         (join-lines (reverse (copy-list lines-final))))

        ;; Then, collect result
        (setf (snippet/lines target)
              (list (join-lines (nreverse lines-final)))))))
  snippets)

(in-package #:ulquikit-tests)

;; (run-tests '(test-include-snippet))

(define-test test-include-snippet
  (let* ((snp/file (ulquikit::make-snippet :name "/tmp/tmp.lisp"
                                           :type :file
                                           :lines '(";; include::A"
                                                    ";; The end")
                                           :linenum 10))
         (snp/code/A (ulquikit::make-snippet :name "A"
                                             :type :code
                                             :lines '("World"
                                                      ";; include::B"
                                                      ";; include::D")
                                             :linenum 20))
         (snp/code/B (ulquikit::make-snippet :name "B"
                                             :type :code
                                             :lines '("Hello")
                                             :linenum 30))
         (snp/code/C (ulquikit::make-snippet :name "C"
                                             :type :code
                                             :lines '("Not processed")
                                             :linenum 15))
         ;; Circular dependency
         (snp/code/D (ulquikit::make-snippet :name "D"
                                             :type :code
                                             :lines '(";; include A")
                                             :linenum 100))

         (snippets (let* ((res (ulquikit::make-snippets))
                          (ulquikit::snippets/file (ulquikit::snippets/file res))
                          (ulquikit::snippets/code (ulquikit::snippets/code res)))
                     (setf (gethash "/tmp/tmp.lisp" ulquikit::snippets/file) snp/file
                           (gethash "A" ulquikit::snippets/code) snp/code/A
                           (gethash "B" ulquikit::snippets/code) snp/code/B
                           (gethash "C" ulquikit::snippets/code) snp/code/C
                           (gethash "D" ulquikit::snippets/code) snp/code/D)
                     ;; Don't add D right away
                     res)))
    (setf snippets (ulquikit::include-snippet! snippets `(:code . "A")))
    (assert-true t)
    (assert-true (ulquikit::snippet/processed? snp/code/A))
    (assert-true (ulquikit::snippet/processed? snp/code/B))
    (assert-true (ulquikit::snippet/processed? snp/code/D))
    (assert-false (ulquikit::snippet/processed? snp/code/C))
    (assert-false (ulquikit::snippet/processed? snp/file))
    (assert-true (scan "^World\\nHello\\n"
                       (nth 0 (ulquikit::snippet/lines snp/code/A))))

    (setf snippets (ulquikit::include-snippet! snippets `(:file . "/tmp/tmp.lisp")))
    (let ((content (nth 0 (ulquikit::snippet/lines snp/file))))
      (assert-true (ulquikit::snippet/processed? snp/file))
      (assert-true (scan "^World\\nHello\\n" content))
      (assert-true (scan ";; The end$" content)))))

;; (run-tests '(test-include-snippet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:ulquikit)

(defun snippet-exists? (snippet snippets)
  "Determines if the corresponding snippet is in `snippets'."
  (declare ((or snippet string cons) snippet)
           (snippets snippets))
  (typecase snippet
    ((or cons snippet)
     (let ((name (typecase snippet
                   (cons    (cdr snippet))
                   (snippet (snippet/name snippet))))
           (type (typecase snippet
                   (cons    (car snippet))
                   (snippet (snippet/type snippet)))))
       (case type
         (:code (not (null (gethash name (snippets/code snippets)))))
         (:file (not (null (gethash name (snippets/file snippets))))))))
    (string
     (or (not (null (gethash snippet (snippets/code snippets))))
         (not (null (gethash snippet (snippets/file snippets))))))))

(in-package #:ulquikit-tests)

(define-test test-snippet-exists?
  (let* ((code/a (make-snippet :name "a" :type :code))
         (code/b (make-snippet :name "b" :type :code))
         (file/a (make-snippet :name "a" :type :file))
         (file/b (make-snippet :name "b" :type :file))
         (snippets (let ((res (make-snippets)))
                     (sethash "a" (snippets/code res) code/a)
                     (sethash "a" (snippets/file res) file/a)
                     res)))
    (assert-eq t   (ulquikit::snippet-exists? code/a snippets))
    (assert-eq t   (ulquikit::snippet-exists? file/a snippets))
    (assert-eq t   (ulquikit::snippet-exists? "a"    snippets))
    (assert-eq t   (ulquikit::snippet-exists? `(:code . "a") snippets))
    (assert-eq t   (ulquikit::snippet-exists? `(:file . "a") snippets))
    (assert-eq nil (ulquikit::snippet-exists? `(:code . "b") snippets))
    (assert-eq nil (ulquikit::snippet-exists? code/b snippets))
    (assert-eq nil (ulquikit::snippet-exists? file/b snippets))
    (assert-eq nil (ulquikit::snippet-exists? "b"    snippets))))

;; (run-tests '(test-snippet-exists?))

(in-package #:ulquikit)

(defun include-directive? (line)
  "Determines of the corresponding line is a include directive.  TODO: Make this extensible."
  (declare (string line))
  (the boolean
       (let ((line (string-trim '(#\Space #\e #\t #\m) line)))
         (not (null (or (scan "^[#;/-]{2} include::.*" line)
                        (scan "^<!-- include::.* -->" line)
                        (scan "^/\\* include::.* \\*/" line)))))))

(in-package #:ulquikit-tests)

(define-test test-include-directive?
  (assert-eq t   (ulquikit::include-directive? "  ;; include::"))
  (assert-eq t   (ulquikit::include-directive? ";; include::"))
  (assert-eq nil (ulquikit::include-directive? "a;; include::"))
  (assert-eq t   (ulquikit::include-directive? ";; include::something"))
  (assert-eq t   (ulquikit::include-directive? "## include::something"))
  (assert-eq t   (ulquikit::include-directive? "// include::something"))
  (assert-eq t   (ulquikit::include-directive? "/* include::something */"))
  (assert-eq t   (ulquikit::include-directive? "<!-- include::something -->"))
  (assert-eq nil (ulquikit::include-directive? "a <!-- include::something -->")))

;; (run-tests '(test-include-directive?))

(in-package #:ulquikit)

(defun parse-include-directive (str)
  "Parses and extracts snippet name from an include directive.  See its tests
for detailed information on input/output format.  TODO: make this extensible."
  (declare (string str))
  (the
   string
   (if (include-directive? str)
       (let ((input (cond ((and (scan " -->$" str) (scan "^<!-- " str))
                           (subseq str 0 (- (length str) (length " -->"))))
                          ((and (scan " \\*/$" str) (scan "^/\\* " str))
                           (subseq str 0 (- (length str) (length " */"))))
                          (t
                           str))))
         (multiple-value-bind (_ name/array)
             (scan-to-strings "include::(.*)$" input)
           (declare (ignore _))
           (elt name/array 0)))
     "")))

(in-package #:ulquikit-tests)

(define-test test-include-directive
  (assert-equal "" (ulquikit::parse-include-directive "  ;; include::"))
  (assert-equal "" (ulquikit::parse-include-directive ";; include::"))
  (assert-equal "" (ulquikit::parse-include-directive
                    "a <!-- include::something -->"))
  (assert-equal "something" (ulquikit::parse-include-directive
                             ";; include::something"))
  (assert-equal "something" (ulquikit::parse-include-directive
                             "## include::something"))
  (assert-equal "something" (ulquikit::parse-include-directive
                             "// include::something"))
  (assert-equal "something" (ulquikit::parse-include-directive
                             "/* include::something */"))
  (assert-equal "something" (ulquikit::parse-include-directive
                             "<!-- include::something -->")))

;; (run-tests '(test-include-directive))

(in-package #:ulquikit)

(defun snippets/get-snippet (snippets &key
                                        (type :code)
                                        name)
  "Helper to quickly retrieve a snippet from a `snippets' struct."
  (declare (snippets snippets)
           (keyword  type)
           (string   name))
  (let ((hash (case type
                (:code (snippets/code snippets))
                (:file (snippets/file snippets))
                (otherwise (make-hash-table)))))
    (the (or boolean snippet) (gethash name hash))))

(in-package #:ulquikit-tests)

(define-test test-snippets/get-snippet
  (let* ((snp/code/a (make-snippet :type :code :name "a"))
         (snp/code/b (make-snippet :type :code :name "b"))
         (snp/file/c (make-snippet :type :file :name "c.lisp"))
         (snippets (let ((res (make-snippets)))
                     (sethash "a" (snippets/code res) snp/code/a)
                     (sethash "b" (snippets/code res) snp/code/b)
                     (sethash "c" (snippets/file res) snp/file/c)
                     res)))
    (assert-equal snp/code/a (ulquikit::snippets/get-snippet snippets
                                                             :type :code
                                                             :name "a"))
    (assert-equal snp/code/b (ulquikit::snippets/get-snippet snippets
                                                             :type :code
                                                             :name "b"))
    (assert-equal snp/file/c (ulquikit::snippets/get-snippet snippets
                                                             :type :file
                                                             :name "c"))))

;; (run-tests '(test-snippets/get-snippet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; include::include-snippets

(in-package #:ulquikit)

(defun include-file-snippets! (snippets)
  "Includes all file snippets in `snippets' and return a `snippets' with all
file snippets included.  Note that this function is destructive."
  (declare (snippets snippets))
  (let ((file-snippets (snippets/file snippets)))
    (iter (for (name _) in-hashtable file-snippets)
          (include-snippet! snippets `(:file . ,name))))
  snippets)

(in-package #:ulquikit)

(defun render-asciidoc (input output)
  "Renders an ASCIIDoc `input' to `output' as HTML.  TODO: make this
extensible."
  (!cmd (asciidoctor-command input output)))

(defun asciidoctor-command (input output)
  "Builds and returns command to render `input' to `output' with ASCIIDoctor."
  (declare ((or string pathname) input output))
  (the string (format nil "asciidoctor ~A -d book -o ~A" input output)))

(defun !cmd (cmd &key (output t)
                 (error-output t)
                 (force-shell t))
  "Runs command by calling `uiou:run-program'."
  (declare (string cmd)
           (boolean force-shell)
           ((or boolean stream) output error-output))
  (uiop:run-program cmd :output output
                    :error-output error-output
                    :force-shell force-shell))

(in-package #:ulquikit)

(defun list-asciidocs (path &key (recursive t))
  "Returns a list of all ASCIIDoc file (i.e. file with .adoc or .txt
extensions) from directory `path', ignoring all temporary files.  TODO: Make
this extensible."
  (declare ((or string pathname) path))
  (if (uiop:file-exists-p path)
      (list path)
    (let* ((path (uiop:ensure-directory-pathname path))
           (current-asciidocs (remove-if #'(lambda (path)
                                             (not (valid-asciidoc-file? path)))
                                         (uiop:directory-files path))))
      (if recursive
          (apply #'append
                 current-asciidocs
                 (mapcar #'(lambda (subdir)
                             (list-asciidocs subdir :recursive recursive))
                         (uiop:subdirectories path)))
        current-asciidocs))))

(in-package #:ulquikit)

(defun valid-asciidoc-file? (path)
  "Determines if a path refers to an AsciiDoc file based on its extension."
  (let ((path (namestring path)))
    (and (not (uiop:directory-pathname-p path))
         (cl-ppcre:scan "^[^#]" path)
         (cl-ppcre:scan "\\.(adoc|txt|asciidoc)$" path))))

(in-package #:ulquikit-tests)

(define-test test-list-asciidocs
  (let* ((files '("a.adoc"
                  "b.adoc"
                  "c.md"
                  "e.adoc"
                  "hello/a.adoc"
                  "hello/b.html"
                  "hello/world/hola.adoc"
                  "hello/world/mundo.adoc"))
         (temppath (uiop:merge-pathnames* "ulquikit/test-list-asciidocs/"
                                          (pathname (uiop:ensure-directory-pathname
                                                     (uiop:getenv "TMPDIR")))))
         (expected (iterate
                    (for path in files)
                    (when (scan "\\.adoc$" path)
                      (collect (uiop:merge-pathnames* path temppath))))))
    ;; Setup
    (uiop:delete-directory-tree temppath :if-does-not-exist :ignore)
    (dolist (path files)
      (let ((file (uiop:merge-pathnames* path temppath)))
        (ensure-directories-exist (uiop:pathname-directory-pathname file))
        (with-open-file (out file :direction :output
                             :if-exists :supersede)
          (princ "Hello world" out))))

    (let ((adocs (list-asciidocs temppath)))
      (assert-equalp (sort expected #'(lambda (path1 path2)
                                        (string< (namestring path1)
                                                 (namestring path2))))
                     (sort adocs #'(lambda (path1 path2)
                                     (string< (namestring path1)
                                              (namestring path2))))))

    ;; Tear down
    (uiop:delete-directory-tree temppath :if-does-not-exist :ignore)))

;; (run-tests '(test-list-asciidocs))

(in-package #:ulquikit)

(defun generate-src (&key (from "src") (to "generated-src") (recursive t))
  "Generates source code from all literate source files in `from' to directory
`to'.  `from' is either a directory or a single literate source file."
  (declare ((or string pathname) from to))
  (let* ((from     (full-path from))
         (to       (full-path (uiop:ensure-directory-pathname to)))
         (snippets (if (null (directory from))
                       (extract-snippets-from-file from)
                     (extract-snippets from :recursive recursive))))
    ;; (format t "Generating src from: ~A to: ~A~%" from to)
    (write-src-files (include-file-snippets! snippets) to)))

;;; (generate-src :from "src/" :to "/tmp/ulquikit-test/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:ulquikit)

(defun write-src-files (snippets to)
  "Writes all source snippets as files to `to'."
  (declare (snippets snippets)
           ((or string pathname) to))
  (iter (for (name snippet) in-hashtable (snippets/file snippets))
        (let ((path (uiop:merge-pathnames*
                     name
                     (uiop:merge-pathnames*
                      to
                      (uiop:getcwd))))
              (content (snippet->string snippet)))
          (format t "Writing ~A~%" path)
          (ensure-directories-exist path)
          (write-file path content))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package #:ulquikit)

(defun generate-html (&key (from "src")
                        (to "generated-html")
                        (recursive t))
  "Generates HTML documentation from all literate source files in `from' to
directory `to'.  `from' is either a directory or a single literate source
file.  The source file could be searched recursively or non-recursively,
depending on the value of `recursive'.  By default, `recursive' is `t'."
  (let* ((from (full-path from))
         (to   (full-path (uiop:ensure-directory-pathname to)))
         (docs (list-asciidocs from :recursive recursive)))
    (cl-cwd:with-cwd from
      (uiop:ensure-all-directories-exist (list to))
      (mapcar #'bordeaux-threads:join-thread
              (mapcar #'(lambda (doc)
                          ;; uiop:merge-pathnames* actually replaces the
                          ;; extension
                          (let ((output (uiop:merge-pathnames*
                                         (html-namepart doc) to)))
                            (format t "~A → ~A~%" doc output)
                            ;; Run in parallel, better performance,
                            ;; experimental
                            (bordeaux-threads:make-thread
                             #'(lambda () (render-asciidoc doc output)))))
                      docs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:ulquikit)

(defun html-namepart (file)
  "Extracts only the name part of the file and replaces its extension with
HTML."
  (declare ((or string pathname) file))
  (let ((filename-part (file-namestring file)))
    (multiple-value-bind (namepart _)
        (uiop:split-name-type filename-part)
      (declare (ignore _))
      (the string (format nil "~A.html" namepart)))))

(in-package #:ulquikit-tests)

(define-test test-html-namepart
  (assert-equal "hello.html" (html-namepart "/tmp/hello"))
  (assert-equal "hello.html" (html-namepart "/tmp/hello.adoc"))
  (assert-equal "hello.html" (html-namepart "/tmp/hello.txt")))

;; (run-tests '(test-html-namepart))

