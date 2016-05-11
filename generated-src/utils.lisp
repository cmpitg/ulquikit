;;
;; This file is part of Ulquikit project.
;;
;; Copyright (C) 2014-2016 Ha-Duong Nguyen <cmpitg AT gmailDOTcom>
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

(defpackage #:ulqui/utils
  (:use :cl
        :alexandria
        :iterate
        :trivial-utf-8
        :cl-ppcre
        :split-sequence)
  (:export #:join-lines
           #:->keyword
           #:->string
           #:sethash
           #:read-file
           #:take-while
           #:drop-while
           #:full-path
           #:file?
           #:write-file
           #:function-name
           #:alist-get
           #:string-butlast))

(defpackage #:ulqui/utils-tests
  (:use :cl
        :lisp-unit
        :ulqui/utils)
  (:import-from :alexandria :when-let))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:ulqui/utils)

(defun join-lines (lines)
  "Joins a list of strings with newline as separator."
  (declare (list lines))
  (the string (format nil "~{~A~^~%~}" lines)))

(in-package #:ulqui/utils-tests)

(define-test test-join-lines
  (assert-equal "" (join-lines '()))
  (assert-equal "a" (join-lines '("a")))
  (assert-equal (format nil "a~%b") (join-lines '("a" "b")))
  (assert-equal (format nil "a~%b~%") (join-lines '("a" "b" ""))))

;; (run-tests '(test-join-lines))

(in-package #:ulqui/utils)

(defun ->keyword (val)
  "Converts a symbol or string into keyword."
  (declare ((or symbol string) val))
  (the keyword (etypecase val
                 (keyword val)
                 (symbol (intern (string-upcase (symbol-name val)) 'keyword))
                 (string (intern (string-upcase val) 'keyword)))))

(defun ->string (val)
  "Converts a symbol or keyword into string."
  (declare ((or symbol string) val))
  (the string (etypecase val
                (string val)
                ((or symbol keyword) (string-downcase (symbol-name val))))))

(in-package #:ulqui/utils)

(defun sethash (key hash value &rest args)
  "Conveniently combining `setf' and `gethash'.

`\(setf \(gethash o hash\) obj\)' ⬄ `\(sethash o hash obj\)'
`\(setf \(gethash o hash\) obj
      \(gethash a hash\) abj\)'
⬄
`\(sethash o hash obj
         a hash abj\)'
"
  (declare (hash-table hash))
  (unless (zerop (mod (length args) 3))
    (error "number of arguments must be divisible by 3."))

  (setf (gethash key hash) value)

  (unless (null args)
    (apply #'sethash (first args) (second args) (third args) (subseq args 3))))

(in-package #:ulqui/utils-tests)

(define-test test-macro-sethash
  (let ((hash (make-hash-table)))
    (sethash :first hash "hello")
    (sethash :second hash "world")
    (assert-equal "hello" (gethash :first hash))
    (assert-equal "world" (gethash :second hash))))

;; (run-tests '(test-macro-sethash))

(in-package #:ulqui/utils)

(defun file? (path)
  "Determines if path represents a file."
  (declare ((or string pathname) path))
  (the boolean
       (null (scan "/$" (namestring path)))))

(in-package #:ulqui/utils-tests)

(define-test test-file?
  (assert-true  (file? "tmp.txt"))
  (assert-true  (file? #p"tmp.txt"))
  (assert-false (file? "tmp.txt/"))
  (assert-false (file? #p"tmp.txt/")))

;; (run-tests '(test-file?))

(in-package #:ulqui/utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-file (path)
    "Reads a file as UTF-8 encoded string."
    (declare ((or string pathname) path))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (read-utf-8-string in :stop-at-eof t))))

(in-package #:ulqui/utils)

(defun write-file (path content)
  "Writes to a file."
  (declare ((or pathname string) path)
           (string content))
  (with-output-to-file (out path :if-exists :supersede)
    (format out "~A" content)))

(in-package #:ulqui/utils)

(defun take-while (fn xs)
  "Takes each `item' of `xs' from the beginning and builds a list until
`\(funcall fn item\)' returns `nil'.

E.g.

  \(take-while #'oddp '\(\)\)          ;; ⇨ '\(\)
  \(take-while #'oddp '\(2 3 4\)\)     ;; ⇨ '\(\)
  \(take-while #'evenp '\(2 2 4\)\)    ;; ⇨ '\(2 2 4\)
  \(take-while #'evenp '\(2 2 1 3\)\)  ;; ⇨ '\(2 2\)"
  (declare (function fn) (sequence xs))
  (the list (iterate (for x in xs)
                     (while (funcall fn x))
                     (collect x))))

(in-package #:ulqui/utils-tests)

(define-test test-take-while
  (assert-equal '()      (take-while #'oddp '()))
  (assert-equal '()      (take-while #'oddp '(2 3 4)))
  (assert-equal '(2 2 4) (take-while #'evenp '(2 2 4)))
  (assert-equal '(2 2)   (take-while #'evenp '(2 2 1 3))))

(in-package #:ulqui/utils)

(defun drop-while (fn xs)
  "Starting from the first item of `xs' that `\(funcall fn item\)' returns
`nil', builds a list with the rest of `xs'.

E.g.

  \(drop-while #'oddp '\(\)\)            ;; ⇨ '\(\)
  \(drop-while #'oddp '\(1 1 5 7\)\)     ;; ⇨ '\(\)
  \(drop-while #'oddp '\(1 2 5 2 3 4\)\) ;; ⇨ '\(2 3 4\)
  \(drop-while #'oddp '\(2 3 4\)\)       ;; ⇨ '\(2 3 4\)"
  (declare (function fn) (sequence xs))
  (the list (labels ((helper (xs)
                       (cond ((null xs)
                              (list))
                             ((not (funcall fn (first xs)))
                              xs)
                             (t
                              (helper (rest xs))))))
              (helper xs))))

(in-package #:ulqui/utils-tests)

(define-test test-drop-while
  (assert-equal '()      (drop-while #'oddp '()))
  (assert-equal '()      (drop-while #'oddp '(1 1 5 7)))
  (assert-equal '(2 3 4) (drop-while #'oddp '(1 1 5 2 3 4)))
  (assert-equal '(2 3 4) (drop-while #'oddp '(2 3 4))))

(in-package #:ulqui/utils)

(defun full-path (path)
  "Returns absolute path."
  (declare ((or string pathname) path))
  (uiop:merge-pathnames* path (cl-cwd:cwd)))

(in-package #:ulqui/utils)

(defun function-name (fsymbol)
  "Returns the name of a corresponding function as string."
  (declare (function fsymbol))
  (let ((name-tmp (nth 1 (split-sequence #\Space
                                         (format nil "~(~A~)" fsymbol)))))
    (string-butlast name-tmp)))

(in-package #:ulqui/utils-tests)

(define-test test-function-name
  (assert-equal "format"        (function-name #'format))
  (assert-equal "function-name" (function-name #'function-name)))

(in-package #:ulqui/utils)

(defun string-butlast (str)
  "Returns the string without the last character.  If the string is
zero-length, returns an empty string."
  (declare (string str))
  (the string
       (let ((length (length str)))
         (if (zerop length)
             ""
           (subseq str 0 (1- length))))))

(in-package #:ulqui/utils-tests)

(define-test test-string-butlast
  (assert-equal ""  (string-butlast ""))
  (assert-equal ""  (string-butlast "a"))
  (assert-equal "a" (string-butlast "ab")))

(in-package #:ulqui/utils)

(defun alist-get (alist key &key (test 'eql))
  "Returns corresponding value for a key, could be used with `setf'.  This
function aliases `alexandria:assoc-value'."
  (alexandria:assoc-value alist key :test test))

(in-package #:ulqui/utils-tests)

(define-test test-alist-get
  (assert-equal "a" (alist-get '((:a . "a")) :a))
  (assert-equal :a  (alist-get '((:a . :a)) :a))
  (assert-equal nil (alist-get '((:a . :a)) :b)))
