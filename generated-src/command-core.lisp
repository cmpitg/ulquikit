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

(in-package #:cl)

(defpackage #:command-core
  (:use :cl :ulqui/utils :iterate :trivial-utf-8)
  (:export #:parse-cmd-args
           #:argument?
           #:option?
           #:option->keyword
           #:defcmd
           #:run-cmd
           #:display-cmd
           #:help))

(defpackage #:command-core-tests
  (:use :cl :lisp-unit))

(setf lisp-unit:*print-failures* t
      lisp-unit:*print-errors*   t)


(in-package #:command-core)

(defun run-cmd (cmd args)
  "Runs command with appropriate arguments by calling the corresponding
function (that shares the same name as the command) in `ulquikit-cmd'
package."
  (declare ((or string symbol package) cmd)
           (list args))
  (let ((func (get-function cmd :ulquikit-cmd)))
    (apply func args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:command-core)

(defun get-function (func &optional (package *package*))
  "Retrieves a function from a package or throws error if not found."
  (declare ((or string symbol function) func)
           ((or string symbol package) package))
  (let* ((package (find-package (typecase package
                                  (string (string-upcase package))
                                  (otherwise package))))
         (func (typecase func
                 (string    (fdefinition
                             (find-symbol (string-upcase func)
                                          package)))
                 (symbol    (fdefinition
                             (find-symbol (symbol-name func)
                                          package)))
                 (otherwise func))))
    func))

(in-package #:command-core-tests)

(define-test test-get-function
  (assert-equal #'command-core:run-cmd (command-core::get-function "RUN-CMD" "COMMAND-CORE"))
  (assert-equal #'command-core:run-cmd (command-core::get-function 'run-cmd "COMMAND-CORE"))
  (assert-equal #'command-core:run-cmd (command-core::get-function :run-cmd :command-core))
  (assert-error 'undefined-function (command-core::get-function :doesnt-exist :command-core)))

;; (run-tests '(test-get-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:command-core)

(defun help (cmd &optional (package *package*))
  "Reads and returns the help of a command, which is the documentation string
of the corresponding function."
  (declare ((or string symbol) cmd)
           ((or string symbol package) package))
  (let* ((cmd-package (find-package package))
         (run-func    (intern (string-upcase cmd) cmd-package)))
    (format t "~A~%" (documentation run-func 'function))))

(in-package #:command-core)

(defun parse-cmd-args (args)
  (let ((arguments (take-while #'argument? args))
        (rest-args (drop-while #'argument? args)))
    (labels ((parse-options (rest-args current-opts)
               (declare (list rest-args current-opts))
               (if (null rest-args)

                   ;; No more option to parse
                   current-opts

                 (let* ((option-name   (first rest-args))
                        (option-values (take-while #'argument? (rest rest-args)))
                        (rest-args     (drop-while #'argument? (rest rest-args)))

                        (option-values/converted (mapcar #'try-convert-value option-values))
                        (option-name/keyword     (option->keyword option-name))

                        (option-values/res
                         (cond
                           ((null option-values)         ;; --help → (:help . t)
                            t)

                           ((= 1 (length option-values)) ;; --help a → (:help . "a")
                            (first option-values/converted)) ;

                           (t                            ;; --help a b → (:help . ("a" "b"))
                            option-values/converted)))

                        (new-option (cons option-name/keyword option-values/res)))
                   (parse-options rest-args
                                  (push new-option
                                        current-opts))))))
      `((:arguments . ,arguments)
        (:options   . ,(parse-options rest-args (list)))))))

(in-package #:command-core-tests)

(define-test test-parse-cmd-args
  (assert-equal `((:arguments . ())
                  (:options   . ()))
                (parse-cmd-args '()))
  (assert-equal `((:arguments . ("hello-world"))
                  (:options   . ()))
                (parse-cmd-args '("hello-world")))
  (assert-equal `((:arguments . ("hello" "world"))
                  (:options   . ()))
                (parse-cmd-args '("hello" "world")))
  (assert-equal `((:arguments . ())
                  (:options   . ((:help . t))))
                (parse-cmd-args '("--help")))
  (assert-equal `((:arguments . ("hello"))
                  (:options   . ((:help . t))))
                (parse-cmd-args '("hello" "--help")))
  (assert-equal `((:arguments . ("hello"))
                  (:options   . ((:help . ("world" "args")))))
                (parse-cmd-args '("hello" "--help" "world" "args")))
  (assert-equal `((:arguments . ())
                  (:options   . ((:help . "hello"))))
                (parse-cmd-args '("--help" "hello")))
  (assert-equal `((:arguments . ("hello" "world"))
                  (:options   . ((:set-tab . 4)
                                 (:help    . t))))
                (parse-cmd-args '("hello" "world" "--help" "--set-tab" "4"))))

;; (run-tests '(test-parse-cmd-args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:command-core)

(defun argument? (str)
  "Determines if a string is considered an argument.  An argument is not
prefixed with a dash \"-\"."
  (declare (string str))
  (the boolean (not (alexandria:starts-with #\- str))))

(in-package #:command-core-tests)

(define-test test-argument?
  (assert-equal t   (argument? ""))
  (assert-equal t   (argument? "a"))
  (assert-equal nil (argument? "-a"))
  (assert-equal nil (argument? "--a"))
  (assert-equal nil (argument? "-")))

;; (run-tests '(test-argument?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:command-core)

(defun option? (str)
  "Determines if a string is considered an option.  An option is prefixed with
a dash \"-\"."
  (declare (string str))
  (the boolean (not (argument? str))))

(in-package #:command-core-tests)

(define-test test-option?
  (assert-equal nil (option? ""))
  (assert-equal nil (option? "a"))
  (assert-equal t   (option? "-a"))
  (assert-equal t   (option? "--a"))
  (assert-equal t   (option? "-")))

;; (run-tests '(test-option?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:command-core)

(defun option->keyword (opt)
  "Converts option as string to Common Lisp keyword."
  (declare ((or symbol string) opt))
  (the keyword
       (typecase opt
         (keyword opt)
         (symbol  (intern (symbol-name opt) :keyword))
         (string  (multiple-value-bind (_ xs)
                      (cl-ppcre:scan-to-strings "^-+(.+)$" opt)
                    (declare (ignore _))
                    (if (zerop (length xs))
                        (error "~S is not a valid option" xs)
                        (intern (string-upcase (aref xs 0)) :keyword)))))))

(in-package #:command-core-tests)

(define-test test-option->keyword
  (assert-equal :h       (option->keyword "-h"))
  (assert-equal :help    (option->keyword "--help"))
  (assert-equal :help    (option->keyword "---help"))
  (assert-equal :help-me (option->keyword "---help-me")))

;; (run-tests '(test-option->keyword))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:command-core)

(defun try-convert-value (value)
  "Tries converting a string value as number, boolean, or returns itself."
  (declare (string value))
  (cond ((string-equal "true" value) t)
        ((string-equal "false" value) nil)
        ((string-equal "t" value) "t")
        ((string-equal "nil" value) "nil")
        (t (let ((res (read-from-string value)))
             (if (numberp res)
                 res
               value)))))

(in-package #:command-core-tests)

(define-test test-try-convert-value
  (assert-equal 1    (command-core::try-convert-value "1"))
  (assert-equal "a"  (command-core::try-convert-value "a"))
  (assert-equal t    (command-core::try-convert-value "true"))
  (assert-equal t    (command-core::try-convert-value "true"))
  (assert-equal nil  (command-core::try-convert-value "false")))

;; (run-tests '(test-try-convert-value))


(in-package #:command-core)

(defmacro defcmd (name list-args &rest body)
  (let* ((command-core-path (or *load-pathname* *compile-file-pathname*))
         (help-file (uiop:merge-pathnames* (format nil
                                                   "~A.help.txt"
                                                   (string-downcase name))
                                           command-core-path))
         (help-content (read-file help-file))
         (command-list-symb (intern "*COMMAND-LIST*" *package*)))
    `(progn (defun ,name ,list-args
              ,help-content
              ,@body)

            ;; Add command to the command list of the current package
            (defvar ,command-list-symb '())
            (push (function ,name) ,command-list-symb))))

(in-package #:command-core)

(defun display-cmd (msg &optional (stream t))
  "Nicely formats and displays a command."
  (declare (string msg)
           ((or stream boolean) stream))
  (format stream "==== ~A ====~%" msg))
