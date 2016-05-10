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

(defpackage #:ulquikit-cmd
  (:use :cl :command-core :cl-ppcre :ulqui/utils))

(in-package #:ulquikit-cmd)

(defun main (argv)
  (let* ((arg&opts (handler-case (parse-cmd-args (rest argv))
                     (simple-error (err)
                       (format *error-output* "~A~%~%" err)
                       (general-usage)
                       (uiop:quit 1))))
         (cmds (alist-get arg&opts :arguments))
         (opts (alexandria:alist-plist (alist-get arg&opts :options))))
    (or (maybe-usage-or-version cmds opts)
        (maybe-help cmds opts)
        (run-cmd (first cmds) (append (rest cmds) opts)))))

(defun maybe-usage-or-version (cmds opts)
  "Shows help or displays Ulquikit version.  This function returns `:end' if
it's supposed to tell the caller to not further process `cmds' and `opts' and
returns `nil' otherwise."
  (if (null cmds)
      (prog1
          :end
        (cond
         ;; ulqui, or
         ;; ulqui --help ((or (null opts)
         ((equal '(:help t) opts)
          (general-usage))

         ;; ulqui --version
         ((equal '(:version t) (member :version opts))
          (run-cmd "version" (list)))

          ;; ulqui --help <command>
         ((= 2 (length (member :help opts)))
          (run-cmd "help" opts))

         (t
          (format *error-output*
                  "Invalid options ~{~A~^ ~}~%~%"
                  (append cmds opts))
          (help)
           (uiop:quit 1))))
    nil))

(defun maybe-help (cmds opts)
  "Shows help for a command when options contains \"--help\" (or `opts'
contains `:help').  The function returns `:end' to tell its caller to stop
further processing and `nil' otherwise."
  (when (equal '(:help t) opts)
    (run-cmd "help" cmds)
    :end))

