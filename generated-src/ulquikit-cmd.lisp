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

;; TODO: refactor
(defun main (argv)
  (let* ((arg&opts (parse-cmd-args (rest argv)))
         (cmds (alist-get arg&opts :arguments))
         (opts (alexandria:alist-plist (alist-get arg&opts :options))))
    (cond
      ((null cmds)
       (help))

      ((string= "help" (first cmds))
       (help (second cmds)))

      ((member :help opts)
       (help (first cmds)))

      (t
       (run-cmd (first cmds) opts)))))

