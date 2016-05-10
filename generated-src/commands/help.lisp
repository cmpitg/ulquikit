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

(in-package #:ulquikit-cmd)

(defcmd help (&optional cmd)
  (if cmd
      (run-help cmd :ulquikit-cmd)
    (general-usage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:ulquikit-cmd)

(defun general-usage ()
  "Shows Ulquikit's general usage."
  (labels ((format-cmd (cmd&desc)
             (format nil
                     "~15A :: ~A"
                     (alist-get cmd&desc :name)
                     (string-butlast (alist-get cmd&desc :description))))

           (help-cmd? (cmd&desc)
             (string= (alist-get cmd&desc :name)
                      "help")))

    (let* ((help-template (documentation 'ulquikit-cmd::help 'function))
           (help-stripped (remove-if #'help-cmd?
                                     ulquikit-cmd::*command-list*))
           (cmd-descriptions
            (format nil "~{~A~^~%~}"
                    (mapcar #'format-cmd help-stripped)))
           (help-text (regex-replace "{{ commands-docs }}"
                                     help-template
                                     cmd-descriptions)))
      (format t help-text))))
