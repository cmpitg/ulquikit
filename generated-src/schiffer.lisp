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

(defpackage #:schiffer
  (:use :cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:schiffer)

(defun gen-src ()
  (!cmd "ulqui generate-src --from src/ --to generated-src/ --recursive false"))

(defun gen-docs ()
  (!cmd "ulqui generate-html --from src/ --to docs/ --recursive false"))

(defun gen-all ()
  (gen-src)
  (gen-docs))

(defun build-ulqui ()
  (displayln "⇨ Generating Quicklisp manifest")
  (write-quicklisp-manifest)

  (displayln "⇨ Making sure build/ exists")
  (uiop/common-lisp:ensure-directories-exist (ulqui-dir-to "build/"))

  (displayln "⇨ Building Ulquikit to build/ulqui")
  (buildapp :manifest-file "quicklisp-manifest.txt"
            :systems '("ulquikit")
            :entry "ulquikit-cmd:main"
            :output (ulqui-dir-to "build/ulqui")))

(defun build-schiffer ()
  (displayln "⇨ Generating Quicklisp manifest")
  (write-quicklisp-manifest)

  (displayln "⇨ Making sure build/ exists")
  (uiop/common-lisp:ensure-directories-exist (ulqui-dir-to "build/"))

  (displayln "⇨ Building Schiffer to ./schiffer")
  (asdf:load-system :alexandria)
  (compile-file (ulqui-dir-to "generated-src/schiffer.lisp")
                :output-file (ulqui-dir-to "build/schiffer.fasl"))
  (buildapp :manifest-file "quicklisp-manifest.txt"
            :systems '("alexandria")
            :loads `(,(ulqui-dir-to "build/schiffer.fasl"))
            :entry "schiffer:main"
            :output (ulqui-dir-to "build/schiffer")))

(defun gen-build-ulqui ()
  (gen-src)
  (build-ulqui))

(defun gen-build-all ()
  (gen-build-ulqui)
  (build-schiffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun !cmd (cmd &key (output t)
                   (error-output t)
                   (force-shell t))
  (uiop:run-program cmd :output output
                    :error-output error-output
                    :force-shell force-shell))

(defun buildapp (&key manifest-file entry output systems loads)
  (!cmd (format-str
         (str-space
          "buildapp --manifest-file ~A"
          "--asdf-tree ~A"
          "~:[~;~:*~{--load-system '~A' ~}~]"
          "~:[~;~:*~{--load '~A' ~}~]"
          "--entry ~A"
          "--output '~A'")
         manifest-file
         (ulqui-dir-to "generated-src/")
         systems                  ; Only --load-system if `systems` is not nil
         loads                    ; Only --load if `loads` is not nil
         entry
         output)))

(defun str-space (&rest strs)
  (format nil "~{~A~^ ~}" strs))

(defun format-str (&rest args)
  (apply 'format nil args))

(defun ulqui-dir-to (dir)
  (uiop:merge-pathnames* dir (current-dir)))

(defun current-dir ()
  "Retrieves the current project directory."
  (uiop:merge-pathnames*
   (uiop:pathname-parent-directory-pathname
    (uiop:pathname-directory-pathname (or *compile-file-pathname*
                                          *load-pathname*)))))

(defun write-quicklisp-manifest ()
  (!cmd (format-str
         (str-space
          "sbcl --noinform --no-sysinit --non-interactive"
          "--eval '(pushnew \"~A\" asdf:*central-registry*)'"
          "--eval '(ql:quickload :ulquikit)'"
          "--eval '(ql:write-asdf-manifest-file \"quicklisp-manifest.txt\")'"
          "--eval '(quit)'")
         (ulqui-dir-to "generated-src/"))))

(defun displayln (str)
  (format t "~A~%" str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package #:schiffer)

(defun show-help ()
  (format t "Usage: schiffer <command> [options] ...

Schiffer is a build helper for Ulquikit.

Available commands:

  gen-src         :: Generate Ulquikit and Schiffer source code to 'generated-src/'
  gen-docs        :: Generate Ulquikit HTML docs to 'docs/'
  gen-all         :: Call 'gen-src', then 'gen-docs'
  build-ulqui     :: Build Ulquikit from 'generated-src/' to 'build/ulqui'
  build-schiffer  :: Build Schiffer from 'generated-src/' to 'build/schiffer'

  gen-build-ulqui :: Call 'gen-src', 'build-ulqui'
  gen-build-all   :: Call 'gen-src', 'build-ulqui', then 'build-schiffer'

  run-tests       :: Run all Ulquikit tests in 'generated-src/'
  ulqui-dev       :: Call the development version of Ulquikit: 'build/ulqui'
  clean           :: Remove all generated source, docs, and build

  mark-stable     :: Mark current source code as stable by copying 'generated-src' to 'release/ulquikit/src' and copying 'build/ulqui' to 'release/ulquikit/ulqui' 
  mark-release    :: Prepare for a release by generating MD5 and SHA1 checksums of 'release/ulquikit/ulqui' to 'release/ulquikit/ulqui.md5sum' and 'release/ulquikit/ulqui.sha1sum'

  update-self     :: Update Schiffer by replacing 'schiffer' with 'build/schiffer'
  help            :: Print this help

Only command 'ulqui-dev' takes extra arguments.
"))


(in-package #:schiffer)

(defun update-self ()
  (displayln "⇨ Updating schiffer, replacing itself with the development version at build/schiffer")
  (uiop:copy-file (ulqui-dir-to "build/schiffer")
                  (ulqui-dir-to "schiffer")))


(in-package #:schiffer)

(defun clean ()
  (displayln "⇨ Removing docs/ generated-src/ build/")
  (!cmd (format-str "rm -rf '~A' '~A' '~A'"
                    (ulqui-dir-to "docs/")
                    (ulqui-dir-to "generated-src/")
                    (ulqui-dir-to "build/"))))


(in-package #:schiffer)

(defun main (argv)
  (when (= 1 (length argv))
    (show-help)
    (uiop:quit 0))

  (let* ((cmd (first (rest argv)))
         (args (rest (rest argv))))
    (alexandria:switch (cmd :test #'string=)
      ("help"            (show-help))

      ("gen-src"         (apply #'gen-src args))
      ("gen-docs"        (apply #'gen-docs args))
      ("gen-all"         (apply #'gen-all args))

      ("build-ulqui"     (apply #'build-ulqui args))
      ("build-schiffer"  (apply #'build-schiffer args))

      ("gen-build-ulqui" (apply #'gen-build-ulqui args))
      ("gen-build-all"   (apply #'gen-build-all args))

      ("update-self"     (apply #'update-self args))
      ("clean"           (apply #'clean args)))))
