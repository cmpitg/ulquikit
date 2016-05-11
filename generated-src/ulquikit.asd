(defsystem ulquikit
  :name "Ulquikit"
  :version "2.0.0"
  :maintainer "Ha-Duong Nguyen <cmpitg@gmail.com>"
  :license "GNU General Public license v3"
  :description "Ulquikit is a hackable tool to make literate programming fun and productive again."
  :depends-on (#:alexandria
               #:uiop
               #:split-sequence
               #:cl-ppcre
               #:bordeaux-threads
               #:trivial-utf-8
               #:iterate
               #:lisp-unit)
  :components ((:file "ulquikit" :depends-on ("utils"))
               (:file "ulquikit-cmd" :depends-on ("utils" "command-core" "ulquikit"))
               (:file "commands/generate-src" :depends-on ("utils" "command-core"))
               (:file "commands/generate-html" :depends-on ("utils" "command-core"))
               (:file "commands/version" :depends-on ("utils" "command-core"))
               (:file "commands/help" :depends-on ("utils" "command-core"))
               (:file "command-core" :depends-on ("utils"))
               (:file "utils")))
