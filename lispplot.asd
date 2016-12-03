;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/12/03

(asdf:defsystem :lispplot
  :name    "lispplot"
  :version "0.0.1"
  :author  "Martin Kersner, <m.kersner@gmail.com>"
  :long-description "Common Lisp API for GNU plot"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "plot")))
