;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/12/03

(defpackage #:cl-plot
  (:use :cl :asdf))

(in-package :cl-plot)

(asdf:defsystem cl-plot
  :name    "cl-plot"
  :version "0.1"
  :author  "Martin Kersner, <m.kersner@gmail.com>"
  :long-description "Common Lisp interface for gnuplot"
  :serial t
  :components ((:file "utils")
               (:file "plot")))
