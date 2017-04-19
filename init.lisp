;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2017/04/19 

(push (namestring (ext:cd)) asdf:*central-registry*)
(asdf:load-system :cl-plot)
