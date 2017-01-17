;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/11/09
;;;;
;;;; Example of plotting data using GNU plot.

(in-package :lispplot)

(defparameter *dataframe* 
  '((1  1)
    (1  3)
    (10 1)
    (1  4)
    (11 -4)
    (1  34)))

(defparameter *fig* (make-instance 'figure))
(xlabel  *fig* "text")
(arrow   *fig* 0 0 10 10 :nohead t)
;(scatter *fig* *dataframe* :cols '(1 2))
(scatter *fig* *dataframe*)
(show    *fig*)
