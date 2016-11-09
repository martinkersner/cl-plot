;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/11/09
;;;;
;;;; Example of plotting data using GNU plot.

(load "plot")

(defparameter *dataframe* 
  '((1  1)
    (1  3)
    (10 1)
    (1  4)
    (11 -4)
    (1  34)))

(defparameter *fig* (make-instance 'figure))
;(xlabel *fig* "Deflection (meters)")
(scatter *fig* *dataframe*)
(arrow   *fig* 0 0 10 10 "nohead")
(xlabel  *fig* "text")
(show    *fig*)
