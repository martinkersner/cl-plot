;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/11/09
;;;;
;;;; Example of plotting data using GNU plot.

(defparameter *dataframe* 
  '((1  1)
    (1  3)
    (10 1)
    (1  4)
    (11 -4)
    (1  34)))

(defparameter *fig* (make-instance 'figure))
;set size 0.5 0.5
;set png size 600, 400
;set output "figure.png"
(xlabel  *fig* "text")
(arrow   *fig* 0 0 10 10 :nohead t)
;(xrange *fig* '(-5 5))
;(yrange *fig* '(0 10))
;(scatter *fig* *dataframe* :cols '(1 2))
(scatter *fig* *dataframe*
         ;:pt 7
         ;:ps 2
         :with "lines"
         )

(show    *fig*)
;(save *fig* "fig.png" 600 400)
;(save *fig* "fig.png")
;(save *fig* "fig.png" 666)
