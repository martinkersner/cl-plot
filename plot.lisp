;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/11/09
;;;;
;;;; Plot data using GNU plot.
;;;
;;; TODO
;;; 2D scatterplot
;;; plot line
;;; 3D scatterplot

(defparameter *output-file* "out") 
(defparameter *shell* "bash")

;;; TODO dont add delimiter at the last position
(defun concatenate-strings (str-lst &optional (complete "") (delim " "))
  (let* (;(delim " ")
         (item-tmp (car str-lst))
         (item (if (stringp item-tmp)
                  item-tmp
                  (write-to-string item-tmp))))

  (if str-lst
    (concatenate-strings
      (cdr str-lst)
      (concatenate 'string complete item delim))
    
    complete)))

;;; class for plotting
(defclass figure ()
  ((pt    :accessor set-pt)
   (ps    :accessor set-ps)
   (nokey :accessor set-nokey)
   (circles :accessor set-circles)
   (points :accessor set-points)
   (commands :accessor get-commands
             :initform '())))

(defgeneric show (fig)
  (:documentation ""))

(defmethod show ((fig figure))
  (let ((stream (open *output-file* 
                      :direction :output 
                      :if-exists :overwrite
                      :if-does-not-exist :create)))
  
    ;; create temporary file with GNU plot commands
    (write-line "cat << EOF | gnuplot -p" stream)

    ;; printing commands
    (mapcar #'(lambda (cmd)
                (write-line (concatenate-strings (list cmd)) stream))
            (get-commands fig))

    (write-line "EOF" stream)
  
    ;; close file
    (close stream)
  
    ;; plot graph
    (ext::shell (concatenate-strings (list *shell* *output-file*)))
  
    ;; remove temporary file
    (delete-file stream)))

;;; SCATTER PLOT
(defgeneric scatter (fig df)
  (:documentation ""))

(defmethod scatter ((fig figure) df)
  (let* ((filename "scatter-data.csv")
         (stream (open filename
                      :direction :output 
                      :if-exists :overwrite
                      :if-does-not-exist :create)))

    (mapcar #'(lambda (row) (write-line (concatenate-strings row) stream)) df)

    (push 
      (concatenate-strings (list "plot" (concatenate 'string "\"" filename "\"") "using 1:2 with points pt 7 ps 2"))
      (get-commands fig))

  (close stream)))

;;; ARROW
(defgeneric arrow (fig X-start Y-start X-end Y-end)
  (:documentation ""))

(defmethod arrow ((fig figure) X-start Y-start X-end Y-end)
  (push 
    (concatenate-strings (list "set arrow from" X-start "," Y-start "to" X-end "," Y-end "nohead"))
    (get-commands fig)))