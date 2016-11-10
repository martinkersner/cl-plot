;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/11/09
;;;;
;;;; Plot data using GNU plot.
;;;
;;; TODO
;;; 2D scatterplot
;;; plot line
;;; 3D scatterplot
;;; create temporary files /tmp directory

(defparameter *output-file* "out") 
(defparameter *shell* "bash")

;;; TODO dont add delimiter at the last position
(defun concatenate-strings (str-lst &optional  (delim " ") (complete ""))
  (let* (;(delim " ")
         (item-tmp (car str-lst))
         (item (if (stringp item-tmp)
                  item-tmp
                  (write-to-string item-tmp))))

  (if str-lst
    (concatenate-strings
      (cdr str-lst)
      delim
      (concatenate 'string complete item delim))
    
    complete)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric add-command (fig &rest cmd)
  (:documentation ""))

(defmethod add-command ((fig figure) &rest cmd)
  (nconc
    (get-commands fig)
    (list (concatenate-strings cmd ""))))

;;; class for plotting
(defclass figure ()
  ((pt    :accessor set-pt) ; TODO
   (ps    :accessor set-ps) ; TODO
   (nokey :accessor set-nokey) ; TODO
   (circles :accessor set-circles) ;TODO
   (points :accessor set-points) ;TODO
   (commands :accessor get-commands
             :initform (list ""))
   (temporary-files :accessor get-temporary-files
                    :initform nil)))

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
  
    ;; remove from temporary data files
    ;(mapcar #'(lambda (tmp-file) (delete-file tmp-file)) (get-temporary-files fig))

    ;; remove temporary command file
    ;(delete-file stream)
    ))

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

    (nconc
      (get-commands fig)
      (list (concatenate-strings (list "plot" (concatenate 'string "\"" filename "\"") "using 1:2 with points pt 7 ps 2"))))

    (push filename (get-temporary-files fig))

  (close stream)))

;;; ARROW
(defgeneric arrow (fig X-start Y-start X-end Y-end &optional nohead)
  (:documentation ""))

(defmethod arrow ((fig figure) X-start Y-start X-end Y-end &optional nohead)
  (nconc
    (get-commands fig)
    (list (concatenate-strings (list "set arrow from" X-start "," Y-start "to" X-end "," Y-end nohead)))))

;;; XLABEL
(defgeneric xlabel (fig label)
  (:documentation "Print label for X axis"))

(defmethod xlabel ((fig figure) label)
  (add-command fig
               "set xlabel \"" label "\""))
