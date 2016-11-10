;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/11/09
;;;;
;;;; Plot data using GNU plot.
;;;
;;; TODO
;;; better way to build commands
;;; 3D scatterplot

(load "utils")

;;; FIGURE
(defclass figure ()
  ((shell :accessor get-shell
          :initform "bash")
   (pt    :accessor get-pt
          :initform 7)
   (ps    :accessor get-ps
          :initform 2)
   (nokey :accessor set-nokey) ; TODO
   (circles :accessor set-circles) ;TODO
   (points :accessor set-points) ;TODO
   (commands :accessor get-commands
             :initform (list ""))
   (temporary-files :accessor get-temporary-files
                    :initform nil)))

(defgeneric add-command (fig &rest cmd)
  (:documentation "Add commands to internal variable commands."))

(defmethod add-command ((fig figure) &rest cmd)
  (nconc
    (get-commands fig)
    (list (concatenate-strings cmd ""))))


(defgeneric show (fig)
  (:documentation "Display plot."))

(defmethod show ((fig figure))
  (let* ((cmd-filename (get-random-filename))
         (stream (open cmd-filename
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
    (ext::shell (concatenate-strings (list (get-shell fig) cmd-filename)))
  
    ;; remove from temporary data files
    (mapcar #'(lambda (tmp-file) (delete-file tmp-file)) (get-temporary-files fig))

    ;; remove temporary command file
    (delete-file stream)
    ))

;;; SCATTER PLOT
(defgeneric scatter (fig df cols &optional extra)
  (:documentation ""))

(defmethod scatter ((fig figure) df cols &optional (extra ""))
  (let* ((filename (get-random-filename))
         (stream (open filename
                      :direction :output 
                      :if-exists :overwrite
                      :if-does-not-exist :create)))

    (mapcar #'(lambda (row) (write-line (concatenate-strings row) stream)) df)

    (add-command fig
                 "plot "
                 (quote-string filename)
                 " using "
                 (concatenate-strings cols ":")
                 " with points pt " (get-pt fig) " ps " (get-ps fig)
                 " " extra)

    (push filename (get-temporary-files fig))

  (close stream)))

;;; ARROW
(defgeneric arrow (fig X-start Y-start X-end Y-end &optional nohead)
  (:documentation "Print arrow."))

(defmethod arrow ((fig figure) X-start Y-start X-end Y-end &optional nohead)
  (add-command fig
               "set arrow from " X-start "," Y-start " to " X-end "," Y-end " " nohead))

;;; General method for setting label of axis.
(defgeneric figure-label (fig axis label)
  (:documentation "Print label of given axis."))

(defmethod figure-label (fig axis label)
  (add-command fig
               "set " (format nil "~(~a~)" axis) "label " (quote-string label)))

;;; XLABEL
(defgeneric xlabel (fig label)
  (:documentation "Print label for X axis."))

(defmethod xlabel ((fig figure) label)
  (figure-label fig 'x label))

;;; YLABEL
(defgeneric ylabel (fig label)
  (:documentation "Print label for Y axis."))

(defmethod ylabel ((fig figure) label)
  (figure-label fig 'y label))

;;; TITLE
(defgeneric title (fig label)
  (:documentation "Print label for Y axis."))

(defmethod title ((fig figure) label)
  (add-command fig
               "set title " (quote-string label)))

;;; NOKEY
(defgeneric nokey (fig)
  (:documentation "Block legend."))

(defmethod nokey ((fig figure))
  (add-command fig
               "set nokey"))

;;; General method for setting range of axis.
(defgeneric figure-range (fig axis range)
  (:documentation "Adjust range of figure."))

(defmethod figure-range (fig axis range)
  (add-command fig
    "set " (format nil "~(~a~)" axis) "range [" (concatenate-strings range ":") "]"))

;;; XRANGE
(defgeneric xrange (fig range)
  (:documentation "Adjust range of X axis."))

(defmethod xrange ((fig figure) range)
  (figure-range fig 'x range))

;;; YRANGE
(defgeneric yrange (fig range)
  (:documentation "Adjust range of Y axis."))

(defmethod yrange ((fig figure) range)
  (figure-range fig 'y range))
