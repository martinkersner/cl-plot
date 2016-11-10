;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/11/09
;;;;
;;;; Plot data using GNU plot.
;;;
;;; TODO
;;; 2D scatterplot
;;; plot line
;;; 3D scatterplot

(defparameter *shell* "bash")

;;; TODO dont add delimiter at the last position
(defun concatenate-strings (str-lst &optional  (delim " ") (complete ""))
  (let* ((item-tmp (car str-lst))
         (item (if (stringp item-tmp)
                  item-tmp
                  (write-to-string item-tmp))))

  (if str-lst
    (concatenate-strings
      (cdr str-lst)
      delim
      (concatenate 'string complete item delim))
    
    complete)))

;;; http://www.codecodex.com/wiki/Generate_a_random_password_or_random_string#Common_Lisp
(defun random-string (length)
  (with-output-to-string (stream)
    (let ((*print-base* 36))
      (loop repeat length do (princ (random 36) stream)))))

;;; Generate path to file with randomly generated name
;;; Expect to work on Linux
(defun get-random-filename (&optional (string-length 5))
  (concatenate 'string
    "/tmp/"
    (write-to-string (get-universal-time))
    (random-string string-length)))

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
    (ext::shell (concatenate-strings (list *shell* cmd-filename)))
  
    ;; remove from temporary data files
    (mapcar #'(lambda (tmp-file) (delete-file tmp-file)) (get-temporary-files fig))

    ;; remove temporary command file
    (delete-file stream)
    ))

;;; SCATTER PLOT
(defgeneric scatter (fig df)
  (:documentation ""))

(defmethod scatter ((fig figure) df)
  (let* ((filename (get-random-filename))
         (stream (open filename
                      :direction :output 
                      :if-exists :overwrite
                      :if-does-not-exist :create)))

    (mapcar #'(lambda (row) (write-line (concatenate-strings row) stream)) df)

    (add-command fig
                 "plot " "\"" filename "\"" " using 1:2 with points pt 7 ps 2")

    (push filename (get-temporary-files fig))

  (close stream)))

;;; ARROW
(defgeneric arrow (fig X-start Y-start X-end Y-end &optional nohead)
  (:documentation ""))

(defmethod arrow ((fig figure) X-start Y-start X-end Y-end &optional nohead)
  (add-command fig
               "set arrow from " X-start "," Y-start " to " X-end "," Y-end " " nohead))

;;; XLABEL
(defgeneric xlabel (fig label)
  (:documentation "Print label for X axis"))

(defmethod xlabel ((fig figure) label)
  (add-command fig
               "set xlabel \"" label "\""))

;;; YLABEL
(defgeneric ylabel (fig label)
  (:documentation "Print label for Y axis"))

(defmethod ylabel ((fig figure) label)
  (add-command fig
               "set ylabel \"" label "\""))
