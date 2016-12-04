;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/11/09
;;;;
;;;; Plot data using GNU plot.
;;;
;;; TODO
;;; clean code
;;; better way to build commands!!
;;; 3D scatterplot

(in-package :lispplot)

(defparameter *space* " ")
(defparameter *empty* "")

;;; FIGURE
(defclass figure ()
  ((shell :accessor get-shell
          :initform "bash")
   (pt    :accessor get-pt
          :initform 7)
   (ps    :accessor get-ps
          :initform 2)
   (nokey :accessor get-nokey
          :initarg :nokey
          :initform nil)
   (palette :accessor get-palette
            :initarg :palette
            :initform nil)
   (circles :accessor set-circles) ;TODO
   (points :accessor set-points) ;TODO
   (commands :accessor get-commands
             :initform (list ""))
   (temporary-files :accessor get-temporary-files
                    :initform nil)))

;;; PRIVATE METHODS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SHOW
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

    ;; setup commands
    ;; nokey
    ;; TODO unify with get-palette
    (if (get-nokey fig)
      (write-line (gen-nokey fig) stream))

    ;; palette
    ;; TODO unify with get-nokey
    (if (get-palette fig)
      (write-line (gen-palette-fig fig) stream))

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

;;; SCATTER
(defgeneric scatter (fig df &key :with cols palette plot-type fill solid-border lt pt ps)
  (:documentation ""))

(defmethod scatter ((fig figure) df &key ((:with with-type) nil)
                                 (cols nil)
                                 (palette nil)
                                 (plot-type 'plot)
                                 (fill nil)
                                 (solid-border)
                                 (lt nil) (pt nil) (ps nil))
  (let* ((filename (get-random-filename))
         (stream (open filename
                      :direction :output 
                      :if-exists :overwrite
                      :if-does-not-exist :create)))

    (mapcar #'(lambda (row) (write-line (concatenate-strings row) stream)) df)

    (add-command fig
                 (gen-scatter-type plot-type) " "
                 (quote-string filename)
                 " using "
                 (scatter-build-cols cols df)
                 (build-with with-type)
                 (gen-pt fig pt)
                 (gen-ps fig ps)
                 (gen-palette-scatter fig palette)
                 (gen-fill fig fill)
                 (if solid-border " solid border " "")
                 (gen-lt fig lt)
                 )

    (push filename (get-temporary-files fig))

  (close stream)))

;;; ARROW
(defgeneric arrow (fig X-start Y-start X-end Y-end &optional nohead)
  (:documentation "Print arrow."))

(defmethod arrow ((fig figure) X-start Y-start X-end Y-end &optional nohead)
  (add-command fig
               "set arrow from " X-start "," Y-start " to " X-end "," Y-end " " nohead))

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

;;; PRIVATE METHODS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ADD-COMMAND
(defgeneric add-command (fig &rest cmd)
  (:documentation "Add commands to internal variable commands."))

(defmethod add-command ((fig figure) &rest cmd)
  (nconc
    (get-commands fig)
    (list (concatenate-strings cmd ""))))

;;; SCATTER TYPE: PLOT, REPLOT
;;; TODO sufficient?
(defun gen-scatter-type (scatter-type)
  (format nil "~(~a~)" scatter-type))

;;; NOKEY
(defgeneric gen-nokey (fig)
  (:documentation "Block legend."))

(defmethod gen-nokey ((fig figure))
  "set nokey")

;;; PALETTE for FIGURE
(defgeneric gen-palette-fig (fig)
  (:documentation "Show palette on plot."))

(defmethod gen-palette-fig ((fig figure))
  "show palette")

;;; General method for setting range of axis.
(defgeneric figure-range (fig axis range)
  (:documentation "Adjust range of figure."))

(defmethod figure-range (fig axis range)
  (add-command fig
    "set " (format nil "~(~a~)" axis) "range [" (concatenate-strings range ":") "]"))

;;; WITH Building block for scatter plot.
(defun build-with (type)
 (if type
  (concatenate 'string " with " (format nil "~(~a~)" type) " ")
  " "))

(defun scatter-build-cols (cols df)
  (if cols
    (concatenate-strings cols ":")
    (concatenate-strings (iota (length (first df)) 1) ":")))

;;; General method for setting label of axis.
(defgeneric figure-label (fig axis label)
  (:documentation "Print label of given axis."))

(defmethod figure-label (fig axis label)
  (add-command fig
               "set " (format nil "~(~a~)" axis) "label " (quote-string label)))

;;; GEN-PT used for SCATTER
;;; TODO keep as DEFGENERIC, DEFMETHOD?
(defgeneric gen-pt (fig val)
  (:documentation "Generate part of 'plot' command related to 'pt' attribute."))

;;; Generate " pt [0-9]+ " if given value is not null
(defmethod gen-pt ((fig figure) val)
  (if val
    (concatenate 'string *space* "pt" *space* (to-str val) *space*)
    *empty*))

;;; GEN-PS used for SCATTER
;;; TODO keep as DEFGENERIC, DEFMETHOD?
(defgeneric gen-ps (fig val)
  (:documentation "Generate part of 'plot' command related to 'ps' attribute."))

;;; Generate " ps [0-9]+ " if given value is not null
(defmethod gen-ps ((fig figure) val)
  (if val
    (concatenate 'string *space* "ps" *space* (to-str val) *space*)
    *empty*))

;;; GEN-LT used for SCATTER
;;; TODO keep as DEFGENERIC, DEFMETHOD?
(defgeneric gen-lt (fig val)
  (:documentation "Generate part of 'plot' command related to 'lt' attribute."))

;;; Generate " lt [0-9]+ " if given value is not null
(defmethod gen-lt ((fig figure) val)
  (if val
    (concatenate 'string *space* "lt" *space* (to-str val) *space*)
    *empty*))

;;; PALETTE for SCATTER
;;; TODO keep as DEFGENERIC, DEFMETHOD?
(defgeneric gen-palette-scatter (fig palette)
  (:documentation "Use palette to display values of points in scatter plot."))

(defmethod gen-palette-scatter ((fig figure) palette)
  (if palette
    (concatenate 'string *space* "palette" *space*)
    *empty*))

;;; GEN-FILL used for SCATTER
;;; TODO keep as DEFGENERIC, DEFMETHOD?
(defgeneric gen-fill (fig fill)
  (:documentation "Generate string fill for 'plot' command."))

(defmethod gen-fill ((fig figure) fill)
  (if fill
    (concatenate 'string *space* "fill" *space*)
    *empty*))
