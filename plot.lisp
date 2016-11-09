;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/11/09
;;;;
;;;; Plot data using GNU plot.

(defparameter *output-file* "out") 
(defparameter *shell* "bash")

;;; TODO dont add delimiter at the last position
(defun concatenate-strings (str-lst &optional (complete ""))
  (let* ((delim " ")
         (item-tmp (car str-lst))
         (item (if (stringp item-tmp)
                  item-tmp
                  (write-to-string item-tmp))))
                                                                                                                                                                         
  (if str-lst
    (concatenate-strings
      (cdr str-lst)
      (concatenate 'string complete item delim))
    
    complete)))

;;; main plotting module
(let ((stream (open *output-file* 
                    :direction :output 
                    :if-exists :overwrite
                    :if-does-not-exist :create)))

  ;; create temporary file with GNU plot commands
  ;; demo
  (write-line "#!/usr/bin/env bash" stream)
  (write-line "cat << EOF | gnuplot -p" stream)
  (write-line "splot sin(x*y/20)" stream)
  (write-line "EOF" stream)

  ;; close file
  (close stream)

  ;; plot graph
  (ext::shell (concatenate-strings (list *shell* *output-file*)))

  ;; remove temoporary file
  (delete-file stream))
 
