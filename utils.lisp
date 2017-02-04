;;;; Martin Kersner, m.kersner@gmail.com
;;;; 2016/11/10
;;;;
;;;; Auxiliary functions for plotting.

;;; Concatenate string given in list and join them with given delimiter.
(defun concatenate-strings (str-lst &optional  (delim " ") (complete ""))
  (let* ((item-tmp (car str-lst))
         (item (if (stringp item-tmp)
                  item-tmp
                  (write-to-string item-tmp))))

    (if str-lst
      (concatenate-strings
        (cdr str-lst)
        (if (= (length (cdr str-lst)) 1) "" delim) ; no delim after last string
        (concatenate 'string complete item delim))

    complete)))

;;; http://www.codecodex.com/wiki/Generate_a_random_password_or_random_string#Common_Lisp
(defun random-string (length)
  (with-output-to-string (stream)
    (let ((*print-base* 36))
      (loop repeat length do (princ (random 36) stream)))))

;;; Generate path to file with randomly generated name.
;;; Expect to work on Linux.
(defun get-random-filename (&optional (string-length 5))
  (concatenate 'string
    "/tmp/"
    (write-to-string (get-universal-time))
    (random-string string-length)))

;;; Quote string.
(defun quote-string (str)
  (concatenate 'string "\"" str "\""))

;;; http://aima.cs.berkeley.edu/lisp/utilities/utilities.lisp
;;; Return a list of n consecutive integers, by default starting at 0.
(defun iota (n &optional (start-at 0))
  (if (<= n 0) nil (cons start-at (iota (- n 1) (+ start-at 1)))))

;;; Convert any given value to string.
(defun to-str (val)
  (format nil "~(~a~)" val))
