;;;; vgplot.lisp

#|
    This library is an interface to the gnuplot utility.
    Copyright (C) 2013  Volker Sarodnick

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(in-package #:vgplot)

(defun open-plot ()
  "Start gnuplot process and return stream to gnuplot"
  (do-execute "gnuplot" nil))

(defun vectorize (vals)
  "Coerce all sequences except strings to vectors"
  (mapcar #'(lambda (x) (if (stringp x)
                            x
                            (coerce x 'vector)))
          vals))

(defun parse-vals (vals)
  "Parse input values to plot and return grouped list: ((x y lbl-string) (x1 y1 lbl-string)...)"
  (cond
    ((stringp (third vals)) (cons (list (pop vals) (pop vals) (pop vals))
                                  (parse-vals vals)))
    ((second vals) (cons (list (pop vals) (pop vals) "")
                         (parse-vals vals)))
    (vals (list (list (first vals) nil ""))) ;; special case of plot val to index, i.e. only y exist
    (t nil)))

(defun parse-label (lbl)
  "Parse label string e.g. \"-k;label;\" and return accordinggnuplot style command string."
  (let ((style "lines")
        (color "red")
        (title "")
        (start-title (or (search ";" lbl) -1)) ;; -1 because subseq jumps over first ;
        (end-title (or (search ";" lbl :from-end t) (length lbl))))
    (setf title (subseq lbl (1+ start-title) end-title))
    (when (> start-title 0)
      (loop for c across (subseq lbl 0 start-title) do
           (ecase c
             (#\- (setf style "lines"))
             (#\. (setf style "dots"))
             (#\+ (setf style "points"))
             (#\r (setf color "red"))
             (#\g (setf color "green"))
             (#\b (setf color "blue"))
             (#\c (setf color "cyan"))
             (#\k (setf color "black")))))
    (format nil "with ~A linecolor rgb \"~A\" title \"~A\" " style color title)))

(defun del-tmp-files (tmp-file-list)
  "Delete files in tmp-file-list and return nil"
  (when tmp-file-list
    (loop for name in tmp-file-list do
         (delete-file name)))
  nil)

(defun count-data-columns (s)
  "Count data columns in strings like \"1 2 3 # comment\", seperators
  could be a variable number of spaces or tabs"
  (let ((sep t) (num 0))
               (loop for c across s do
                    (cond
                      ((eql c #\# ) (return))
                      ((eql c #\	 ) (setf sep t))
                      ((eql c #\ ) (setf sep t))
                      (t (when sep
                           (incf num)
                           (setf sep nil)))))
               num))

(let ((stream-list nil) ; List holding the streams of not active plots
      (stream nil) ; Stream of the active plot
      (tmp-file-names nil)) ; list of temporary filenames
  (defun format-plot (text &rest args)
    "Format directly to active gnuplot process"
    (when stream
      (apply #'format stream text args)
      (force-output stream)))
  (defun close-plot ()
    "Close connected gnuplot"
    (when stream
      (format stream "quit~%")
      (force-output stream)
      (close stream)
      (setf stream (pop stream-list)))
    (setf tmp-file-names (del-tmp-files tmp-file-names)))
  (defun close-all-plots ()
    "Close all connected gnuplots"
    (close-plot)
    (when stream
      (close-all-plots)))
  (defun new-plot ()
    "Add a new plot window to a current one."
    (when stream
      (push stream stream-list)
      (setf stream (open-plot)))
    (setf tmp-file-names (del-tmp-files tmp-file-names)))
  (defun plot (&rest vals)
    "Plot y = f(x) on active plot, create plot if needed.
vals could be: y                  plot y over its index
               x y                plot y = f(x)
               x y lable-string   plot y = f(x) using lable-string as label
               following parameters add plots e.g.:
               x y label x1 y1 label1 ..."
    (unless stream
      (setf stream (open-plot)))
    (setf tmp-file-names (del-tmp-files tmp-file-names))
    ;; todo: how to always delete temp files?
    (let ((val-l (parse-vals (vectorize vals)))
          (plt-cmd nil))
      (loop for pl in val-l do
           (push (with-output-to-temporary-file (tmp-file-stream :template "vgplot-%.dat")
                   (if (null (second pl)) ;; special case plotting to index
                       (map nil #'(lambda (a) (format tmp-file-stream "~,,,,,,'eE~%" a)) (first pl))
                       (map nil #'(lambda (a b) (format tmp-file-stream "~,,,,,,'eE ~,,,,,,'eE~%" a b))
                                          (first pl) (second pl))))
                 tmp-file-names)
           (setf plt-cmd (concatenate 'string (if plt-cmd
                                                  (concatenate 'string plt-cmd ", ")
                                                  "plot ")
                                      (format nil "\"~A\" ~A"
                                              (first tmp-file-names) (parse-label (third pl))))))
      (format stream "set grid~%")
      (format stream "~A~%" plt-cmd)
      (force-output stream))
    t)
  (defun plot-file (data-file)
    "Plot data-file directly, datafile must hold columns separated by spaces or tabs, use with-lines style"
    (let ((c-num)
          (cmd-string (format nil "plot \"~A\" using ($1) with lines" data-file)))
      (with-open-file (in data-file :direction :input)
        ;; use only lines with more than zero columns, i.e. drop comment lines
        (setf c-num (do ((num (count-data-columns (read-line in))
                                (count-data-columns (read-line in))))
                        ((> num 0) num))))
      (loop for i from 2 to c-num do
         (setf cmd-string (concatenate 'string cmd-string
                                       (format nil ", \"~A\" using ($~A) with lines" data-file i))))
      (unless stream
        (setf stream (open-plot)))
      (format stream "set grid~%")
      (format stream "~A~%" cmd-string)
      (force-output stream))
    t)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exported utilities

(defun range (a &optional b (step 1))
  "Return vector of values in a certain range:
\(range limit\) return natural numbers below limit
\(range start limit\) return ordinary numbers starting with start below limit
\(range start limit step\) return numbers starting with start, successively adding step untill reaching limit \(excluding\)"
  (apply 'vector (let ((start)
                       (limit))
                   (if b
                       (setf start a
                             limit b)
                       (setf start 0
                             limit a))
                   (if (> limit start)
                       (loop for i from start below limit by step collect i)
                       (loop for i from start above limit by step collect i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities for demo and demo

(defun drop-substring (substring instring)
  (let ((i 0)
        (ilen (length substring))
        (chars))
    (loop for c across instring do
         (if (char= c (aref substring i))
             (incf i)
             (if (= i 0)
                 (push c chars)
                 (progn
                   (loop for j below i do
                        (push (aref substring j) chars))
                   (push c chars)
                   (setf i 0))))
         (if (= i ilen)
             (setf i 0)))
    (coerce (nreverse chars) 'string)))

(defun print-n-run-list (lst)
  "Print commands in lst and run them after a (read-line)"
  (loop for cmd in lst do
       (progn (princ (drop-substring "vgplot::" (format nil "~s" cmd)))
              (read-line)
              (eval cmd))))

(defun demo ()
  (let ((*print-case* :downcase))
    (format t "****************************************************************~%")
    (format t "vgplot demo, run commands by pressing RETURN~%")
    (format t "****************************************************************~%")
    (print-n-run-list
     '( ;; add demo commands here
       (defvar x)
       (defvar y)
       (setf x (range 0 (* 2 pi) 0.01))
       (setf y (map 'vector #'sin x))
       (plot x y)
       (plot x y "y = sin(x)")
       (defvar z)
       (setf z (map 'vector #'cos x))
       (plot x y "b;y = sin(x);" x z "g;y = cos(x);")
       (new-plot)
       (setf y (map 'vector #'(lambda (a) (sin (* 2 a))) x))
       (plot x y "+k;y = cos(2x) (new-plot);")
       (close-all-plots)))))
