;;;; demo.lisp

#|
    This file contains a demonstration of some of the functions of vgplot.
    vgplot is an interface to the gnuplot utility.
    Copyright (C) 2013, 2014  Volker Sarodnick

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
  "Print commands in lst and run them after a (read-char),
ENTER continue, all other characters break and quit demo"
  (loop for cmd in lst do
       (progn (princ (drop-substring "vgplot::" (format nil "~s" cmd)))
              (unless (char= #\Newline (read-char))
                (close-all-plots)
                (return-from print-n-run-list nil))
              (eval cmd))))

(defun demo ()
  "Show usecases of vgplot."
  (let ((*print-case* :downcase))
    (format t "****************************************************************~%")
    (format t "vgplot demo, run commands by pressing RETURN~%")
    (format t "press q (or any other character) RETURN to leave the demo~%")
    (format t "****************************************************************~%")
    (print-n-run-list
     '( ;; add demo commands here
       (plot '(0 -2 17))
       (text 0.2 10 "You can plot a list directly against its index\\n(text x y) adds a string")
       (progn
         (plot #(1 2 3) #(0 -2 -17) ";silly example;")
         (title "Simple curve")
         (text 1.2 -14 "Plot vectors with legend and add a title"))
       (progn
         (defvar x)
         (defvar y)
         (setf x (range 0 (* 2 pi) 0.01))
         (setf y (map 'vector #'sin x))
         (plot x y "y = sin(x)")
         (xlabel "[rad]")
         (ylabel "magnitude")
         (text 0.2 -0.6 "Use function range to create vectors and add labels to axes"))
       (and "text-show-labels shows the active labels and their tags"
        (text-show-label))
       (and "Usefull to delete or change labels"
        (text-delete 1 2 3))
       (text 0.5 -0.5 "You can use a different size" :fontsize 14)
       (progn
         (text 0.5 -0.5 "You can change a present label"
               :tag 1 :rotation 60 :font "Times" :fontsize 12)
         (and "font definitions for text change also the font of the legend,
the following is needed to change the keys back")
         (format-plot t "set key font \",10\"")
         (replot))
       (progn
         (text-delete 1)
         (text 3 0.5 "axis can change the range of the x axis")
         (axis (list (/ pi 2) 5)))
       (progn
         (text -0.5 -0.5 "axis can also change the range of both x and y axis" :tag 1)
         (axis (list -1 pi -1.2 1.2)))
       (progn
         (text 0.5 -0.5 "t means autoscale and nil unchanged corresponding axis" :tag 1)
         (axis '(t  nil)))
       (progn
         (text 0.5 -0.5 "another example of the use of axis" :tag 1)
         (axis '(nil nil -1.5 t)))
       (progn
         (text 0.5 -0.5 "Remove the grid" :tag 1)
         (grid nil))
       (and "close-plot closes the actual plot"
        (close-plot))
       (progn
         (defvar z)
         (setf z (map 'vector #'cos x))
         (plot x y "b;y = sin(x);" x z "g;y = cos(x);")
         (title "Some Other Graphs"))
       (progn
         (new-plot)
         (setf y (map 'vector #'(lambda (a) (sin (* 2 a))) x))
         (plot x y "+k;y = cos(2x) (new-plot);")
         (text 0.5 -0.5 "new-plot adds a new plot window"))
       (text-show-label)
       (progn
         (plot x y "og;y = cos(2x) (new-plot);")
         (text 0.5 0.5 "Different line style" :tag 1))
       (progn
         (and "(format-plot) allows direct commands to the running gnuplot process")
         (format-plot t "set size square 0.5,0.5~%")
         (replot))
       (close-all-plots)
       (let ((x #(1 2 3 4)))
         (subplot 3 2 0)
         (plot x #(0.5 2 -3 4) x #(-1 2 3 4))
         (title "Use of multiplots")
         (subplot 3 2 1)
         (plot x #(0.2 0.4 3 0.4) x #(-1 -2 3 4))
         (title "")
         (subplot 3 2 2)
         (plot x #(-1 2 3 4) x #(-1 -2 -3 4))
         (subplot 3 2 3)
         (plot x #(1 -1.8 -2.8 4) x #(-1 -2 -3 -4))
         (subplot 3 2 4)
         (plot x #(1 2 -3 4) x #(1 -2 3 4))
         (subplot 3 2 5)
         (plot x #(1 -2 3 4) x #(1 -2 -3 4)))
       (close-plot)
       (progn
         (setf y #(0 4 6.5 6.8 6.5 6.2 6.1 6.05 6.0 6.0))
         (stairs y)
         (title "Example of a stairstep plot"))
       (progn
         (setf x (range (length y)))
         (stairs x y "line 1" '(0 1 10) '(0 6 6) "k;line 2;"))
       (close-plot)
       (flet ((e-fun (x) (- 1 (exp (- x)))))
         (let* ((x (range 0 6 0.1))
                (y (map 'vector #'e-fun x))
                (xd (range 7))
                (yd (map 'vector #'e-fun xd))
                (sampled (stairs-no-plot xd yd)))
           (plot x y "b;continuous;"
                        (first sampled) (second sampled) "sampled"))
         (title "Example of a mixture of continuous and discrete characteristics"))
       (close-plot)
       (progn
         (bar '(:y (7 9 7.6 8)))
         (axis '(-1 4 -1 10))
         (title "Simple example of a bar plot"))
       (close-plot)
       (progn
         (bar '((:x #(2011 2013 2014) :y #(0.8 0.9 -0.3) :color "blue" :label "Values 1")
                (:x #(2011 2013 2014) :y #(0.7 0.65 0.5) :color "red" :label "Values 2")
                (:x #(2011 2013 2014) :y #(0.75 0.4 0.1) :color "cyan" :label "Values 3"))
              :width 0.5)
         (title "Another example of a bar plot"))
       (close-plot)
       (or "The following works if you copy data.txt and data.csv
from vgplot's source directory to your directory")
       (when (cl-fad:file-exists-p "data.txt")
         (plot-file "data.txt"))
       (when (cl-fad:file-exists-p "data.csv")
         (plot-file "data.csv"))
       (when (cl-fad:file-exists-p "data.csv")
         (plot (first (load-data-file "data.csv")))
         (text 2 -1 "load-data-file returns data from a csv file"))
       (close-all-plots)))))
