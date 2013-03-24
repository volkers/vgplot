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

(defun convert-newline (s)
  "Convert lisp newlines to gnuplot style"
  (regex-replace-all "~%" s "\\n"))

(let ((stream-list nil) ; List holding the streams of not active plots
      (stream nil)) ; Stream of the active plot
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
      (setf stream (pop stream-list))))
  (defun close-all-plots ()
    "Close all connected gnuplots"
    (close-plot)
    (when stream
      (close-all-plots)))
  (defun new-plot ()
    "Add a new plot window to a current one."
    (when stream
      (push stream stream-list)
      (setf stream (open-plot))))
  (defun plot (x y &optional &key title (grid t))
    "Plot x,y to active plot, create plot if needed."
    (unless stream
      (setf stream (open-plot)))
    (unless (vectorp x)
      (setf x (coerce x 'vector)
            y (coerce y 'vector)))
    (if grid
      (format stream "set grid~%")
      (format stream "unset grid~%"))
    (when title
      (format stream "set title \"~a\"~%" (convert-newline title)))
    (format stream "plot '-' with lines using 1:2~%")
    (map 'vector #'(lambda (a b) (format stream "~A ~A~%" a b)) x y)
    (format stream "e~%")
    (force-output stream)))


;; utilities and tests 
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

(defun test ()
  (let ((x (range 0 (* 2 pi) 0.01))
        (y))
    (setf y (map 'vector #'sin x))
    (plot x y  :title "y = sin(x)~%(plot x y)")
    (format t "Press ENTER for next plot.~%")
    (read-line)
    (setf y (map 'vector #'cos x))
    (plot x y :title "y = cos(x)~%(plot x y :grid nil)" :grid nil)
    (format t "Press ENTER for new plot.~%")
    (read-line)
    (new-plot)
    (setf y (map 'vector #'(lambda (a) (sin (* 2 a)))  x))
    (plot x y :title "y = cos(2x)~%(new-plot)~%(plot x y)")
    (format t "Press ENTER for (close-all-plots).~%")
    (read-line)
    (close-all-plots)))
