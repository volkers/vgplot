;;;; vgplot.lisp

(in-package #:vgplot)

(defvar *plots* nil "List holding the identifiers of the plots")
(defvar *active-plot* nil "Identifier of the active plot")

(defun open-plot ()
  "Start gnuplot process and returns stream to gnuplot"
  (do-execute "gnuplot" nil))

(defun close-plot ()
  "Close gnuplot from *active-plot*"
  (when *active-plot*
    (let ((g-stream (plot-stream *active-plot*)))
      (format g-stream "quit~%")
      (force-output g-stream)
      (close g-stream)
      (setf *active-plot* (pop *plots*)))))

(defclass plot-class ()
  ((stream-fd :reader plot-stream
              :initarg :stream-fd)))

(defun make-plot ()
  (make-instance 'plot-class :stream-fd (open-plot)))

(defun plot (x y)
  (unless *active-plot*
    (setf *active-plot* (make-plot)))
  (format (plot-stream *active-plot*) "plot '-' with lines using 1:2~%")
  (map 'vector #'(lambda (a b) (format (plot-stream *active-plot*) "~A ~A~%" a b)) x y)
  (format (plot-stream *active-plot*) "e~%")
  (force-output (plot-stream *active-plot*)))

