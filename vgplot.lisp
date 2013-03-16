;;;; vgplot.lisp

(in-package #:vgplot)

(defun open-plot ()
  "Start gnuplot process and return stream to gnuplot"
  (do-execute "gnuplot" nil))

(let ((stream-list nil) ; List holding the streams of not active plots
      (stream nil)) ; Stream of the active plot
  (defun close-plot ()
    "Close connected gnuplot"
    (when stream
      (format stream "quit~%")
      (force-output stream)
      (close stream)
      (setf stream (pop stream-list))))
  (defun plot (x y)
    (unless stream
      (setf stream (open-plot)))
    (format stream "plot '-' with lines using 1:2~%")
    (map 'vector #'(lambda (a b) (format stream "~A ~A~%" a b)) x y)
    (format stream "e~%")
    (force-output stream)))

(defun test ()
  (let ((x #(0 1 2 3))
        (y #(0 2 -1 3)))
    (plot x y)))

        