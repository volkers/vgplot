;;;; package.lisp

(defpackage #:vgplot
  (:use :cl)
  (:import-from :ltk :do-execute)
  (:import-from :cl-fad :with-output-to-temporary-file)
  (:export :close-all-plots
           :close-plot
           :demo
           :figure
           :format-plot
           :new-plot
           :plot
           :plot-file
           :replot
           :range))
