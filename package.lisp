;;;; package.lisp

(defpackage #:vgplot
  (:use :cl)
  (:import-from :ltk :do-execute)
  (:import-from :cl-fad :with-output-to-temporary-file)
  (:export :format-plot :close-plot :close-all-plots :new-plot :plot :plot-file :demo :range))
