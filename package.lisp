;;;; package.lisp

(defpackage #:vgplot
  (:use :cl)
  (:import-from :ltk :do-execute)
  (:export :format-plot :close-plot :close-all-plots :new-plot :plot :test :range))
