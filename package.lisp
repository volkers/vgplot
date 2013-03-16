;;;; package.lisp

(defpackage #:vgplot
  (:use :cl)
  (:import-from :ltk :do-execute)
  (:export :close-plot :plot :test))
