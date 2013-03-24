;;;; package.lisp

(defpackage #:vgplot
  (:use :cl)
  (:import-from :ltk :do-execute)
  (:import-from :cl-ppcre :regex-replace-all)
  (:export :format-plot :close-plot :close-all-plots :new-plot :plot :test :range))
