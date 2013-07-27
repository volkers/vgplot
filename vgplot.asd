;;;; vgplot.asd

(asdf:defsystem #:vgplot
  :version "0.1.0"
  :serial t
  :description "Interface to gnuplot"
  :author "Volker Sarodnick <volkersar@gmx.net>"
  :license "GPL"
  :depends-on (#:ltk #:cl-fad #:cl-ppcre)
  :components ((:file "package")
               (:file "vgplot")))
