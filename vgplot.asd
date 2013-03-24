;;;; vgplot.asd

(asdf:defsystem #:vgplot
  :serial t
  :description "Interface to gnuplot"
  :author "Volker Sarodnick <volkersar@gmx.net>"
  :license "GPL"
  :depends-on (#:ltk #:cl-ppcre)
  :components ((:file "package")
               (:file "vgplot")))
