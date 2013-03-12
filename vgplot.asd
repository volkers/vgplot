;;;; vgplot.asd

(asdf:defsystem #:vgplot
  :serial t
  :description "Interface to gnuplot"
  :author "Volker Sarodnick <volkersar@gmx.net>"
  :license "LGPL"
  :depends-on (:ltk)
  :components ((:file "package")
               (:file "vgplot")))
