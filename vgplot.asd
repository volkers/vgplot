;;;; vgplot.asd

(asdf:defsystem #:vgplot
  :serial t
  :description "Interface to gnuplot"
  :author "Volker Sarodnick <volkersar@gmx.net>"
  :license "GPL"
  :depends-on (:ltk)
  :components ((:file "package")
               (:file "vgplot")))
