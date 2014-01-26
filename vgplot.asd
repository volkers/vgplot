;;;; vgplot.asd

(asdf:defsystem :vgplot
  :serial t
  :description "Interface to gnuplot"
  :author "Volker Sarodnick"
  :license "GPL"
  :depends-on (:ltk :cl-fad :cl-ppcre)
  :components ((:file "package")
               (:file "vgplot")
               (:file "demo")))

(asdf:defsystem :vgplot-test
  :serial t
  :description "Test environment for vgplot"
  :author "Volker Sarodnick"
  :license "GPL"
  :depends-on (:vgplot :lisp-unit)
  :components ((:file "package-test")
               (:file "vgplot-test")))
