;;;; vgplot.test.asd

(asdf:defsystem #:vgplot-test
  :serial t
  :description "Test environment for vgplot"
  :author "Volker Sarodnick <volkersar@gmx.net>"
  :license "GPL"
  :depends-on (#:vgplot #:lisp-unit)
  :components ((:file "package-test")
               (:file "vgplot-test")))
