;;;; package.lisp

(defpackage #:vgplot
  (:use :cl)
  (:import-from :ltk :do-execute)
  (:import-from :cl-fad :with-output-to-temporary-file)
  (:documentation "# vgplot

This common lisp library is an interface to the gnuplot plotting
utility.

The intention of the API is to resemble to some of the plot commands
of octave or matlab.

## Usage

(asdf:load-system :vgplot) or (ql:quickload :vgplot)

(vgplot:plot '(1 2 3) '(0 -2 17))

For examples run the demo function:

(vgplot:demo)

and see API documentation in doc/vgplot.html or here: http://goto.glocalnet.net/volkers/vgplot/vgplot.html

## License

Copyright (C) 2013 - 2015 Volker Sarodnick

mail: (remove #\\y (subseq \"avoidspamyvolykerysyar@gymyx.net\" 9))

GNU General Public License
")
  (:export :*debug*
           :*gnuplot-binary*
           :axis
           :bar
           :close-all-plots
           :close-plot
           :demo
           :figure
           :format-plot
           :grid
           :legend
           :load-data-file
           :loglog
           :new-plot
           :plot
           :plot-file
           :print-plot
           :replot
           :range
           :semilogx
           :semilogy
           :stairs
           :stairs-no-plot
           :subplot
           :text
           :text-delete
           :text-show-label
           :title
           :xlabel
           :ylabel))
