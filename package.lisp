;;;; package.lisp

(defpackage #:vgplot
  (:use :cl)
  (:import-from :ltk :do-execute)
  (:import-from :cl-fad :with-output-to-temporary-file)
  (:documentation "# vgplot

This common lisp library is an interface to the gnuplot plotting
utility.

The intention of the API is to be similar to some of the plot commands
of octave or matlab.

## Usage

(asdf:load-system :vgplot)

(vgplot:plot '(1 2 3) '(0 -2 17))

For examples run the demo function in vgplot.lisp:

(vgplot:demo)

and see API documentation in doc/vgplot.html

## License

Copyright (C) 2013, 2014 Volker Sarodnick

mail: (remove #\\y (subseq \"avoidspamyvolykerysyar@gymyx.net\" 9))

GNU General Public License
")
  (:export :axis
           :bar
           :close-all-plots
           :close-plot
           :demo
           :figure
           :format-plot
           :grid
           :load-data-file
           :new-plot
           :plot
           :plot-file
           :replot
           :range
           :stairs
           :stairs-no-plot
           :subplot
           :text
           :text-delete
           :text-show-label
           :title
           :xlabel
           :ylabel))
