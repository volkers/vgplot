;;;; vgplot-test.lisp

#|
    This is the test environment for vgplot, an interface to the gnuplot utility.
    Copyright (C) 2013 - 2020  Volker Sarodnick

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(in-package #:vgplot-test)

(setq lisp-unit:*print-failures* t)

(define-test test-get-separator
  (assert-equal t (vgplot::get-separator "30	4	87"))
  (assert-equal t (vgplot::get-separator "1.9e3 4 87"))
  (assert-equal nil (vgplot::get-separator " # comment"))
  (assert-equal nil (vgplot::get-separator ""))
  (assert-equal #\, (vgplot::get-separator "30,4,87"))
  (assert-equal #\; (vgplot::get-separator "30.04567;4;87 # comment"))
  (assert-equal #\, (vgplot::get-separator " 30.0d19 ,4 , 87")))

(define-test test-count-data-columns
  (assert-equal 0 (vgplot::count-data-columns ""))
  (assert-equal 1 (vgplot::count-data-columns "17"))
  (assert-equal 2 (vgplot::count-data-columns "17	987654321"))
  (assert-equal 2 (vgplot::count-data-columns "1.7d179, 87654321" #\,))
  (assert-equal 20 (vgplot::count-data-columns "1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0" #\,))
  (assert-equal 2 (vgplot::count-data-columns "		1.7e179 ; 87654321 # comment" #\;))
)

(define-test test-stairs-no-plot
  (assert-equalp (list #(0) #(1)) (vgplot:stairs-no-plot #(1)))
  (assert-equalp '(#(0 1 1 2 2 3 3 4 4) #(3 3 2 2 7 7 1 1 6))
                 (vgplot:stairs-no-plot #(3 2 7 1 6)))
  (assert-equalp '(#(0 1 1 2 2 3 3 4 4) #(3 3 2 2 7 7 1 1 6))
                 (vgplot:stairs-no-plot '(3 2 7 1 6)))
  (assert-equalp '(#(0 1 1 2 2 3 3 4 4) #(3 3 2 2 7 7 1 1 6))
                 (vgplot:stairs-no-plot '(0 1 2 3 4) '(3 2 7 1 6)))
  (assert-equalp '(#(1 3 3) #(4.0 4.0 -1))
                 (vgplot:stairs-no-plot #(1 3) #(4.0 -1)))
  (assert-equalp '(#(1 3 3) #(17.0 17.0 -1.0))
                 (vgplot:stairs-no-plot #(1 3) '(17.0 -1.0))))

(define-test test-parse-label
  (assert-equalp "with lines  title '' "
                 (vgplot::parse-label ""))
  (assert-equalp "with lines  title '' "
                 (vgplot::parse-label ";"))
  (assert-equalp "with lines  title '' "
                 (vgplot::parse-label ";;"))
  (assert-equalp "with lines  title 'title' "
                 (vgplot::parse-label ";title;"))
  (assert-equalp "with lines  title 'title' "
                 (vgplot::parse-label ";title"))
  (assert-equalp "add-styles  title 'title' "
                 (vgplot::parse-label ";title;add-styles"))
  (assert-equalp "add-styles  title 'title' "
                 (vgplot::parse-label "+b;title;add-styles"))
  (assert-equalp "with lines linecolor rgb 'red' title 'title' "
                 (vgplot::parse-label "-r;title;"))
  (assert-equalp "with points linecolor rgb 'green' title 'title' "
                 (vgplot::parse-label "g+;title;"))
  (assert-equalp "with lines dt '. . ' linecolor rgb 'blue' title 'title' "
                 (vgplot::parse-label ":b;title;"))
  (assert-equalp "with dots linecolor rgb 'cyan' title 'title' "
                 (vgplot::parse-label ".c;title;"))
  (assert-equalp "with points linecolor rgb 'black' title 'title' "
                 (vgplot::parse-label "+k;title;"))
  (assert-equalp "with circles linecolor rgb 'yellow' title 'title' "
                 (vgplot::parse-label "oy;title;"))
  (assert-equalp "with circles linecolor rgb 'magenta' title 'title' "
                 (vgplot::parse-label "om;title;"))
  (assert-equalp "with dots linecolor rgb 'white' title 'title' "
                 (vgplot::parse-label ".w;title;"))
  (assert-equalp "with dots linecolor rgb '#112233' title 'title' "
                 (vgplot::parse-label ".#112233;title;"))
  (assert-equalp "with points linecolor rgb '#112233' title 'title' "
                 (vgplot::parse-label "#112233+;title;"))
  )

(defun run ()
  (lisp-unit:run-tests :all :vgplot-test))
