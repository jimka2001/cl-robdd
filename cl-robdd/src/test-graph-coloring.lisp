;; Copyright (c) 2020 EPITA Research and Development Laboratory
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :graph-coloring-test
  (:use :cl :graph-coloring :cl-robdd :scrutiny :adjuvant :cl-fad
	)
  )


(in-package :graph-coloring-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow-all-symbols :package-from :graph-coloring :package-into :graph-coloring-test))

(define-test test/coloring
  (bdd-with-new-hash ()
    (let* ((nodes (list "a" "b" "c" "d"))
           (uni-directional-graph (assoc-to-hash '(("a" ())
                                                   ("b" ("a"))
                                                   ("c" ("a"))
                                                   ("d" ("c" "b")))))
           (colors (vector "red" "green" "blue" "yellow")))
      (multiple-value-bind (colorization bdd) (graph-to-bdd nodes uni-directional-graph)
        (bdd-visit-satisfying-assignments
         bdd (lambda (assign-true assign-false)
               (let ((color-mapping (assign-colors colorization assign-true assign-false colors)))
                 (dolist (color colors)
                   (let ((same-color-states (loop :for state :being :the :hash-keys :in color-mapping
                                                  :if (string= color (gethash state color-mapping))
                                                    :collect state)))
                     (dolist (state1 same-color-states)
                       (dolist (state2 same-color-states)
                         (unless (string= state1 state2)
                           
                           ;; assert that if two states on the map are colored the same,
                           ;; then they don't connect
                           ;;   i.e., they don't share a border as per uniDirectionalGraph
                           (assert-false (member state2 (gethash state1 uni-directional-graph)
                                                 :test #'string=))
                           (assert-false (member state1 (gethash state2 uni-directional-graph)
                                                 :test #'string=))))))))))))))
(define-test test/sanity
  (sanity-check 4)
  (sanity-check 5)
  (sanity-check 10))
