;; Copyright (c) 2016 EPITA Research and Development Laboratory
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

(defpackage :cl-robdd-analysis-test
  (:use :cl :cl-robdd :cl-robdd-analysis :lisp-unit))

(in-package :cl-robdd-analysis-test)

(shadow-all-symbols :package-from :cl-robdd-analysis
                    :package-into :cl-robdd-analysis-test)


(defun test-operation-order ()
  (labels ((local (vars)
             (when vars
               (local (cdr vars)))
             (let ((*bdd-operation-order* :reduce))
               (format t "size=~A ~A~%" (length vars) *bdd-operation-order*)
               (bdd-with-new-hash ()
                 (time (bdd (random-boolean-combination vars)))))
             (let ((*bdd-operation-order* :divide-and-conquer))
               (format t "size=~A ~A~%" (length vars)
                       *bdd-operation-order*)
               (bdd-with-new-hash ()
                 (time (bdd (random-boolean-combination vars)))))))
    (local '(Z1 Z2 Z3 Z4 Z5 Z6 Z7 Z8 Z9))))


(define-test test/random-combination
  (flet ((test-it (*bdd-operation-order*)
           (bdd-with-new-hash ()
             (let ((vars '(Z1 Z2 Z3 Z4 Z5 Z6)))
               (bdd (random-boolean-combination vars))))))
    (test-it :reduce)
    (test-it :divide-and-conquer)))
