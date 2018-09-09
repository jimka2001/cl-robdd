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
  (:use :cl :cl-robdd :cl-robdd-analysis :jimka-test))

(in-package :cl-robdd-analysis-test)

(shadow-all-symbols :package-from :cl-robdd-analysis
                    :package-into :cl-robdd-analysis-test)


(defun test ()
  (run-package-tests :cl-robdd-analysis-test))


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

(define-test test/median
  (assert-true (= 1 (median-a-list '((1 3)))))
  (assert-true (= 1 (median-a-list '((0 1) (1 3) (2 1)))))
  (assert-true (= 1 (median-a-list '((0 2) (1 3) (2 1)))))
  (assert-true (= 1/2 (median-a-list '((0 2) (1 2)))))
  )

 ;;    (sb-ext::gc :full t)
 ;;    (latex-measure-bdd-sizes "/Users/jnewton/newton.16.edtchs/src" '(Z1 Z2 Z3 Z4 Z5 Z6) 4000)

(defun test-with-z1-z3 (prefix num-samples &key (re-run t))
  (garbage-collect)
  (latex-measure-bdd-sizes prefix '(Z1 Z2 Z3 Z4 Z5 Z6) num-samples
                           :min 1 :max 3 :re-run re-run))

(defun test-with-z1-z6 (prefix num-samples &key (re-run t))
  (garbage-collect)
  (latex-measure-bdd-sizes prefix '(Z1 Z2 Z3 Z4 Z5 Z6) num-samples
                           :min 1 :max 6 :re-run re-run))

(defun test-with-z7-z8 (prefix num-samples &key (re-run t))
  (garbage-collect)
  (latex-measure-bdd-sizes prefix '(Z1 Z2 Z3 Z4 Z5 Z6 Z7 Z8) num-samples
                           :min 7 :max 8 :re-run re-run))

(defun test-with-z3-z8 (prefix num-samples &key (re-run t))
  (garbage-collect)
  (latex-measure-bdd-sizes prefix '(Z1 Z2 Z3 Z4 Z5 Z6 Z7 Z8) num-samples
                           :min 3 :max 8 :re-run re-run))

(defun test-with-z1-z8 (prefix num-samples &key (re-run t))
  (garbage-collect)
  (latex-measure-bdd-sizes prefix '(Z1 Z2 Z3 Z4 Z5 Z6 Z7 Z8) num-samples
                           :min 1 :max 8 :re-run re-run))


;; (test-with-z1-z6 "/Users/jnewton/newton.16.edtchs/src/bdd-distribution.ltxdat" 1000)


(define-test test/bdd-sizes
  (ensure-directories-exist "/tmp/jnewton/graph/bdd-distribution.ltxdat")
  (test-with-z1-z6 "/tmp/jnewton/graph" 10 ;; 4000
                   ))

(define-test test/difference-function
  (assert-true (equal (difference-function '((1 1.0))
                                           '((2 2.0)))
                      '((1 0.0) (2 0.0))))
  (assert-true (equal (difference-function '((2 2.0))
                                           '((1 1.0)))
                      '((1 0.0) (2 0.0))))
  (assert-true (equal (difference-function '((1 1.0) (3 3.0))
                                           '((2 2.0) (4 4.0)))
                      '((1 1.0) (2 2.0) (3 3.0) (4 0.0))))
  (assert-true (equal (difference-function '((2 2.0) (4 4.0))
                                           '((1 1.0) (3 3.0)))
                      '((1 -1.0) (2 -2.0) (3 -3.0) (4 0.0)))))

(define-test test/tree-reduce-bdd
  (dolist (density '(0.2 0.5 1.0))
    (loop :for n :from 2 to 9
	  :for bool-comb = (cl-robdd-analysis::random-boolean-combination n :density density)
	  :for dnf-linear = (let ((cl-robdd::*bdd-reduce-function* #'reduce))
			      (bdd-with-new-hash ()
				(bdd-to-dnf (bdd bool-comb))))
	  :for dnf-tree = (let ((cl-robdd::*bdd-reduce-function* #'tree-reduce))
			    (bdd-with-new-hash ()
			      (bdd-to-dnf (bdd bool-comb))))
	  :unless (equal dnf-linear dnf-tree)
	    :do (assert nil (bool-comb dnf-linear dnf-tree)
			"created 2 different dnf representations from ~A~%      reduce: ~A~% tree-reduce: ~A~%"
			bool-comb dnf-linear dnf-tree)
	  :do (assert-true (equal dnf-linear dnf-tree)))))
