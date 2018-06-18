;; Copyright (c) 2018 EPITA Research and Development Laboratory
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


(defpackage :cl-robdd-test
  (:use :cl :cl-robdd)
  )
  
(in-package :cl-robdd-test)


(let ((package-into (find-package  :cl-robdd-test))
      (package-from (find-package  :cl-robdd)))
  (do-symbols (name package-from)
    (when (and (eq package-from (symbol-package name))
               (not (find-symbol (symbol-name name) package-into)))
      (format t "importing name=~A into ~S ~%" name pacakge-into)
      (shadowing-import name package-into))))



(define-test test/bdd-and
  (let ((test-vectors (list (list *bdd-false* *bdd-false* *bdd-false*)
                            (list *bdd-false* *bdd-true*  *bdd-false*)
                            (list *bdd-true*  *bdd-false* *bdd-false*)
                            (list *bdd-true*  *bdd-true*  *bdd-true*))))
    (bdd-with-new-hash ()
      (dolist (test-vector test-vectors)
        (destructuring-bind (a b f) test-vector
          (assert-true (eq f (bdd-and a b))))))))

(define-test test/bdd-or
  (let ((test-vectors (list (list *bdd-false* *bdd-false* *bdd-false*)
                            (list *bdd-false* *bdd-true*  *bdd-true*)
                            (list *bdd-true*  *bdd-false* *bdd-true*)
                            (list *bdd-true*  *bdd-true*  *bdd-true*))))
    (bdd-with-new-hash ()
      (dolist (test-vector test-vectors)
        (destructuring-bind (a b f) test-vector
          (assert-true (eq f (bdd-or a b))))))))

(define-test test/bdd-and-not
  (let ((test-vectors (list (list *bdd-false* *bdd-false* *bdd-false*)
                            (list *bdd-false* *bdd-true*  *bdd-false*)
                            (list *bdd-true*  *bdd-false* *bdd-true*)
                            (list *bdd-true*  *bdd-true*  *bdd-false*))))
    (bdd-with-new-hash ()
      (dolist (test-vector test-vectors)
        (destructuring-bind (a b f) test-vector
          (assert-true (eq f (bdd-and-not a b))))))))

(define-test test/bdd-xor
  (let ((test-vectors (list (list *bdd-false* *bdd-false* *bdd-false*)
                            (list *bdd-false* *bdd-true*  *bdd-true*)
                            (list *bdd-true*  *bdd-false* *bdd-true*)
                            (list *bdd-true*  *bdd-true*  *bdd-false*))))
    (bdd-with-new-hash ()
      (dolist (test-vector test-vectors)
        (destructuring-bind (a b f) test-vector
          (assert-true (eq f (bdd-xor a b))))))))


(define-test test/bdd-xor-2
  (let ((test-operands '((z1 z2)
                         ((or z1 z2) z3)
                         ((or z1 z2) (or z1 z3))
                         ((or (not z1) z2) (or z1 z3))
                         ((or z1 z2) (or z1 (not z3)))
                         ((or z1 (not z2)) (or z1 z3))
                         ((or z1 (not z2)) (or (not z1) z3))
                         ((or z1 z3 z3 z4 z5 z6 z7) (or z3 z4 z5 z6 z7 z8 z9))
                         )))
    (bdd-with-new-hash ()
      (dolist (test-operand test-operands)
        (destructuring-bind (a b) (mapcar #'bdd test-operand)
          (assert-true (eq (bdd-or (bdd-and-not a b) (bdd-and-not b a))
                           (bdd-xor a b)))
          (assert-true (eq (bdd-and-not (bdd-or a b) (bdd-and a b))
                           (bdd-xor a b)))
          (assert-true (eq (bdd-xor b a)
                           (bdd-xor a b)))
)))))
          
