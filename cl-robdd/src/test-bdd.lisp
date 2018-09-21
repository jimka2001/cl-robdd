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
  (:use :cl :cl-robdd :scrutiny
	)
  )
  
(in-package :cl-robdd-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow-all-symbols :package-from :cl-robdd :package-into :cl-robdd-test))

(defun test ()
  (run-package-tests :cl-robdd-test))


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
          
(define-test test/bdd-cmp
  (bdd-with-new-hash ()
    ;; =
    (assert-true (eq '= (bdd-cmp 'a 'a)))
    (assert-true (eq '= (bdd-cmp "a" "a")))
    (assert-true (eq '= (bdd-cmp 1 1)))
    (assert-true (eq '= (bdd-cmp 1.0 1.0)))
    (assert-true (eq '= (bdd-cmp 1/2 1/2)))
    (assert-true (eq '= (bdd-cmp nil nil)))
    (assert-true (eq '= (bdd-cmp '(a 1 1.0) '(a 1 1.0))))

    ;; <
    (assert-true (eq '< (bdd-cmp "CL-USER" "KEYWORD")))
    (assert-true (eq '< (bdd-cmp 'CL-USER::x :x)))
    (assert-true (eq '< (bdd-cmp '(a b c) '(a b c d))))
    (assert-true (eq '< (bdd-cmp '(a 1 c) '(a 2 c d))))
    (assert-true (eq '< (bdd-cmp '(a 1 c d) '(a 2 c))))
    (assert-true (eq '< (bdd-cmp 'string 'symbol)))
    ;; (assert-true (eq '< (bdd-cmp "string" 'symbol)))
    (assert-true (eq '< (bdd-cmp 'cons 'null)))
    (assert-true (eq '< (bdd-cmp nil '(a))))
    (assert-true (eq '< (bdd-cmp 1/3 1/2)))

    ;; >
    (assert-true (eq '> (bdd-cmp "KEYWORD" "CL-USER")))
    (assert-true (eq '> (bdd-cmp :x 'CL-USER::x)))
    (assert-true (eq '> (bdd-cmp '(a b c d) '(a b c))))
    (assert-true (eq '> (bdd-cmp '(a 2 c d) '(a 1 c))))
    (assert-true (eq '> (bdd-cmp '(a 2 c) '(a 1 c d))))
    (assert-true (eq '> (bdd-cmp 'symbol 'string)))
    ;; (assert-true (eq '> (bdd-cmp 'symbol "string")))
    (assert-true (eq '> (bdd-cmp 'null 'cons)))
    (assert-true (eq '> (bdd-cmp '(a) nil)))
    (assert-true (eq '> (bdd-cmp 1/2 1/3)))
    )  )




