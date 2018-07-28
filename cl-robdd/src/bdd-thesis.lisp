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

(in-package :cl-robdd)

(defun bdd-xor-list (&rest bdd-list)
  (reduce #'bdd-xor bdd-list :initial-value *bdd-false*))

(defun bdd-or-list  (&rest bdd-list)
  (reduce #'bdd-or bdd-list :initial-value *bdd-false*))

(defun bdd-and-list (&rest bdd-list)
  (reduce #'bdd-and bdd-list :initial-value *bdd-true*))

(defun bdd-and-not-list (bdd1 &rest bdd-list)
  (bdd-and-not bdd1
	       (apply #'bdd-and-list bdd-list)))

(defun bdd-dnf-to-bdd (expr)
  (typecase expr
    ((eql t)
     *bdd-true*)
    ((eql nil)
     *bdd-false*)
    (symbol
     (bdd-node expr *bdd-true* *bdd-false*))
    ((not list)
     (error "invalid expression ~A" expr))
    (t
     (apply 
       (case (car expr)
	 ((not)     #'bdd-not)
	 ((and)     #'bdd-and-list)
	 ((or)      #'bdd-or-list)
	 ((and-not) #'bdd-and-not-list)
	 ((xor)     #'bdd-xor-list)
	 (t (error "invalid expression ~A" expr)))
       (mapcar #'bdd-dnf-to-bdd (cdr expr))))))

(defun bdd-to-dnf (bdd)
  (let (or-terms)
    (labels ((partial-dnf (node and-term)
	       (typecase node
		 (bdd-true
		  (push (cons 'and (reverse and-term)) or-terms))
		 (bdd-false nil)
		 (bdd-node
		  (partial-dnf (bdd-positive node) (cons (bdd-label node) and-term))
		  (partial-dnf (bdd-negative node) (cons `(not ,(bdd-label node)) and-term))))))
      (partial-dnf bdd nil)
      (cons 'or or-terms))))

(defun bdd (expr)
  (typecase expr
    ((eql t)
     *bdd-true*)
    ((eql nil)
     *bdd-false*)
    (symbol
     (bdd-node expr *bdd-true* *bdd-false*))
    ((not list)
     (error "invalid expression ~A" expr))
    (t
     (apply 
       (case (car expr)
	 ((not)     #'bdd-not)
	 ((and)     #'bdd-and-list)
	 ((or)      #'bdd-or-list)
	 ((and-not) #'bdd-and-not-list)
	 ((xor)     #'bdd-xor-list)
	 (t (error "invalid expression ~A" expr)))
       (mapcar #'bdd (cdr expr))))))

(defvar *bdd-hash* nil "The default value of *bdd-hash* is NOT a hash table")

(defun bdd-call-with-new-hash (thunk)
  (let ((*bdd-hash* (make-hash-table :test #'equal)))
    (funcall thunk)))

(defmacro bdd-with-new-hash (&body body)
   `(bdd-call-with-new-hash (lambda () ,@body)))



(defun bdd-call-with-new-hash (thunk)
  (let ((*bdd-hash* (make-hash-table :test #'equal)))
    (funcall thunk)))

(defun bdd-call-with-new-hash-strong (thunk)
  (let ((*bdd-hash* (make-hash-table :test #'equal)))
    (funcall thunk)))

(defun bdd-call-with-new-hash-weak (thunk)
  (let ((*bdd-hash* (make-hash-table :test #'equal
				     :weakness :value)))
    (funcall thunk)))

(defun bdd-call-with-new-hash-weak-dynamic (thunk)
  (if *bdd-hash*
      (funcall thunk)
      (bdd-call-with-new-hash-weak thunk)))
