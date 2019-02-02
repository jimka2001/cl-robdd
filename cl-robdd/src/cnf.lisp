;; Copyright (c) 2019 EPITA Research and Development Laboratory
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

(in-package   :cl-robdd)

(defun numerical-cnf-to-bdd (clauses)
  "CLAUSES is a list of sublists, each called a clause.
 A clause is a list of integers, i, either positive or negative.  (abs i)
 represents a literal form. If i<0, the literal is interpreted as negated.
 The clause consists of zero or more such integers, which are interpeted 
 as the disjunction of the clauses.  E.g., (1 -2 3) represents (x1 + !x2 + x3).
 CLAUSES represents a conjunction such as (and clause1 clause2 ...)."
  (labels ((clause-to-bdd (obj)
             (etypecase obj
               (bdd obj)
               (list
                ;; list of integers to bdd
                (bdd (cons 'or
                           (mapcar (lambda (num)
                                     (if (plusp num)
                                         num
                                         (list 'not (abs num))))
                                   obj))))))
         (to-bdd (obj1 obj2)
           (bdd-and (clause-to-bdd obj1) (clause-to-bdd obj2))))
    (tree-reduce #'to-bdd (cons *bdd-true* clauses)
                 :initial-value *bdd-true*)))
                 
               
