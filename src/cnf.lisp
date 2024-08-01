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

(defun bdd-std-numerical-cmp (num1 num2)
  (cond ((= num1 num2)
         '=)
        ((< num1 num2)
         '<)
        ((> num1 num2)
         '>)
        (t
         (error "cannot compare ~A and ~A" num1 num2))))

(labels ((clause-to-bdd (obj inner-op)
           (etypecase obj
             (bdd obj)
             (list
              ;; convert list of integers into a bdd
              (bdd (cons inner-op
                         (mapcar (lambda (num)
                                   (if (plusp num)
                                       num
                                       (list 'not (abs num))))
                                 obj))))))
         (numerical-[cd]nf-to-bdd  (clauses inner-op outer-op stop-when initial-value)
           (tree-reduce outer-op clauses
                        :key (lambda (obj)
                               (clause-to-bdd obj inner-op))
                        :stop-when stop-when
                        :initial-value initial-value)))

  (defun numerical-cnf-to-bdd (clauses)
    "CLAUSES is a list of sublists, each called a clause.
 A clause is a list of integers, i, either positive or negative.  (abs i)
 represents a literal form. If i<0, the literal is interpreted as negated.
 The clause consists of zero or more such integers, which are interpreted 
 as the disjunction of the conjunctive clauses.  E.g., (1 -2 3) represents (x1 + !x2 + x3).
 CLAUSES represents a conjunction such as (and clause1 clause2 ...)."
    (numerical-[cd]nf-to-bdd clauses 'or #'bdd-and
                             *bdd-false* ;; when calculating AND stop when false
                             *bdd-true*  ;; when calculating AND start with true
                             ))
  
  (defun numerical-dnf-to-bdd (clauses)
    "CLAUSES is a list of sublists, each called a clause.
 A clause is a list of integers, i, either positive or negative.  (abs i)
 represents a literal form. If i<0, the literal is interpreted as negated.
 The clause consists of zero or more such integers, which are interpreted
 as the conjunction of the disjunctive clauses.  E.g., (1 -2 3) represents (x1 & !x2 & x3).
 CLAUSES represents a disjunction such as (or clause1 clause2 ...)."
    (numerical-[cd]nf-to-bdd clauses 'and #'bdd-or
                             *bdd-true* ;; when calculating OR stop when true
                             *bdd-false* ;; when calculating OR start with false
                             )))
                 
(defun comb (n m &aux (acc 1))
  (declare (type unsigned-byte n m))
  ;; how many ways to chose m items from a population of n
  ;; n * (n-1) * ... (n-m+1)
  ;;  (comb 10 9) = 10
  ;;  (comb 10 8) = 10 * 9
  ;;  (comb 10 7) = 10 * 9 * 8
  (assert (<= m n))
  (loop :for i :from (1+ m) :to n
        :do (setf acc (* acc i)))
  acc)
               
(defun random-cnf-clauses (num-vars num-clauses terms-per-clause)
  "Return a list of distinct clauses, where each clause is a list of
 integers between (- num-vars) and num-vars, excluding 0, with the
 restriction that if n is in the clause, then -n is not in the clause.
 each such clause represents a clause in a CNF form where (abs n)
 represents a literal and -(abs n) represents the negated literal."
  (assert (< 0 num-vars))
  (assert (<= terms-per-clause num-vars))
  (assert (<= num-clauses (* (comb num-vars terms-per-clause) (expt 3 num-vars))))
  (labels ((random-var ()
             ;; if num-vars is 13, we need to chose between 1 and 13, not between 0 and 12
             (1+ (random num-vars)))
           (randomly-negate (num)
             (if (= 0 (random 2))
                 num
                 (- num)))
           (random-clause (&aux (remaining terms-per-clause) clause)
             (loop :while (plusp remaining)
                   :for var = (random-var)
                   :unless (member var clause :key #'abs)
                     :do (progn (decf remaining)
                                (push (randomly-negate var) clause)))
             (sort clause #'< :key #'abs)))
    (let ((remaining num-clauses)
          clauses)
      (loop :while (plusp remaining)
            :for clause = (random-clause)
            :unless (member clause clauses :test #'equal)
              :do (progn (decf remaining)
                         (push clause clauses)))
      clauses)))

(defun random-cnf-sat-p (num-vars num-clauses terms-per-clause)
  (the bdd (numerical-cnf-to-bdd (random-cnf-clauses num-vars num-clauses terms-per-clause))))

(defun cnf-statistics (&key num-vars num-clauses terms-per-clause num-samples)
  (bdd-with-new-hash (&aux (num-sat 0) (*bdd-cmp-function* #'bdd-std-numerical-cmp))
    (dotimes (_ num-samples)
      (unless (eql *bdd-false* (random-cnf-sat-p num-vars num-clauses terms-per-clause))
        (incf num-sat)))
    (values (float (/ num-sat num-samples))
            num-sat num-samples)))


;;  LocalWords:  McCluskey mccluskey downto destructuring vec DNF CNF
;;  LocalWords:  maxterms minterms cnf dnf MERCHANTABILITY sublicense
;;  LocalWords:  NONINFRINGEMENT etypecase disjunction bdd cond plusp
;;  LocalWords:  mapcar setf expt dolist pushnew aref eql gethash eq
;;  LocalWords:  removef plists pos qm nconc acc subsetp cdr nconc
;;  LocalWords:  dotimes incf
