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
 The clause consists of zero or more such integers, which are interpeted 
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
 The clause consists of zero or more such integers, which are interpeted 
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
  (the bdd (numerical-cnf-to-bdd (random-cdf-clauses num-vars num-clauses terms-per-clause))))

(defun cnf-statistics (&key num-vars num-clauses terms-per-clause num-samples)
  (bdd-with-new-hash (&aux (num-sat 0) (*bdd-cmp-function* #'bdd-std-numerical-cmp))
    (dotimes (_ num-samples)
      (unless (eql *bdd-false* (random-cnf-sat-p num-vars num-clauses terms-per-clause))
        (incf num-sat)))
    (values (float (/ num-sat num-samples))
            num-sat num-samples)))

(defun quine-mccluskey-reduce (num-vars clauses &key (form :cnf))
  "Given a list of CLAUSES which represent a CNF form,  apply phase-1 of the
 Quine McCluskey method to reduce terms such as (a+b)(a+b')->a
 In addition, (a+b)(a+b+c)->(a+b) is also done.
 :FORM can be used to specify :dnf (DNF form)."
  ;; the QM method is implemented as follows
  ;; clauses is a list such as ((1 2 3) (-1 2 3) (2 4) (1 -2 4 5) (1 -2 -4 5) (-1 2 3 4 5) (1 2 3 4 5))
  ;;    which has the meaning  (and (or x1 x2 x3)       ; (1 2 3)
  ;;                                (or (not x1) x2 x3) ; (-1 2 3)
  ;;                                (or x2 x4)          ; (2 4)
  ;;                                (or x1 (not x2) x4 x5) ; (1 -2 4 5)
  ;;                                (or (not a) b c d e) ; (-1 2 3 4 5)
  ;;                                (or a b c d e)       ; (1 2 3 4 5)
  ;;                                )
  ;; We assume each clause has already been sorted by increasing absolute value (1 -2 3), not (-2 1 3)
  ;; 1. We sort the clauses according to number of positive elements. e.g., (1 -2 -3) < (-1 2 -3 4) < (1 2 3 -4 -5).
  ;;    This is done in function group-clauses which creates a vector, vec, such that vec[i] is the
  ;;    list of all clauses which have i positive elements.
  ;; 2. We traverse over vec num-vars - 1 times in reverse order,
  ;;      first  pass from i = num-vars downto 1,
  ;;      second pass from i = num-vars - 1 downto 1,
  ;;      third  pas  from i = num-vars - 2 downto 1.
  ;;    During each pass we compare each element of vec[i] and with each element of vec[i-1] (quadratic search),
  ;;    (this is somewhat inefficient because vec[i] and vec[i-1] contain clauses of different lengths,
  ;;      this problem should be optimized away in a future release).
  ;;    to find clauses which are compatible, meaning they are the same length and corresponding elements
  ;;    have the same absolute value.  This implies they differ by exactly one entry (because every clause in vec[i]
  ;;    has exactly i positive numbers, and every clause in vec[i-1] has exactly i-1 positive elements).
  ;;    When c1 and c2 are found to be compatible, we schedule them to be removed from vec[i] and vec[i-1]
  ;;    and schedule a new element to be added to vec[i-1], that clause simply removes the element that is different.
  ;;    E.g.,  c1 = (1 -2 3 -4)  ; in vec[2]
  ;;           c2 = (1  2 3 -4)  ; in vec[3]
  ;;     remove c1 from vec[2], remove c2 from vec[3]
  ;;     and add (1 3 -4) to vec[2].
  ;;     This adding and removal is delayed so as not to interfere with the iteration.
  ;;   Each of these backward traversals over vec, (perhaps) removes some clauses of size m and m-1 and
  ;;     adds clauses of size m for various sizes of m.
  ;; 3. The QM method has two phases, the one described in #2, and a second which we omit.  This second phase
  ;;     would normally compare the newly derived clauses to the original clauses to decide which subset
  ;;     of the original set must be retained.   A possibly interesting future enhancement to this algorithm
  ;;     would be to implement QM phase 2.
  ;; 4. The algorithm described thus far (steps 1 and 2) make a big simplification of the QM method.
  ;;    QM normally starts with minterms (or maxterms) i.e., with clauses which are all the same size.
  ;;    However, we start with clauses which are potentially many different sizes.
  ;;    This means that some reductions of standard-QM will be missed.
  ;;    We somewhat compensate for this with an additional phase implemented by the remove-supers local
  ;;    function, in which we search for clauses which are totally implied by another clause
  ;;    E.g., (a+b+c+d) is implied by (b+d), we we can remove (a+b+c+d)
  ;;    in terms of integers, if the clause list contains (... (2 4) ... (1 2 3 4) ...)
  ;;    then we can remove (1 2 3 4)
  ;;    This same reduction works when the clauses is considered a CNF clause or a DNF clause.
  ;;       because also (bd)+(abcd) = bd  as well as (b+c)(a+b+c+d)=(b+c) by duality.
  (declare (type (member :cnf :dnf :raw) form)
           (type (and fixnum unsigned-byte) num-vars))
  (labels ((sort-clause (clause)
             (sort clause #'< :key #'abs))
           (count-positive (clause)
             (count-if (lambda (var)
                         (plusp var)) clause))
           (group-clauses (&aux (vec (make-array (1+ num-vars) :initial-element nil)))
             (loop :for pair :in (sort (group-by clauses :key #'count-positive) #'> :key #'car)
                   :do (destructuring-bind (length clauses) pair
                         (setf (aref vec length) (mapcar #'sort-clause clauses))))
             vec)
           (compatible? (clause1 clause2)
             ;; returns Boolean indicating whether all the corresponding items are equal in absolute value
             ;; and the lists are the same length.
             (cond
               ((not clause1)
                ;; did we reach the end of both lists at the same time?  then compatible yes.
                (not clause2))
               ((not clause2)
                nil ;;(not clause1)
                )
               (t
                (and (= (abs (car clause1))
                        (abs (car clause2)))
                     (compatible? (cdr clause1) (cdr clause2))))))
           (reduce-one-var (clause1 clause2)
             ;; given two compatible (according to compatible?) clauses, return the list of
             ;;   equal elements, ie removing elements which agree in value but differ in absolute-value.
             ;;   only one such element should be removed.
             (mapcan (lambda (v1 v2)
                       (if (= v1 v2)
                           (list v1)
                           nil)) clause1 clause2))
           (reduce-pass (top-index vec)
             (let (add remove)
               (loop :for i :from top-index :downto 1
                     :do (dolist (clause-1 (aref vec i))
                           (dolist (clause-2 (aref vec (1- i)))
                             (when (compatible? clause-1 clause-2)
                               (pushnew clause-1 remove :test #'equal)
                               (pushnew clause-2 remove :test #'equal)
                               (pushnew (reduce-one-var clause-1 clause-2) add)))))
               (cond
                 ((or remove add)
                  (dolist (clause remove)
                    (let ((i (count-positive clause)))
                      (setf (aref vec i) (remove clause (aref vec i) :test #'equal))))
                  (dolist (clause add)
                    (pushnew clause (aref vec (count-positive clause))  :test #'equal))
                  (reduce-pass (1- top-index) vec))
                 (t
                  (loop :for i :from 0 :to num-vars
                        :nconc (aref vec i))))))
           (remove-supers (clauses acc)
             (cond
               ((null clauses)
                (reverse acc))
               (t
                (remove-supers (cdr clauses)
                               (if  (exists c2 (cdr clauses)
                                      (subsetp c2 (car clauses)))
                                    acc
                                    (cons (car clauses) acc)))))))
    (case form
      ((:cnf :dnf)
       (remove-supers (reverse (reduce-pass num-vars (group-clauses)))
                      nil))
      ((:raw)
       (reduce-pass num-vars (group-clauses))))))

;;  LocalWords:  McCluskey mccluskey downto destructuring vec DNF CNF
;;  LocalWords:  maxterms minterms
