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

(in-package :cl-robdd-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow-all-symbols :package-from :cl-robdd
		      :package-into :cl-robdd-test))

(define-test test/numerical-cnf-to-bdd
  (bdd-with-new-hash (&aux (*bdd-cmp-function* #'bdd-std-numerical-cmp))
    (let ((bdd1 (numerical-cnf-to-bdd '((1 2) (-2 3))))
          (bdd2 (numerical-cnf-to-bdd '((-1 3) (2 -3) (-2 -3)))))
      (assert-false (eql *bdd-false* bdd1))
      (assert-false (eql *bdd-false* bdd2))
      (assert-true  (eql *bdd-false* (bdd-and bdd1 bdd2))))))

(define-test test/random-cnf-clauses
  (bdd-with-new-hash (&aux (*bdd-cmp-function* #'bdd-std-numerical-cmp))
    (assert-true (member (random-cnf-clauses 1 2 1)
                         '(((-1) (1))
                           ((1) (-1)))
                         :test #'equal))))

(define-test test/comb
  ;; how many ways to chose m items from a population of n
  ;; n * (n-1) * ... (n-m+1)
  ;;  (comb 10 9) = 10
  ;;  (comb 10 8) = 10 * 9
  ;;  (comb 10 7) = 10 * 9 * 8
  (assert-true (= 1 (comb 10 10)))
  (assert-true (= 10 (comb 10 9)))
  (assert-true (= 90 (comb 10 8)))
  (assert-true (= (* 10 9 8) (comb 10 7))))
  

(define-test test/cmp-clauses
  ;; TODO finish this test
  (assert-true (equal nil
                      (sort nil #'clause-<)))
  (assert-true (equal '((1))
                      (sort (copy-list '((1))) #'clause-<)))
  (assert-true (equal '((-1) (1))
                      (sort (copy-list '((1) (-1))) #'clause-<)))
  (assert-true (equal '((-1) (1))
                      (sort (copy-list '((-1) (1))) #'clause-<)))
  (assert-true (equal '((-1 3) (1 2))
                      (sort (copy-list '((1 2) (-1 3))) #'clause-<)))
  (assert-true (equal '((-1 3) (1 2) (1 3) (-2 2) (2 1) )
                      (sort (copy-list '((1 2) (-2 2) (-1 3) (1 3) (2 1))) #'clause-<)))
  (assert-true (equal '((-1 3) (1 2))
                      (sort (copy-list '((-1 3) (1 2))) #'clause-<))))
  

(define-test test/quine-mccluskey-reduce-2
  (assert-true (equal '((1)) (quine-mccluskey-reduce '((1 -2) (1 2 3) (1 2 -3))))))

(define-test test/quine-mccluskey-reduce
  (dolist (clauses '(((1) (-1))
                     ((1) (2) (3) (1 2 -3))
                     ((1 2 3 4)
                      (1 2 3 5)
                      (-1 2 3 4)
                      (1 -2 3 4)
                      (1 2 -3 4)
                      (1 2 3 -5)
                      (-1 -2 3 4)
                      (-1 2 -3 4)
                      (-1 2 3 -4)
                      (1 2 -3 -5))
                     ((1 2) (-1 2))
                     ((-1 2 3 6) (-1 2 3) (1 -2 -4 5 -6) (1 -2 -4 5 6) (1 -2 -4 5) (1 -2 -4 5) (1 -2 -6) (1 -2 3 -6) (2 4))
                     ((1 2 3) (-1 2 3) (2 4) (1 -2 4 5) (1 -2 -4 5) (-1 2 3 4 5) (1 2 3 4 5))
                     ((1 2 3) (-1 2 3) (2 4) (1 -2 4 5) (1 3 -5) (1 -2 -4 5) (-1 2 3 4 5) (1 2 3 4 5))
                     ((1 2) (1 -2))
                     ((1 2 3) (1 2 -3) (1 2) (2 -3))
                     (
                      (-1 2 -3 6)
                      (-1 2 -3)
                      (-1 2 3 4 -5 6)
                      (-1 2 3 4 -5)
                      (-1 2 3 4 5 6)
                      (-1 2 3 4 5)
                      (-1 2 3 6)
                      (-1 2 3)
                      (1 -2 -4 5 -6)
                      (1 -2 -4 5 6)
                      (1 -2 -4 5)
                      (1 -2 -4 5)

                      (2 4))

                     (
                      (-1 2 -3 6)
                      (-1 2 -3)
                      (-1 2 3 4 -5 6)
                      (-1 2 3 4 -5)
                      (-1 2 3 4 5 6)
                      (-1 2 3 4 5)
                      (-1 2 3 6)
                      (-1 2 3)
                      (1 -2 -4 5 -6)
                      (1 -2 -4 5 6)
                      (1 -2 -4 5)
                      (1 -2 -4 5)
                      (1 -2 -6)
                      (1 -2 3 -6)
                      (2 4))

                     (
                      (-1 2 -3 6)
                      (-1 2 -3)
                      (-1 2 3 4 -5 6)
                      (-1 2 3 4 -5)
                      (-1 2 3 4 5 6)
                      (-1 2 3 4 5)
                      (-1 2 3 6)
                      (-1 2 3)
                      (1 -2 -4 5 -6)
                      (1 -2 -4 5 6)
                      (1 -2 -4 5)
                      (1 -2 -4 5)
                      (1 -2 -6)
                      (1 -2 3 -6)
                      (1 -2 3 4 5 -6)
                      (1 -2 3 4 5)
                      (1 -2 3)
                      (1 -2 4 5 -6)
                      (1 -2 4 5 -6)
                      (1 -2 4 5 7)
                      (1 -2 4 5)
                      (1 -2)
                      (1 2 -3 -6)
                      (1 2 -3 7)
                      (1 2 -3)
                      (1 2 -5)
                      (1 2 3 4 5)
                      (1 2 3)
                      (1 2 5)
                      (1 3 -5)
                      (2 -3)
                      (2 4 5)
                      (2 4))))
    (bdd-with-new-hash (&aux (*bdd-cmp-function* #'bdd-std-numerical-cmp))
      (labels ((f (clauses)
                 (unless (eql (numerical-cnf-to-bdd (quine-mccluskey-reduce clauses))
                              (numerical-cnf-to-bdd clauses))
                   (when (cdr clauses)
                     (dolist (clause clauses)
                       (f (remove clause clauses)))))

                 (when (cdr clauses)
                   (f (cdr clauses)))
                 ;; (format t "clauses ~A~%" clauses)
                 ;; (format t "qm-raw ~A~%" (quine-mccluskey-reduce 7 clauses :form :raw))
                 ;; (format t "qm-cnf ~A~%" (quine-mccluskey-reduce 7 clauses :form :cnf))

                 (assert-true (eql (numerical-cnf-to-bdd (quine-mccluskey-reduce clauses))
                                   (numerical-cnf-to-bdd clauses)))
                 (assert-true (eql (numerical-dnf-to-bdd (quine-mccluskey-reduce clauses :form :raw))
                                   (numerical-dnf-to-bdd clauses)))

                 (assert-true (eql (numerical-cnf-to-bdd (quine-mccluskey-reduce clauses))
                                   (numerical-cnf-to-bdd clauses)))

                 (assert-true (eql (numerical-dnf-to-bdd (quine-mccluskey-reduce clauses :form :dnf))
                                   (numerical-dnf-to-bdd clauses)))))
        (f clauses)
        ))))

(define-test test/compatible
  (assert-false (qm-compatible? '(-1 2 3 4) '(1 -2 3 -4)))
  (assert-false (qm-compatible? '(1 2) '(-1 -2)))
  (assert-false (qm-compatible? '(-1 2) '(1 3)))
  (assert-false (qm-compatible? '(1 2) '(-1 3)))
  (assert-false (qm-compatible? '(1 3) '(1 2)))
  (assert-false (qm-compatible? '(1 3) '(-1 2)))
  (assert-false (qm-compatible? '(-1 3) '(1 2)))
  (assert-false (qm-compatible? '(-1 3) '(1 2)))

  (assert-true (qm-compatible? '(1 2) '(-1 2)))
  (assert-true (qm-compatible? '(1 -2) '(1 2)))
  (assert-true (qm-compatible? '(-1 2) '(1 2)))
  (assert-true (qm-compatible? '(1 2) '(1 -2))))

(define-test test/read-dimacs-file
  (dolist (string '(
                    ;; example from https://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html
                    "c  simple_v3_c2.cnf
c
p cnf 3 2
1 -3 0
2 3 -1 0"
                    ;; missing final 0
                    "c  simple_v3_c2.cnf
c
p cnf 3 2
1 -3 0
2 3 -1 "
                    ;; space at the end of some lines
                    "c  simple_v3_c2.cnf
c
p cnf 3 2
1 -3 0   
2 3 -1 "
                    ;; missing final 0 and space
                    "c  simple_v3_c2.cnf
c
p cnf 3 2
1 -3 0
2 3 -1"
                    ;; missing newline between clauses
                    "c  simple_v3_c2.cnf
c
p cnf 3 2
1 -3 0 2 3 -1 0"
                    ))
    (with-input-from-string (stream string)
      (assert-false (set-exclusive-or '((1 -3) (2 3 -1))
                                      (read-dimacs-file stream) :test (lambda (x y)
                                                                     (and (subsetp x y)
                                                                          (subsetp y x)))))))
  (with-input-from-string (stream "c  simple_v3_c2.cnf
c
p cnf 3 2
1 -3 0 2 3 -1 0")
    (assert-true (equal '((1 -3) (2 3 -1))
                        (read-dimacs-file stream))))

  (let ((count 0))
    (with-input-from-string (stream "c  simple_v3_c2.cnf
c
p cnf 3 2
1 -3 0 2 3 -1 0")
      (read-dimacs-file stream :consume (lambda (clause)
                                       (declare (ignore clause))
                                       (incf count))))
    (assert-true (equal 2 count)))
  )

(define-test test/read-dimacs-file-2
  (let* ((cl-robdd-test-system (asdf:find-system :cl-robdd-test))
         (cl-robdd-test-path (asdf:component-pathname cl-robdd-test-system))
         (data-path (pathname-as-directory (merge-pathnames cl-robdd-test-path "data")))
         (sample-files '("aim-100-1_6-no-1.cnf"
                         "aim-50-1_6-yes1-4.cnf"
                         "bf0432-007.cnf"
                         "dtba-sat.cnf"
                         "dubois20.cnf"
                         "dubois21.cnf"
                         "dubois22.cnf"
                         "hole6.cnf"
                         "par8-1-c.cnf"
                         "quinn.cnf"
                         "sat-33ZzxW.cnf"
                         ;; "sat-dMt1DH.cnf" ; too big
                         "simple_v3_c2.cnf"
                         "zebra_v155_c1135.cnf")))
    (dolist (sample sample-files)
      (let ((fname (merge-pathnames data-path sample)))
        (read-dimacs-file fname)
        (dimacs-to-vec fname)
        (quine-mccluskey-reduce (dimacs-to-vec fname))))))
                         
  
