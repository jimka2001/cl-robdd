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

(defun equal-abs-list (clause-1 clause-2)
  "assuming two lists have the same length, are corresponding elements equal in absolute value?"
  (every (lambda (x y)
           (eql (abs x) (abs y)))
         clause-1 clause-2))

(defun qm-compatible? (clause1 clause2 &optional (diff 0))
  ;; Returns Boolean indicating whether all the corresponding items are equal in absolute value,
  ;; but the lists differ by exactly one value,
  ;; We have already verified (assured that) clause1 and clause2 have the same length
  (declare (type (and fixnum unsigned-byte) diff)
           (type (or null (cons fixnum)) clause1 clause2)
           (optimize (speed 3) (debug 0) (compilation-speed 0)))
  (cond
    ((> diff 1) ; stop if diff ever exceeds 1
     nil)
    ((null clause1) ; if we reached the end, we should have found exactly 1 difference
     (assert (null clause2))
     (= 1 diff))
    (t
     (and (= (abs (car clause1))
             (abs (car clause2)))
          (qm-compatible? (cdr clause1) (cdr clause2) (if (= (car clause1) (car clause2))
                                                          diff
                                                          (1+ diff)))))))

(defun clause-< (clause1 clause2 &aux (c1 (car clause1)) (c2 (car clause2)))
  "The comparison function used by sort to sort the clauses deterministically.
 The clauses are first ordered by absolute value of first element, and when the absolute
 values are equal, the negatives are sorted before the positives
 (-1 ...) < (1 ...) < (-2 ...) < (2 ...) < (-3 ...) < (3 ...) ...
 If the first elements are equal, a similar test is made on the 2nd element etc."
  (declare (optimize (speed 3) (debug 0) (compilation-speed 0))
           (type (or null fixnum) c1 c2))
  (cond
    ((null clause1)
     nil)
    ((= c1 c2)
     (clause-< (cdr clause1) (cdr clause2)))
    ((= (abs c1) (abs c2))
     (< c1 c2))
    (t
     (< (abs c1) (abs c2)))))

(defclass qm-vec ()
  ((hash :reader pos-count-hash :initform (make-hash-table :test #'eql))
   (form :initarg :form :initform :cnf :reader form :type (member :dnf :cnf :raw))
   (num-vars :initarg :num-vars :type (and fixnum unsigned-byte))))

(defun calc-num-vars (clauses)
  (let ((hash (make-hash-table :test #'eql)))
    (dolist (clause clauses)
      (dolist (num clause)
        (setf (gethash (abs num) hash) t)))
    (hash-table-count hash)))

(defun count-positive (clause)
  (count-if (lambda (var)
              (plusp var))
            clause))

(defgeneric add-clause (vec clause &key pos-count length test-unique sort))
(defmethod add-clause ((vec qm-vec) clause &key (sort t) (test-unique t) (pos-count (count-positive clause)) (length (length clause)))
  (declare (type (and unsigned-byte fixnum) pos-count length))
  (let* ((pos-count-hash (pos-count-hash vec)) ; the hash indexed by pos-count
         (length-hash (or (gethash pos-count pos-count-hash)
                          (setf (gethash pos-count pos-count-hash)
                                (make-hash-table :test #'eql))))) ; the hash indexed by length
    (cond
      ((and (null test-unique)
            (null sort))
       (push clause (gethash length length-hash)))
      ((and test-unique
            (not (member clause (gethash length length-hash) :test #'equal)))
       (setf (gethash length length-hash)
             (merge 'list (list clause) (gethash length length-hash) #'clause-<)))
      (sort
       (setf (gethash length length-hash)
             (merge 'list (list clause) (gethash length length-hash) #'clause-<)))
      (t
       (push clause (gethash length length-hash))))))

(defgeneric sort-vec (vec))
(defmethod sort-vec ((vec qm-vec))
  (declare (optimize (speed 3) (debug 0) (compilation-speed 0)))
  (loop :for length-hash :being :the :hash-values :of (pos-count-hash vec)
        :do (loop :for length :being :the :hash-keys :of length-hash
                  :do (setf (gethash length length-hash)
                            (sort (gethash length length-hash) #'clause-<))
                  :do (labels ((remove-duplicates-sorted (sorted acc)
                                 (cond
                                   ((null sorted)
                                    (nreverse acc))
                                   ((equal (car sorted) (cadr sorted))
                                    (remove-duplicates-sorted (cdr sorted) acc))
                                   (t
                                    (remove-duplicates-sorted (cdr sorted) (cons (car sorted) acc))))))
                        (setf (gethash length length-hash)
                              (remove-duplicates-sorted (gethash length length-hash) nil))))))

(defgeneric remove-clause (vec clause &key pos-count length))
(defmethod remove-clause ((vec qm-vec) clause &key (pos-count (count-positive clause)) (length (length clause)) &aux (hash (gethash pos-count (pos-count-hash vec))))
  (setf (gethash length hash)
        (delete clause (gethash length hash) :test #'eq :count 1)))

(defgeneric get-clauses (vec &key pos-count length))
(defmethod get-clauses ((vec qm-vec) &key pos-count length)
  (declare (type (and fixnum unsigned-byte) pos-count length))
  (let* ((pos-count-hash (pos-count-hash vec))
         (length-hash (gethash pos-count pos-count-hash)))
    (if length-hash
        (gethash length length-hash)
        nil)))

(defgeneric map-clauses (consume vec))
(defmethod map-clauses (consume (vec qm-vec))
  (declare (type (function (list &key (length (and fixnum unsigned-byte)) (pos-count (and fixnum unsigned-byte))) t) consume))
  (loop :for pos-count :being :the :hash-keys :of (pos-count-hash vec)
        :do (maphash (lambda (length clauses)
                       (dolist (clause clauses)
                         (funcall consume clause :length length :pos-count pos-count)))
                     (gethash pos-count (pos-count-hash vec)))))

(defun sort-clause (clause)
  (sort (copy-list clause) #'< :key #'abs))

(defgeneric quine-mccluskey-reduce (obj &key remove-supers &allow-other-keys))
(defmethod quine-mccluskey-reduce ((clauses list) &key (remove-supers t) (form :cnf))
  (let ((vec (make-instance 'qm-vec :form form)))
    (dolist (clause clauses)
      (add-clause vec (sort-clause clause)))
    (quine-mccluskey-reduce vec :remove-supers remove-supers)))

(defmethod quine-mccluskey-reduce ((vec qm-vec) &key (remove-supers t) &allow-other-keys)
  "Given a list of CLAUSES which represent a CNF form,  apply phase-1 of the
 Quine McCluskey method to reduce terms such as (a+b)(a+b')->a
 In addition, (a+b)(a+b+c)->(a+b) is also done."


    ;; (maphash (lambda (pos-count length-hash)
    ;;            (check-type pos-count fixnum)
    ;;            (check-type length-hash hash-table)
    ;;            (maphash (lambda (length clauses)
    ;;                       (check-type length fixnum)
    ;;                       (check-type clauses list)) length-hash))
    ;;          (pos-count-hash vec))

  (declare (optimize (speed 3) (debug 0) (compilation-speed 0)))
  ;; the QM method is implemented as follows
  ;; clauses is a list such as ((1 2 3) (-1 2 3) (2 4) (1 -2 4 5) (1 -2 -4 5) (-1 2 3 4 5) (1 2 3 4 5))
  ;;    which has the meaning  (and (or x1 x2 x3)       ; (1 2 3)
  ;;                                (or (not x1) x2 x3) ; (-1 2 3)
  ;;                                (or x2 x4)          ; (2 4)
  ;;                                (or x1 (not x2) x4 x5) ; (1 -2 4 5)
  ;;                                (or (not a) b c d e) ; (-1 2 3 4 5)
  ;;                                (or a b c d e)       ; (1 2 3 4 5)
  ;;                                )
  ;; 0. We enforce that each clause has already been sorted by increasing absolute value (1 -2 3), not (-2 1 3)
  ;;      this work is done by sort-clause.
  ;;     The given vec (instance of qm-vec) has a hash table which maps the number of positive integers in
  ;;      the cause to another hash table.  The accessor pos-count-hash returns a hash table which can
  ;;      be indexed by a positive interger (or zero) indicating the number of positive elements in the clause.
  ;;      The value corresponding to the pos-count key is a hash table indexable by an integer representing
  ;;      the clause length.   (gethash 5 (gethash 3 (pos-count-hash vec))), roughly hash[3][5],
  ;;      returns a list of clauses, each clause has exactly 5 elements, 3 of which are positive.
  ;;      Each element has already been sorted by increasing absolute value: (1 -2 -3), not (-2 1 3).
  ;;      And the list of clauses has already been sorted by the clause-< function.
  ;;
  ;; 1. We traverse over vec max-num-pos times in reverse order,
  ;;      first  pass from i = max-num-pos downto 1,
  ;;      second pass from i = num-vars - 1 downto 1,
  ;;      third  pas  from i = num-vars - 2 downto 1.
  ;;    During each pass we compare each element of clause-a=vec[i][j] and with each element of clause-b=vec[i-1][j] 
  ;;       only if all the elements of clause-a and the elements of clause-b are the same in absolute value.
  ;;       E.g., clause-a=(2 -4 5 6) clause-b=(2 -4 -5 6)
  ;;       or    clause-a=(-2 4 5 6) clause-b=(2 4 -5 -6)
  ;;       This is a linear search, not a quadratic search.  The reason this is linear is because
  ;;       each list is already sorted (in increasing order) such that we only need to look at the 
  ;;       head of the two lists, and carefully pop off the list whose head is less than the other.
  ;;       This is an important exception.   It might happen that several elements at the head of either
  ;;       or both the lists are equal to each other in abs value,
  ;;            (-1 2 3 4) = (1 -2 3 4) = (1 2 -3 4) = (1 2 3 -4)
  ;;       and  (-1 -2 3 4) = ( 1 2 -3 -4)
  ;;       so we might have to do a small quadratic search.
  ;;       
  ;;    We find clauses which are compatible, meaning they are the same length and corresponding elements
  ;;    have the same absolute value, and exactly one entry differs.
  ;;    When clause-a and clause-b are found to be compatible, we schedule them to be removed from vec[i][j] and vec[i-1][j]
  ;;    and schedule a new element to be added to vec[i-1][j-1], that clause simply removes the element that is different.
  ;;    E.g.,  clause-a = (1  2 3 -4)  ; in vec[3][4]   3 positive and length=4
  ;;           clause-b = (1 -2 3 -4)  ; in vec[2][4]   2 positive and length=4
  ;;     remove clause-a from vec[3][4], remove clause-b from vec[2][4]
  ;;     and add (1 3 -4) to vec[2][3], 2 positive and length=3.
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
  (labels ((reduce-one-var (clause1 clause2)
             ;; given two compatible (according to qm-compatible?) clauses, return the list of
             ;;   equal elements, ie removing elements which agree in value but differ in absolute-value.
             ;;   only one such element should be removed.
             (mapcan (lambda (v1 v2)
                       (declare (type fixnum v1 v2))
                       (if (= v1 v2)
                           (list v1)
                           nil))
                     clause1 clause2))

           (reduce-1 (pos-count add-plist remove-plist)
             (let* ((pos-count-1 (1- pos-count))
                    (length-hash-a (gethash pos-count (pos-count-hash vec)))
                    (length-hash-b (gethash pos-count-1 (pos-count-hash vec))))
               (when (and length-hash-a
                          length-hash-b)
                 ;; (format t "reduce-1 pos-count ~A~%" pos-count)
                 (maphash (lambda (length clauses-a
                                   &aux
                                     (clauses-b (gethash length length-hash-b)))
                            ;; e.g., clauses-a and clauses-b are lists of different lengths
                            ;;  each element of clause-a and clause-b is a list of length L (e.g., L=5)
                            ;;  every element of clause-a has P positive elements
                            ;;  every element of clause-b has P-1 positive elements
                            ;; (format t "    length ~A~%" length)
                            (while (and clauses-a
                                        clauses-b)
                              ;; (when (= 0 (mod (length clauses-a) 5000))
                              ;;(format t " length clauses-a=~D~%" (length clauses-a)))
                              ;;(when (= 0 (mod (length clauses-b) 5000))
                              ;; (format t " length clauses-b=~D~%" (length clauses-b)))
                              ;;(format t "        ~D clauses of length=~D pos-count=~A~%" (length clauses-a) length pos-count)
                              ;;(format t "  ~A~%" clauses-a)
                              ;;(format t "        ~D clauses of length=~D pos-count=~A~%" (length clauses-b) length pos-count-1)
                              ;;(format t "  ~A~%" clauses-b)
                              (let ((clause-a (car clauses-a))
                                    (clause-b (car clauses-b)))
                                (cond
                                  ((equal-abs-list clause-a clause-b)
                                   ;;(format t "  abs-equal-clauses ~A ~A~%" clause-a clause-b)
                                   (let ((clauses-aa (list (pop clauses-a)))
                                         (clauses-bb (list (pop clauses-b))))
                                     (while (and clauses-a
                                                 (equal-abs-list clause-a (car clauses-a)))
                                       ;;(format t "  abs-equal-clauses ~A ~A~%" clause-a (car clauses-a))
                                       (push (pop clauses-a) clauses-aa))
                                     (while (and clauses-b
                                                 (equal-abs-list clause-b (car clauses-b)))
                                       ;;(format t "  abs-equal-clauses ~A ~A~%" clause-b (car clauses-b))
                                       (push (pop clauses-b) clauses-bb))
                                     ;; now a quadratic search, but maximally length^2 times
                                     (dolist (aa clauses-aa)
                                       (dolist (bb clauses-bb)
                                         (when (qm-compatible? aa bb)
                                           (funcall remove-plist aa :length length :pos-count pos-count)
                                           (funcall remove-plist bb :length length :pos-count pos-count-1)
                                           (funcall add-plist (reduce-one-var aa bb)
                                                    :sort nil :test-unique nil :length (1- length) :pos-count pos-count-1))))))
                                  ((clause-< clause-a clause-b)
                                   (pop clauses-a))
                                  (t
                                   (pop clauses-b))))))
                          length-hash-a))))
           
           (qm-reduce ()
             (let ((changed t)
                   remove-plists
                   add-plists
                   (max-pos-count (loop :for pos-count :being :the :hash-keys :of (pos-count-hash vec)
                                        :maximize pos-count)))
               (while changed
                 (setf changed nil)
                 (loop :for pos-count :being :the :hash-keys :of (pos-count-hash vec)
                       :when (<= pos-count max-pos-count)
                         ;; call reduce-1 on all pos-count entries, but remember whether something changed
                         :do (reduce-1 pos-count
                                       (lambda (&rest plist)
                                         ;; TODO we could add immediately, but delay removal
                                         (apply #'add-clause vec plist)
                                         ;;(push plist add-plists)
                                         )
                                       (lambda (&rest plist)
                                         (push plist remove-plists))))
                 (setf changed add-plists)
                 ;;(format t "removing ~D~%" (length remove-plists))
                 (dolist (plist remove-plists)
                   (apply #'remove-clause vec plist))
                 ;;(format t "adding ~D~%" (length add-plists))
                 ;; (dolist (plist add-plists)
                 ;;   (apply #'add-clause vec plist))
                 (setf remove-plists nil
                       add-plists nil)
                 ;; There might be new indices, but nothing larger than max-pos-count.
                 ;; We wish to find the maximum pos-count which is strictly < max-pos-count
                 ;;   and make that the new max-pos-count
                 (setf max-pos-count
                       (loop :for pos-count :being :the :hash-keys :of (pos-count-hash vec)
                             :when (< pos-count max-pos-count)
                               :maximize pos-count)))))
           
           (remove-supers ()
             (destructuring-dolist ((&key pos-count length clause)
                                    (loop
                                      :for pos-count-1 fixnum :being :the :hash-keys :of (pos-count-hash vec)
                                      :for length-1-hash = (gethash pos-count-1 (pos-count-hash vec))
                                      :nconc
                                      (loop
                                        :for pos-count-2 fixnum :being :the :hash-keys :of (pos-count-hash vec)
                                        :for length-2-hash = (gethash pos-count-2 (pos-count-hash vec))
                                        :when (<= pos-count-1 pos-count-2)
                                          ;; don't search for subsets A < B if pos-count(A) > pos-count(B)
                                          :nconc
                                          (loop
                                            :for length-1 fixnum :being :the :hash-keys :of length-1-hash
                                            :nconc
                                            (loop
                                              :for length-2 fixnum :being :the :hash-keys :of length-2-hash
                                              :when (< length-1 length-2) ; strictly less
                                                ;; :do (format t "length: ~A < ~A  pos-count ~A <=~A ~%" length-1 length-2 pos-count-1 pos-count-2)
                                                :nconc
                                                (loop
                                                  :for clause-2 :in (gethash length-2 length-2-hash)
                                                  :when (exists clause-1 (gethash length-1 length-1-hash)
                                                          (subsetp clause-1 clause-2))
                                                    :collect (list :pos-count pos-count-2
                                                                   :length length-2
                                                                   :clause clause-2)))))))
               (remove-clause vec clause :pos-count  pos-count :length length))))

    ;; (format t "reducing~%")
    (qm-reduce)

    (when remove-supers
      (case (form vec)
        ((:cnf :dnf)
         (format t "removing supers~%")
         (remove-supers))
        ((:raw)
         nil)))

    ;;(format t "sorting~%")
    (let (clauses)
      (map-clauses (lambda (clause &key &allow-other-keys)
                     ;;(format t "clause=~A~%" clause)
                     (push clause clauses))
                   vec)
      (sort clauses #'< :key #'length)
      )))

(defun dimacs-to-vec (file)
  (let ((vec (make-instance 'qm-vec)))
    ;; (format t "reading ~A~%" file)
    (read-dimacs-file file
                   :consume (lambda (clause)
                              (add-clause vec clause :test-unique nil :sort nil)))
    ;;(format t "sorting~%")

    ;; (maphash (lambda (pos-count length-hash)
    ;;            (check-type pos-count fixnum)
    ;;            (check-type length-hash hash-table)
    ;;            (maphash (lambda (length clauses)
    ;;                       (check-type length fixnum)
    ;;                       (check-type clauses list)) length-hash))
    ;;          (pos-count-hash vec))
    (sort-vec vec)

    ;; (maphash (lambda (pos-count length-hash)
    ;;            (check-type pos-count fixnum)
    ;;            (check-type length-hash hash-table)
    ;;            (maphash (lambda (length clauses)
    ;;                       (check-type length fixnum)
    ;;                       (check-type clauses list)) length-hash))
    ;;          (pos-count-hash vec))
    vec))

(defun read-dimacs-file (file &key (consume (let ((conc-buf (list nil)))
                                           (lambda (clause)
                                             (tconc conc-buf (reverse clause))
                                             (car conc-buf)))))
  "Read a DIMACS CNF file, as described by https://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html
 The CNF file format is an ASCII file format.

 Each clause encountered in the DIMACS file is passed to a call to the given CONSUME
 function.  The terms of the clause are in reverse order as listed explicitly in the file.
 E.g., if the clause in the file is listed as '1 2 -3 0', then consume is called with 
 (-3 2 1) as argument.  The default consume function reverses these back into
 the order specified in the DIMACS file, and accumulates the list of clauses
 in the same order as in the file. 

 The file may begin with comment lines. The first character of each
 comment line must be a lower case letter \"c\". Comment lines typically
 occur in one section at the beginning of the file, but are allowed to
 appear throughout the file.

 The comment lines are followed by the \"problem\" line. This begins
 with a lower case \"p\" followed by a space, followed by the problem
 type, which for CNF files is \"cnf\", followed by the number of
 variables followed by the number of clauses.

 The remainder of the file contains lines defining the clauses, one by
 one.

 A clause is defined by listing the index of each positive literal,
 and the negative index of each negative literal. Indices are 1-based,
 and for obvious reasons the index 0 is not allowed.

 The definition of a clause may extend beyond a single line of text.

 The definition of a clause is terminated by a final value of \"0\".

 The file terminates after the last clause is defined.

 Some odd facts include:

 The definition of the next clause normally begins on a new line, but
 may follow, on the same line, the \"0\" that marks the end of the
 previous clause.

 In some examples of CNF files, the definition of the last clause is
 not terminated by a final '0';

 In some examples of CNF files, the rule that the variables are
 numbered from 1 to N is not followed. The file might declare that
 there are 10 variables, for instance, but allow them to be numbered 2
 through 11."
  (typecase file
    ((or pathname string)                             ; file name
     (with-open-file (stream file :direction :input :if-does-not-exist :error
                                  :external-format :utf-8)
       (read-dimacs-file stream :consume consume)))
    (stream
     (let ((EOF (list nil))
           final-value)
       (labels ((skip-to-eol ()
                  (read-line file nil EOF))
                (read-clause ()
                  (let (clause)
                    (loop :for num = (read file nil EOF)
                          :when (or (eql 0 num)
                                    (eql EOF num))
                            :do (loop-finish)
                          :do (push num clause)
                          :finally (setf final-value (funcall consume clause))))))
         (loop :for ch = (peek-char nil file nil EOF)
               :do (cond
                     ((eql ch EOF)
                      (loop-finish))
                     ((or (digit-char-p ch)
                          (eql ch #\-))
                      (read-clause))
                     (t
                      (skip-to-eol)))))

       final-value))))

;;  LocalWords:  McCluskey mccluskey downto destructuring vec DNF CNF
;;  LocalWords:  maxterms minterms cnf dnf MERCHANTABILITY sublicense
;;  LocalWords:  NONINFRINGEMENT etypecase disjunction bdd cond plusp
;;  LocalWords:  mapcar setf expt dolist pushnew aref eql gethash eq
;;  LocalWords:  removef plists pos qm nconc acc subsetp cdr nconc
;;  LocalWords:  dotimes incf
