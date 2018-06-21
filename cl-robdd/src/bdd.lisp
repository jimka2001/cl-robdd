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

(defgeneric bdd-serialize (bdd))
(defgeneric bdd-factory (bdd-class))
(defgeneric bdd (obj &key bdd-node-class))
(defgeneric bdd-leaf (value))
(defgeneric bdd-node (label positive negative  &key bdd-node-class))
(defgeneric bdd-or (b1 b2))
(defgeneric bdd-and (b1 b2))
(defgeneric bdd-and-not (b1 b2))
(defgeneric bdd-xor (b1 b2))
(defgeneric bdd-not (b))
(defgeneric %bdd-node (label positive-bdd negative-bdd &key bdd-node-class &allow-other-keys))
(defgeneric bdd-allocate (label positive-bdd negative-bdd &key bdd-node-class &allow-other-keys))


(deftype class-designator ()
  `(or (and symbol (not null)) class))

(defvar *bdd-count* 1)
(defclass bdd ()
  ((ident ;; :reader bdd-ident
    :type unsigned-byte
    :initarg :ident :initform (incf *bdd-count*))
   (label ;; :reader bdd-label
    :initarg :label)
   (dnf)
   (expr)))

;; reader for the label slot.  I've implemented this as a defun rather than :reader becase
;;  it seems to be in the critical performance loop and the normal function performs marginally
;;  faster than the method
(defun bdd-label (bdd)
  (slot-value bdd 'label))

;; reader for the label slot.  I've implemented this as a defun rather than :reader becase
;;  it seems to be in the critical performance loop and the normal function performs marginally
;;  faster than the method
(defun bdd-ident (bdd)
  (slot-value bdd 'ident))



(defclass bdd-node (bdd)
  ((positive :type bdd :initarg :positive ;;:reader bdd-positive
         )
   (negative :type bdd :initarg :negative ;; :reader bdd-negative
          )))

(defmethod bdd-factory ((bdd-class (eql (find-class 'bdd-node))))
  #'bdd)

(defmethod bdd-factory ((bdd-class (eql 'bdd-node)))
  #'bdd)

(defun bdd-bfs (bdd action)
  (let* ((buf (tconc nil bdd))
         ;; TODO -- don't really need nodes, we could
         ;; actually just pop (car buf), but this way
         ;; is a bit less obscure.
         (nodes (car buf)))
    (while nodes
      (funcall action (car nodes))
      (typecase (car nodes)
        (bdd-node
         (unless (member (bdd-positive (car nodes)) (car buf) :test #'eq)
           (tconc buf (bdd-positive (car nodes))))
         (unless (member (bdd-negative (car nodes)) (car buf) :test #'eq)
           (tconc buf (bdd-negative (car nodes))))))
      (pop nodes))))


(defvar *bdd-generation* 0)
(defvar *bdd-hash-strength* :weak-dynamic ) ;; or :weak or :weak-dynamic

(defun bdd-new-hash (&key (bdd-node-type '(or bdd-node bdd-leaf))
                       ((bdd-hash-strength *bdd-hash-strength*) *bdd-hash-strength*))
  (flet ((make-hash ()
           (incf *bdd-generation*)
           (case *bdd-hash-strength*
             ((:weak :weak-dynamic)
              (make-hash-table :test #'equal
                               #+sbcl :weakness #+sbcl :value
                               #+allegro :values #+allegro :weak))
             (t
              (make-hash-table :test #'equal)))))
    (list :generation *bdd-generation*
          :bdd-node-type bdd-node-type
          :recent-count 0
          :strength *bdd-hash-strength*
          :hash 
          (case *bdd-hash-strength*
            ((:weak)
             (make-hash))
            ((:strong)
             (make-hash-table :test #'equal))
            (t
             (if (bdd-hash)
                 (progn
                   ;; if we are reusing the current hash table, then we must
                   ;; make sure that the node type is the same
                   (assert (equal bdd-node-type
                                  (bdd-node-type))
                           ()
                           "vain attempt make a new hash structure, reusing old hash table, but node types are not compatible:  ~A vs ~A"
                           bdd-node-type (bdd-node-type))
                   (bdd-hash))
                 (make-hash)))))))

(defvar *bdd-hash-struct* nil)
(defun bdd-hash ()
  (getf *bdd-hash-struct* :hash))
(defun bdd-recent-count ()
  (getf *bdd-hash-struct* :recent-count))

(defun (setf bdd-recent-count) (value)
  (setf (getf *bdd-hash-struct* :recent-count)
        value))

(defun bdd-node-type ()
  (getf *bdd-hash-struct* :bdd-node-type))

(defun bdd-generation ()
  (getf *bdd-hash-struct* :generation))

;;(defvar *bdd-hash* (bdd-new-hash))
;;(defvar *bdd-recent-count* 0)

(defvar *bdd-verbose* nil)

(defmacro bdd-with-new-hash (vars &body body)
  `(bdd-call-with-new-hash (lambda ,vars ,@body)))

(defun bdd-call-with-new-hash (thunk &key (bdd-node-type '(or bdd-node bdd-leaf)) (verbose *bdd-verbose*))
  (let ((*bdd-verbose* verbose)
        (*bdd-hash-struct* (bdd-new-hash :bdd-node-type bdd-node-type)))
    (prog1 (funcall thunk)
      (when verbose
        (format t "finished with ~A~%" (bdd-hash))))))
  
(defun bdd-make-key (label positive negative)
  (list positive negative label))

(defun bdd-find-int-int (hash label positive negative)
  (declare (type fixnum positive negative)
           (optimize (speed 3)))
  (gethash (bdd-make-key label positive negative) hash))

(defun bdd-find (hash label positive-bdd negative-bdd)
  (declare (type bdd positive-bdd negative-bdd))
  (when (eq hash (bdd-hash))
    (setf (bdd-recent-count) (hash-table-count (bdd-hash))))
  (bdd-find-int-int hash label (bdd-ident positive-bdd) (bdd-ident negative-bdd)))

#+sbcl
(progn
  (defun report-hash-lossage ()
    (when (bdd-hash)
      (let ((old (bdd-recent-count))
            (new (hash-table-count (bdd-hash))))
        (unless (= old new)
          ;; (format t "generation=~D Before GC hash count was ~A, after GC is ~A, lossage=~A~%"
          ;;         *bdd-generation* old new (- old new))
          (setf (bdd-recent-count) new)))))
  (pushnew 'report-hash-lossage sb-ext:*after-gc-hooks*))


(defmethod print-object ((bdd bdd) stream)
  (print-unreadable-object (bdd stream :type t :identity nil)
    (when (slot-boundp bdd 'ident)
      (format stream "[~D]" (slot-value bdd 'ident)))
    (format stream "~S=~S" (bdd-serialize bdd) (bdd-to-dnf bdd))))

(defmethod bdd ((bdd bdd) &key &allow-other-keys)
  bdd)



;; reader for the label slot.  I've implemented this as a defun rather than :reader becase
;;  it seems to be in the critical performance loop and the normal function performs marginally
;;  faster than the method
(defun bdd-negative (bdd)
  (slot-value bdd 'negative))

;; reader for the label slot.  I've implemented this as a defun rather than :reader becase
;;  it seems to be in the critical performance loop and the normal function performs marginally
;;  faster than the method
(defun bdd-positive (bdd)
  (slot-value bdd 'positive))

(defclass bdd-leaf (bdd) ())

(defmethod bdd-serialize ((leaf bdd-leaf))
  (bdd-label leaf))

(defclass bdd-true (bdd-leaf)
  ((ident :initform 1)
   (label :initform t)
   (dnf :initform t)
   (expr :initform t)))

(defclass bdd-false (bdd-leaf)
  ((ident :initform 0)
   (label :initform nil)
   (dnf :initform nil)
   (expr :initform nil)))

(defvar *bdd-true* (make-instance 'bdd-true))
(defvar *bdd-false* (make-instance 'bdd-false))

(defmethod bdd ((label symbol) &key (bdd-node-class 'bdd-node))
  (declare (type class-designator bdd-node-class))
  (%bdd-node label *bdd-true* *bdd-false* :bdd-node-class bdd-node-class))

(defgeneric bdd-list-to-bdd (head tail &key bdd-node-class))
(defvar *bdd-operation-order* (the (member :divide-and-conquer
                                           :reduce) :divide-and-conquer ))
(flet ((bdd-list-to-bdd-helper (len bdds zero op)
         (labels ((divide-and-conquer (len bdds)
                    ;; len is the length of the bdds list, this is redunant but prevents re-counting each time
                    (case len
                      ((0)
                       zero)
                      ((1)
                       (car bdds))
                      (t
                       (let* ((half-length (truncate len 2))
                              (trailing (nthcdr half-length bdds))
                              (leading (ldiff bdds trailing)))
                         (funcall op (divide-and-conquer half-length leading)
                                  (divide-and-conquer (- len half-length) trailing)))))))
           (ecase *bdd-operation-order*
             ((:reduce)
              (reduce op bdds :initial-value zero))
             ((:divide-and-conquer)
              (divide-and-conquer len bdds))))))

  (defmethod bdd-list-to-bdd ((head (eql 'xor)) tail &key (bdd-node-class 'bdd-node) &allow-other-keys)
    (declare (type class-designator bdd-node-class))
    (bdd-list-to-bdd-helper (length tail) (mapcar (bdd-factory bdd-node-class) tail) *bdd-true* #'bdd-xor))
  
  (defmethod bdd-list-to-bdd ((head (eql 'and)) tail &key (bdd-node-class 'bdd-node) &allow-other-keys)
    (declare (type class-designator bdd-node-class))
    (bdd-list-to-bdd-helper (length tail) (mapcar (bdd-factory bdd-node-class) tail) *bdd-true* #'bdd-and))

  (defmethod bdd-list-to-bdd ((head (eql 'or)) tail &key (bdd-node-class 'bdd-node) &allow-other-keys)
    (declare (type class-designator bdd-node-class))
    (bdd-list-to-bdd-helper (length tail) (mapcar (bdd-factory bdd-node-class) tail) *bdd-false* #'bdd-or))

  (defmethod bdd-list-to-bdd ((head (eql 'and-not)) tail &key (bdd-node-class 'bdd-node) &allow-other-keys)
    (declare (type class-designator bdd-node-class))
    ;; (assert (<= 2 (length tail)) ()
    ;;         "AND-NOT takes at least two arguments: cannot convert ~A to a BDD" expr)
    (destructuring-bind (bdd-head &rest bdd-tail) (mapcar (bdd-factory bdd-node-class) tail)
      (ecase *bdd-operation-order*
        ((:reduce)
         (reduce #'bdd-and-not bdd-tail :initial-value bdd-head))
        ((:divide-and-conquer)
         (bdd-and-not bdd-head (bdd-list-to-bdd-helper (length bdd-tail) bdd-tail *bdd-true* #'bdd-and)))))))

(defmethod bdd-list-to-bdd ((head (eql 'not)) tail &key (bdd-node-class 'bdd-node) &allow-other-keys)
  (declare (type class-designator bdd-node-class))
  ;; (assert (null (cdr tail)) ()
  ;;         "NOT takes exactly one argument: cannot convert ~A to a BDD" expr)
  (bdd-and-not *bdd-true* (funcall (bdd-factory bdd-node-class) (car tail))))

(defmethod bdd-list-to-bdd (head tail &key bdd-node-class &aux (expr (cons head tail)) )
  (%bdd-node expr *bdd-true* *bdd-false* :bdd-node-class bdd-node-class))

(defmethod bdd ((expr list) &key (bdd-node-class 'bdd-node))
  (declare (type class-designator bdd-node-class))
  (bdd-list-to-bdd (car expr) (cdr expr) :bdd-node-class bdd-node-class))

(defmethod bdd ((label (eql nil)) &key &allow-other-keys)
  *bdd-false*)

(defmethod bdd ((label (eql t)) &key &allow-other-keys)
  *bdd-true*)

(defmethod bdd-leaf ((value (eql t)))
  *bdd-true*)
(defmethod bdd-leaf ((value (eql nil)))
  *bdd-false*)

(defmethod bdd-serialize ((b bdd-node))
  (list (bdd-label b)
        (bdd-serialize (bdd-positive b))
        (bdd-serialize (bdd-negative b))))

(defmethod bdd-node (label positive negative &key &allow-other-keys)
  (error "cannot create bdd-node from arguments ~A" (list label positive negative)))

(defmethod bdd-node :around (label (positive (eql t)) negative &key (bdd-node-class 'bdd-node))
  (declare (type class-designator bdd-node-class))
  (bdd-node label *bdd-true* negative :bdd-node-class bdd-node-class))

(defmethod bdd-node :around (label (positive (eql nil)) negative &key (bdd-node-class 'bdd-node))
  (declare (type class-designator bdd-node-class))
  (bdd-node label *bdd-false* negative :bdd-node-class bdd-node-class))

(defmethod bdd-node :around (label positive (negative (eql t)) &key (bdd-node-class 'bdd-node))
  (declare (type class-designator bdd-node-class))
  (bdd-node label positive *bdd-true* :bdd-node-class bdd-node-class))

(defmethod bdd-node :around (label positive (negative (eql nil)) &key (bdd-node-class 'bdd-node))
  (declare (type class-designator bdd-node-class))
  (bdd-node label positive *bdd-false* :bdd-node-class bdd-node-class))

(defvar *bdd-hash-access-count* 0)

(defmethod bdd-node (label (positive bdd) (negative bdd) &key (bdd-node-class 'bdd-node))
  (declare (type class-designator bdd-node-class))
  (%bdd-node label positive negative :bdd-node-class bdd-node-class))

(defmethod bdd-not ((true bdd-true))
  *bdd-false*)

(defmethod bdd-not ((false bdd-false))
  *bdd-true*)

(defmethod bdd-not ((b bdd))
  (bdd-and-not *bdd-true* b))

(defmethod bdd-or ((true bdd-true) (b bdd))
  *bdd-true*)
(defmethod bdd-or :around ((b bdd) (true bdd-true))
  *bdd-true*)
(defmethod bdd-or ((false bdd-false) (b bdd))
  b)
(defmethod bdd-or :around ((b bdd) (false bdd-false))
  b)

(defmethod bdd-xor ((true bdd-true) (b bdd))
  (bdd-not b))
(defmethod bdd-xor ((false bdd-false) (b bdd))
  b)
(defmethod bdd-xor :around ((b bdd) (true bdd-true))
  (bdd-not b))
(defmethod bdd-xor :around ((b bdd) (false bdd-false))
  b)



(defmethod bdd-and ((true bdd-true) (b bdd))
  b)
(defmethod bdd-and :around ((b bdd) (true bdd-true))
  b)
(defmethod bdd-and ((false bdd-false) (b bdd))
  *bdd-false*)
(defmethod bdd-and :around ((b bdd) (false bdd-false))
  *bdd-false*)


(defmethod bdd-and-not :around ((b bdd) (true bdd-true))
  *bdd-false*)
(defmethod bdd-and-not ((false bdd-false) (b bdd))
  *bdd-false*)
(defmethod bdd-and-not :around ((b bdd) (false bdd-false))
  b)
(defmethod bdd-and-not ((true bdd-true) (b bdd))
  (%bdd-node (bdd-label b)
            (bdd-and-not *bdd-true* (bdd-positive b))
            (bdd-and-not *bdd-true* (bdd-negative b))
            :bdd-node-class (class-of b)))


(defun %bdd-cmp (t1 t2)
  (cond
    ((equal t1 t2)
     '=)
    ((null t1)
     '<)
    ((null t2)
     '>)
    ((and (listp t1)
          (not (listp t2)))
     '>)
    ((and (listp t2)
          (not (listp t1)))
     '<)
    ((not (eql (class-of t1) (class-of t2))) 
     (bdd-cmp (class-name (class-of t1)) (class-name (class-of t2))))
    (t
     ;; thus they are the same type, but they are not equal
     (typecase t1
       (list
        (let (value)
          (while (and t1
                      t2
                      (eq '= (setf value (bdd-cmp (car t1) (car t2)))))
            (pop t1)
            (pop t2))
          (cond
            ((and t1 t2)
             value)
            (t1    '>)
            (t2    '<)
            (t     '=))))
       (symbol
        (cond
          ((not (eql (symbol-package t1) (symbol-package t2)))
           ;; call bdd-cmp because symbol-package might return nil
           ;;  don't call string= directly
           (bdd-cmp (symbol-package t1) (symbol-package t2)))
          ((string< t1 t2) ;; same package
           '<)
          (t
           '>)))
       (package
        (bdd-cmp (package-name t1) (package-name t2)))
       (string
        ;; know they're not equal, thus not string=
        (cond
          ((string< t1 t2)
           '<)
          (t
           '>)))
       (number
        (cond ((< t1 t2)
               '<)
              (t
               '>)))
       (t
        (error "cannot compare a ~A with a ~A" (class-of t1) (class-of t2)))))))

(defvar *bdd-cmp-function* #'%bdd-cmp)

(defun bdd-cmp (t1 t2)
  (the (member < > =)
       (funcall *bdd-cmp-function* t1 t2)))

(flet ((bdd-op (op bdd-1 bdd-2)
         (declare (type bdd bdd-1 bdd-2))
         (let ((lab-1   (bdd-label bdd-1))
               (positive-1  (bdd-positive bdd-1))
               (negative-1 (bdd-negative bdd-1))
               (lab-2   (bdd-label bdd-2))
               (positive-2  (bdd-positive bdd-2))
               (negative-2 (bdd-negative bdd-2)))
           (declare (type bdd positive-1 positive-2 negative-1 negative-2))
           (ecase (bdd-cmp lab-1 lab-2)
             ((=)
              ;; If the labels are equal, then the operations twice,
              ;; once on the two positive branches, and once on the two negative branches.
              (%bdd-node lab-1 (funcall op positive-1 positive-2) (funcall op negative-1 negative-2)
                         :bdd-node-class (class-of bdd-1)))
             ;; If the labels are not equal, then take the lesser label
             ;; and perform the op on the lesser.positive vs greater  and lesser.negative vs greater,
             ;; being careful not to change the order of the arguments as there is
             ;; no guarantee that op is commutative, and in fact and-not is not commutative.
             ;; This means (%bdd-node lesser.label (op lesser.positive greater) (op lesser.negative greater))
             ;;   or       (%bdd-node lesser.label (op greater lesser.positive) (op greater lesser.negative))
             ((<)
              (%bdd-node lab-1 (funcall op positive-1 bdd-2) (funcall op negative-1 bdd-2)
                         :bdd-node-class (class-of bdd-1)))
             ((>)
              (%bdd-node lab-2 (funcall op bdd-1 positive-2) (funcall op bdd-1 negative-2)
                         :bdd-node-class (class-of bdd-2)))))))

  (defmethod bdd-or ((b1 bdd-node) (b2 bdd-node))
    (if (eq b1 b2)
        b1
        (bdd-op #'bdd-or b1 b2)))

  (defmethod bdd-xor ((b1 bdd-node) (b2 bdd-node))
    (if (eq b1 b2)
        *bdd-false*
        (bdd-op #'bdd-xor b1 b2)))

  (defmethod bdd-and ((b1 bdd-node) (b2 bdd-node))
    (if (eq b1 b2)
        b1
        (bdd-op #'bdd-and b1 b2)))

  (defmethod bdd-and-not ((b1 bdd-node) (b2 bdd-node))
    (if (eq b1 b2)
        *bdd-false*
        (bdd-op #'bdd-and-not b1 b2))))

(defmethod bdd-or (b1 b2)
  (error "bdd-or not implemented for ~A and ~A" b1 b2))
(defmethod bdd-xor (b1 b2)
  (error "bdd-xor not implemented for ~A and ~A" b1 b2))
(defmethod bdd-and (b1 b2)
  (error "bdd-and not implemented for ~A and ~A" b1 b2))
(defmethod bdd-and-not (b1 b2)
  (error "bdd-and-not not implemented for ~A and ~A" b1 b2))

(defun bdd-to-dnf (bdd)
  (slot-value bdd 'dnf))

(defun bdd-to-expr (bdd)
  (slot-value bdd 'expr))

(defun %bdd-to-dnf (bdd)
  "Convert a BDD to logical expression in DNF (disjunctive normal form), i.e. an OR of ANDs.
The construction attempts re-use cons cells in order to reduce the memory footprint of a large
set of BDDs."
  (declare (type bdd bdd))
  (labels (
           (wrap (op zero forms)
             (cond ((cdr forms)
                    (cons op forms))
                   (forms
                    (car forms))
                   (t
                    zero)))
           (prepend (head dnf)
             (typecase dnf
               ((cons (eql or))
                (wrap
                 'or nil
                 (mapcar (lambda (tail)
                           (prepend head tail))
                         (cdr dnf))))
               ((cons (eql and))
                (wrap 'and t (cons head (cdr dnf))))
               ((eql t)
                head)
               ((eql nil)
                nil)
               (t
                (wrap 'and t (list head dnf)))))
           (disjunction (positive negative)
             (cond
               ((null positive)
                negative)
               ((null negative)
                positive)
               ((and (typep positive '(cons (eql or)))
                     (typep negative '(cons (eql or))))
                (cons 'or (nconc (copy-list (cdr positive)) (cdr negative))))
               ((typep positive '(cons (eql or)))
                (wrap 'or nil (cons negative (cdr positive))))
               ((typep negative '(cons (eql or)))
                (wrap 'or nil (cons positive (cdr negative))))
               (t
                (wrap 'or nil (list positive negative))))))
    
    (let ((positive-terms  (prepend (bdd-label bdd) (bdd-to-dnf (bdd-positive bdd))))
          (negative-terms (prepend `(not ,(bdd-label bdd)) (bdd-to-dnf (bdd-negative bdd)))))
      (disjunction positive-terms
                   negative-terms))))

(defmethod slot-unbound (class (bdd bdd-node) (slot-name (eql 'dnf)))
  (setf (slot-value bdd 'dnf)
        (%bdd-to-dnf bdd)))

(defmethod slot-unbound (class (bdd bdd-node) (slot-name (eql 'expr)))
  (setf (slot-value bdd 'expr)
        (cond
          ((and (eq *bdd-false* (bdd-positive bdd))
                (eq *bdd-true* (bdd-negative bdd)))
           `(not ,(bdd-label bdd)))
          ((and (eq *bdd-false* (bdd-negative bdd))
                (eq *bdd-true* (bdd-positive bdd)))
           (bdd-label bdd))
          ((eq *bdd-false* (bdd-positive bdd))
           `(and (not ,(bdd-label bdd)) ,(bdd-to-expr (bdd-negative bdd))))
          ((eq *bdd-false* (bdd-negative bdd))
           `(and ,(bdd-label bdd) ,(bdd-to-expr (bdd-positive bdd))))
          ((eq *bdd-true* (bdd-positive bdd))
           `(or ,(bdd-label bdd)
                (and (not ,(bdd-label bdd)) ,(bdd-to-expr (bdd-negative bdd)))))
          ((eq *bdd-true* (bdd-negative bdd))
           `(or (and ,(bdd-label bdd) ,(bdd-to-expr (bdd-positive bdd)))
                (not ,(bdd-label bdd))))
          (t
           `(or (and ,(bdd-label bdd) ,(bdd-to-expr (bdd-positive bdd)))
                (and (not ,(bdd-label bdd)) ,(bdd-to-expr (bdd-negative bdd))))))))


(defun incr-hash ()
  (incf *bdd-hash-access-count*)
  (when *bdd-verbose*
    (when (= 0 (mod *bdd-hash-access-count* 10000))
      (format t "bdd hash = ~A wall-time=~A cpu-time=~A~%"
              (getf *bdd-hash-struct* :hash)
              (truncate (get-internal-run-time) internal-time-units-per-second)
              (truncate (get-universal-time) internal-time-units-per-second)))))

(defmethod bdd-allocate (label (positive-bdd bdd-node) (negative-bdd bdd-node) &key &allow-other-keys)
  (if (eq (typep positive-bdd 'bdd-leaf)
          (typep negative-bdd 'bdd-leaf))
      (call-next-method label positive-bdd negative-bdd :bdd-node-class (class-of positive-bdd))
      (error "cannot create a bdd-node with children classes ~A"
             (list (class-of negative-bdd) (class-of positive-bdd)))))

(defmethod bdd-allocate (label (positive-bdd bdd-node) (negative-bdd bdd-leaf) &key &allow-other-keys)
  (call-next-method label positive-bdd negative-bdd :bdd-node-class (class-of positive-bdd)))

(defmethod bdd-allocate (label (positive-bdd bdd-leaf) (negative-bdd bdd-node) &key &allow-other-keys)
  (call-next-method label positive-bdd negative-bdd :bdd-node-class (class-of negative-bdd)))

(defmethod bdd-allocate (label (positive-bdd bdd) (negative-bdd bdd) &key (bdd-node-class 'bdd-node))
  (declare (type class-designator bdd-node-class))
  (let* ((bdd (make-instance bdd-node-class
                             :label label
                             :positive positive-bdd
                             :negative negative-bdd))
         (key (bdd-make-key label (bdd-ident positive-bdd) (bdd-ident negative-bdd))))
    (incr-hash)
    (assert (typep bdd (bdd-node-type)) (bdd) "The bdd hash ~A is expecting objects of type ~A not ~A"
            (bdd-hash) (bdd-node-type)  (type-of bdd))
    (setf (gethash key (bdd-hash)) bdd)))

(defmethod %bdd-node (label (positive-bdd bdd) (negative-bdd bdd) &key (bdd-node-class 'bdd-node))
  (declare (type class-designator bdd-node-class))
  (cond
    ((eq positive-bdd negative-bdd)
     positive-bdd)
    ((bdd-find (bdd-hash) label positive-bdd negative-bdd))
    (t
     (bdd-allocate label positive-bdd negative-bdd :bdd-node-class bdd-node-class))))




