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

(defgeneric bdd-serialize (bdd)
  (:documentation "The serialization of a BDD is used for various types of display purposes
such as debugging and PRINT-OBJECT."))
(defgeneric bdd-factory (bdd-class)
  (:documentation "Given a class or class-name return the factory function for instantiating a BDD object.
The factor-function is a unary function which can accept a Boolean expresion as an s-expression,
and return a BDD representing that expression."))
(defgeneric bdd (obj &key bdd-node-class))
(defgeneric bdd-leaf (value)
  (:documentation "Given T or NIL, return the corresponding leaf node *BDD-TRUE* or *BDD-FALSE*"))
(defgeneric bdd-node (label positive negative  &key bdd-node-class)
  (:documentation "Create a new instance of BDD-NODE class if necessary, via a call to BDD-ENSURE-NODE.
This generic function accepts t and nil as positive and negative children along with other BDD nodes."))
(defgeneric bdd-or (b1 b2))
(defgeneric bdd-and (b1 b2))
(defgeneric bdd-and-not (b1 b2))
(defgeneric bdd-xor (b1 b2))
(defgeneric bdd-not (b))
(defgeneric bdd-allocate (label positive-bdd negative-bdd &key bdd-node-class &allow-other-keys)
  (:documentation "Allocate a new bdd object, whose class is specified by BDD-NODE-CLASS, and
register the object in the global hash indicated by calling (BDD-HASH).  This function should be
called after it has been verified that the positive and negative children are not equal, and
that no such object already exists in the hash table."))
(defgeneric bdd-dnf-wrap (bdd operator zero forms)
  (:documentation   "Given a BDD object, an OPERATOR, ZERO, and a list FORMS,
return a Boolean expression as simply as possible representing the a application of
OPERATOR to FORMS.  The methods of this generic function are free to make further
reductions depending on the class of BDD given."
))

(deftype class-designator ()
  `(or (and symbol (not null)) class))

(defvar *bdd-count* 1
  "Increment variable to assure that every BDD has a unique IDENT")
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
  "Reader for the LABEL slot which represents the Boolean variable corresponding to this ROBDD node."
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
	     ))
  (:documentation "subclass of BDD representing internal nodes of a BDD, i.e., nodes having
two children, accessible by the reader functions BDD-POSITIVE and BDD-NEGATIVE."))


(defmethod bdd-factory ((bdd-class (eql (find-class 'bdd-node))))
  #'bdd)

(defmethod bdd-factory ((bdd-class (eql 'bdd-node)))
  #'bdd)

(defun bdd-bfs (bdd action)
  "Walk a given BDD (object of class) bdd, calling the given FUNCTION on each node exactly once.
The return value of FUNCTION is ignored."
  (declare (type bdd bdd)
	   (type (function (bdd) t) action))
  (let* ((buf (tconc nil bdd))
         ;; don't really need nodes, we could
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
(defvar *bdd-hash-strength* :weak-dynamic "Special variable whose type is (member :weak-dynamic :weak :strong).
The value of this variable indicates which hash strategy to use by controling the behavior of BDD-ENSURE-HASH.
Each call to BDD-ENSURE-HASH:
  :strong       -- will create a new strong hash table.
  :weak         -- will create a new weak hash table.
  :weak-dynamic -- will create a new hash only if the BDD-NODE-TYPE of the currently available hash table, returned by (bdd-hash), is the same EQUAL valueas BDD-NODE-TYPE passed to BDD-ENSURE-HASH, otherwise the current global hash table be used (without allocating a new one).   In any case if there is no global hash table currently available, a new hash will be allocated and returned.")


;; TODO :generation should be removed, along with removing the corresponding global variable
;;   and the bdd-generation function.
(defun bdd-ensure-hash (&key (bdd-node-type '(or bdd-node bdd-leaf))
                       ((bdd-hash-strength *bdd-hash-strength*) *bdd-hash-strength*))
  "This function implements the semantics of *BDD-HASH-STRENGH*.
BDD-ENSURE-HASH is called by BDD-CALL-WITH-NEW-HASH to produce the new hash struct plist
to be bound to the special variable *BDD-HASH-STRUCT*
Returns a plist with keys
    :BDD-NODE-TYPE -- class name of nodes, e.g., bdd-node or bdd-leaf, 
         may be different class names for subclass of bdd.
    :RECENT-COUNT -- used in reporting cache lossage
    :STENGTH -- one of :strong, :weak, or :weak-dynamic
    :HASH -- the actual hash table storing bdd nodes"
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
             (cond
               ((not (equal bdd-node-type (bdd-node-type)))
                ;; if we are reusing the current hash table, then we must
                ;; make sure that the node type is the same
                (make-hash))
               ((bdd-hash))
               (t
                (make-hash))))))))

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

;;(defvar *bdd-hash* (bdd-ensure-hash))
;;(defvar *bdd-recent-count* 0)

(defvar *bdd-verbose* nil)

(defmacro bdd-with-new-hash (vars &body body)
  "Any access to the machinery in this package must occure within the dynamic extent of this function."
  `(bdd-call-with-new-hash (lambda ,vars ,@body)))

(defun bdd-call-with-new-hash (thunk &key (bdd-node-type '(or bdd-node bdd-leaf)) (verbose *bdd-verbose*))
  "Functional version of the BDD-WITH-NEW-HASH macro, which takes a 0-ary function to evaluate
in a dynamic extent which rebinds *BDD-HASH-STRUCT* and *BDD-VERBOSE*.  *BDD-HASH-STRUCT* is
rebound by a call to BDD-ENSURE-HASH whose behavior depends on the value of BDD-NODE-TYPE"
  (let ((*bdd-verbose* verbose)
        (*bdd-hash-struct* (bdd-ensure-hash :bdd-node-type bdd-node-type)))
    (multiple-value-prog1 (funcall thunk)
      (when verbose
        (format t "finished with ~A~%" (bdd-hash))))))
  
(defun bdd-make-key (label positive negative)
  (list positive negative label))

(defun bdd-find-int-int (hash label positive negative)
  (declare (type fixnum positive negative)
           (optimize (speed 3)))
  (gethash (bdd-make-key label positive negative) hash))

(defun bdd-find (label positive-bdd negative-bdd)
  "Search the hash table returned from (BDD-HASH) to determine
whether there is already a BDD whose label is LABEL having
the two given children."
  (declare (type bdd positive-bdd negative-bdd))
  (let ((hash (bdd-hash)))
    (setf (bdd-recent-count) (hash-table-count hash))
    (bdd-find-int-int hash label (bdd-ident positive-bdd) (bdd-ident negative-bdd))))

(defun (setf bdd-find) (bdd label positive-bdd negative-bdd)
  (let ((key (bdd-make-key label (bdd-ident positive-bdd) (bdd-ident negative-bdd))))
    (setf (gethash key (bdd-hash)) bdd)))

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
  "Read accessor for the NEGATIVE slot of a bdd-node.  Returns the negative child."
  (slot-value bdd 'negative))

;; reader for the label slot.  I've implemented this as a defun rather than :reader becase
;;  it seems to be in the critical performance loop and the normal function performs marginally
;;  faster than the method
(defun bdd-positive (bdd)
  "Read accessor for the POSITIVE slot of a bdd-node.  Returns the positive child."
  (slot-value bdd 'positive))

(defclass bdd-leaf (bdd) ()
  (:documentation "Class reprenting leaf nodes of a BDD.  The two possible leaf
node objects are *BDD-FALSE* and *BDD-TRUE*, which are singleton instances of
BDD-FALSE and BDD-TRUE."))

(defmethod bdd-serialize ((leaf bdd-leaf))
  "Serialize a BDD-LEAF node simply as t or nil."
  (bdd-label leaf))

(defclass bdd-true (bdd-leaf)
  ((ident :initform 1)
   (label :initform t)
   (dnf :initform t)
   (expr :initform t))
  (:documentation
   "Class of singleton object representing the TRUE leaf node of an ROBDD.  The singleton object is `*bdd-true*`."))

(defclass bdd-false (bdd-leaf)
  ((ident :initform 0)
   (label :initform nil)
   (dnf :initform nil)
   (expr :initform nil))
  (:documentation
   "Class of singleton object representing the FALSE leaf node of an ROBDD.  The singleton object is `*bdd-false*`."))

(defvar *bdd-true* (make-instance 'bdd-true) "Singleton TRUE object which is a leaf node of every non-trivial ROBDD.")
(defvar *bdd-false* (make-instance 'bdd-false) "Singleton FALSE object which is a leaf node of every non-trivial ROBDD.")

(defmethod bdd ((label symbol) &key (bdd-node-class 'bdd-node))
  (declare (type class-designator bdd-node-class))
  (bdd-ensure-node label *bdd-true* *bdd-false* :bdd-node-class bdd-node-class))

(defmethod bdd ((label fixnum) &key (bdd-node-class 'bdd-node))
  (declare (type class-designator bdd-node-class))
  (bdd-ensure-node label *bdd-true* *bdd-false* :bdd-node-class bdd-node-class))

(defgeneric bdd-list-to-bdd (head tail &key bdd-node-class))

(defvar *bdd-reduce-function* #'tree-reduce "function to preform reduction, either #'cl:reduce or cl-robdd:tree-reduce.
This function will be used within bdd-list-to-bdd when to perfrom and, or, and xor on multiple arguments.")

(defmethod bdd-list-to-bdd ((head (eql 'xor)) tail &key (bdd-node-class 'bdd-node) &allow-other-keys)
  (declare (type class-designator bdd-node-class))
  (funcall *bdd-reduce-function* #'bdd-xor tail
	   :initial-value *bdd-false*
	   :key (bdd-factory bdd-node-class)))

(defmethod bdd-list-to-bdd ((head (eql 'and)) tail &key (bdd-node-class 'bdd-node) &allow-other-keys)
  (declare (type class-designator bdd-node-class))
  (funcall *bdd-reduce-function* #'bdd-and tail
	   :initial-value *bdd-true*
	   :key (bdd-factory bdd-node-class)))

(defmethod bdd-list-to-bdd ((head (eql 'or)) tail &key (bdd-node-class 'bdd-node) &allow-other-keys)
  (declare (type class-designator bdd-node-class))
  (funcall *bdd-reduce-function* #'bdd-or tail
	   :initial-value *bdd-false*
	   :key (bdd-factory bdd-node-class)))

(defmethod bdd-list-to-bdd ((head (eql 'and-not)) tail &key (bdd-node-class 'bdd-node) &allow-other-keys)
  (declare (type class-designator bdd-node-class))
  ;; (assert (<= 2 (length tail)) ()
  ;;         "AND-NOT takes at least two arguments: cannot convert ~A to a BDD" expr)
  (destructuring-bind (bdd-head &rest bdd-tail) tail
    (bdd-and-not (funcall (bdd-factory bdd-node-class) bdd-head)
		 (bdd-list-to-bdd 'and bdd-tail :bdd-node-class bdd-node-class))))

(defmethod bdd-list-to-bdd ((head (eql 'not)) tail &key (bdd-node-class 'bdd-node) &allow-other-keys)
  (declare (type class-designator bdd-node-class))
  ;; (assert (null (cdr tail)) ()
  ;;         "NOT takes exactly one argument: cannot convert ~A to a BDD" expr)
  (bdd-and-not *bdd-true* (funcall (bdd-factory bdd-node-class) (car tail))))

(defmethod bdd-list-to-bdd (head tail &key bdd-node-class &aux (expr (cons head tail)) )
  (bdd-ensure-node expr *bdd-true* *bdd-false* :bdd-node-class bdd-node-class))

(defmethod bdd ((expr list) &key (bdd-node-class 'bdd-node))
  (declare (type class-designator bdd-node-class))
  (bdd-list-to-bdd (car expr) (cdr expr) :bdd-node-class bdd-node-class))

(defmethod bdd ((label (eql nil)) &key &allow-other-keys)
  *bdd-false*)

(defmethod bdd ((label (eql t)) &key &allow-other-keys)
  *bdd-true*)

(defmethod bdd-leaf ((value (eql t)))
  "Return the TRUE leaf node *BDD-TRUE*"
  *bdd-true*)
(defmethod bdd-leaf ((value (eql nil)))
  "Return the FALSE leaf node *BDD-FALSE*"
  *bdd-false*)

(defmethod bdd-serialize ((b bdd-node))
  "Serialize a BDD-NODE (i.e., internal node) as a list of the label followed by the serialization
of the positive then the negative child nodes."
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

(defmethod bdd-node (label (positive bdd) (negative bdd) &key (bdd-node-class 'bdd-node))
  (declare (type class-designator bdd-node-class))
  (bdd-ensure-node label positive negative :bdd-node-class bdd-node-class))

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
  (bdd-ensure-node (bdd-label b)
            (bdd-and-not *bdd-true* (bdd-positive b))
            (bdd-and-not *bdd-true* (bdd-negative b))
            :bdd-node-class (class-of b)))

(defvar *bdd-cmp-function* #'compare-objects "Special (dynamic) variable containing the function object to be used
when comparing two ROBDD labels.  This should be a function which returns a symbol CL:<, CL:>, or CL:= deterministically
given two objects.")

(defun bdd-cmp (t1 t2)
  (the (member < > =)
       (funcall *bdd-cmp-function* t1 t2)))

(defun bdd-op (op bdd-1 bdd-2)
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
       (bdd-ensure-node lab-1 (funcall op positive-1 positive-2) (funcall op negative-1 negative-2)
		  :bdd-node-class (class-of bdd-1)))
      ;; If the labels are not equal, then take the lesser label
      ;; and perform the op on the lesser.positive vs greater  and lesser.negative vs greater,
      ;; being careful not to change the order of the arguments as there is
      ;; no guarantee that op is commutative, and in fact and-not is not commutative.
      ;; This means (bdd-ensure-node lesser.label (op lesser.positive greater) (op lesser.negative greater))
      ;;   or       (bdd-ensure-node lesser.label (op greater lesser.positive) (op greater lesser.negative))
      ((<)
       (bdd-ensure-node lab-1 (funcall op positive-1 bdd-2) (funcall op negative-1 bdd-2)
		  :bdd-node-class (class-of bdd-1)))
      ((>)
       (bdd-ensure-node lab-2 (funcall op bdd-1 positive-2) (funcall op bdd-1 negative-2)
		  :bdd-node-class (class-of bdd-2))))))

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
      (bdd-op #'bdd-and-not b1 b2)))

(defmethod bdd-or (b1 b2)
  (error "bdd-or not implemented for ~A and ~A" b1 b2))
(defmethod bdd-xor (b1 b2)
  (error "bdd-xor not implemented for ~A and ~A" b1 b2))
(defmethod bdd-and (b1 b2)
  (error "bdd-and not implemented for ~A and ~A" b1 b2))
(defmethod bdd-and-not (b1 b2)
  (error "bdd-and-not not implemented for ~A and ~A" b1 b2))

(defun bdd-to-dnf (bdd)
  "Return the DNF (disjunctive normal form) of the Boolean expression representing the
given BDD.  This DNF generation is lazy and memoized.  The first time BDD-TO-DNF is called
the expression is generated and attached to the BDD object (via the DNF slot), 
thereafter, the same s-expression is returned."
  (slot-value bdd 'dnf))

(defun bdd-to-expr (bdd)
  "Return a Boolean expression representing the given BDD.  This expression is not
necessarly the DNF form, rather it is the easiest expression to generate.  The expression
is formed by accessing the EXPR slot of the given BDD.  Since the slot is lazily 
initialized, the first call to BDD-TO-EXPR may be more time consuming than subsequent
calls with the same BDD.  The expression is formed as: (or (and A P) (and (not A) N))
where P and N are the expressions of the positive and negative children respectively,
with additional simplification in the case either child node is *bdd-false* or *bdd-true*."
  (declare (type bdd bdd))
  (slot-value bdd 'expr))

(defmethod bdd-dnf-wrap ((bdd bdd) operator zero terms)
  "Given a list of TERMS, return an s-expression representing a Boolean expression
which combines the TERMS with the given OPERATOR.  E.g., (and T1 T2 T3).
A couple of corner cases are considered.  If TERMS is the empty list, then
the given ZERO is returned.   ZERO should be NIL when OPERATOR is OR and it should
be T when OPERATOR is AND.  If TERMS is a singleton list, its first element is returned.
I.e., rather than returning (or X), simply X is returned; and rather than returning (and),
T is returned."
  (cond ((cdr terms)
	 (cons operator terms))
	(terms
	 (car terms))
	(t
	 zero)))

(defun %bdd-to-dnf (bdd)
  "Convert a BDD to logical expression in DNF (disjunctive normal form), i.e. an OR of ANDs.
The construction attempts re-use cons cells in order to reduce the memory footprint of a large
set of BDDs."
  (declare (type bdd bdd))
  (labels (
	   (prepend (head dnf)
	     (typecase dnf
	       ((cons (eql or))
		(bdd-dnf-wrap
		 bdd 'or nil
		 (mapcar (lambda (tail)
			   (prepend head tail))
			 (cdr dnf))))
	       ((cons (eql and))
		(bdd-dnf-wrap bdd 'and t (cons head (cdr dnf))))
	       ((eql t)
		head)
	       ((eql nil)
		nil)
	       (t
		(bdd-dnf-wrap bdd 'and t (list head dnf)))))
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
                (bdd-dnf-wrap bdd 'or nil (cons negative (cdr positive))))
               ((typep negative '(cons (eql or)))
                (bdd-dnf-wrap bdd 'or nil (cons positive (cdr negative))))
               (t
                (bdd-dnf-wrap bdd 'or nil (list positive negative))))))
    
    (let ((positive-terms (prepend (bdd-label bdd) (bdd-to-dnf (bdd-positive bdd))))
          (negative-terms (prepend `(not ,(bdd-label bdd)) (bdd-to-dnf (bdd-negative bdd)))))
      (disjunction positive-terms
                   negative-terms))))

(defmethod slot-unbound (class (bdd bdd-node) (slot-name (eql 'dnf)))
  (setf (slot-value bdd 'dnf)
        (%bdd-to-dnf bdd)))

(defmethod slot-unbound (class (bdd bdd-node) (slot-name (eql 'expr)))
  "Lazy initialization for the EXPR slot of a BDD-NODE.
The value calculated is a Boolean expression which is easier to calculate
than the DNF form.   The DNF form would require traversal of the entire BDD
blow this point.  By contrast, calculating the EXPR slot for a node who
does NOT have terminals as children nodes is simply:
EXPR = (or (and label P) (and (not label) N))
where P is the EXPR slot of the positive child (bdd-to-expr (bdd-positive bdd))
  and N is the EXPR slot of the negative child (bdd-to-expr (bdd-negative bdd)).
There is additional simplification if one of the child nodes is *bdd-false*
or *bdd-true*."
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
    (assert (typep bdd (bdd-node-type)) (bdd) "The bdd hash ~A is expecting objects of type ~A not ~A"
            (bdd-hash) (bdd-node-type)  (type-of bdd))
    (setf (gethash key (bdd-hash)) bdd)))

(defun bdd-ensure-node (label positive-bdd negative-bdd &key (bdd-node-class 'bdd-node))
  (declare (type class-designator bdd-node-class)
	   (type bdd positive-bdd negative-bdd))
  "Allocate a new BDD (object of class designator by bdd-node-class) or return an
existing one if possible.  An existing exists in two cases.  1) if the positive-bdd
and negative-bdd are EQ, then just return positive-bdd, thus enforcing the deletion
rule, or 2) if such a BDD already exists in in the hash, as obtained by calling
BDD-FIND, thus enforcing the merging rule and maintaining structural identity; i.e.,
two BDDs representing the same Boolean expression, are EQ to each other."
  (cond
    ((eq positive-bdd negative-bdd)
     positive-bdd)
    ((bdd-find label positive-bdd negative-bdd))
    (t
     (bdd-allocate label positive-bdd negative-bdd :bdd-node-class bdd-node-class))))

(defun bdd-walk (bdd visitor-function &key (bdd-node-class))
  "This function starts at a BDD, and walks the dag applying
the VISITOR-FUNCTION at each internal node.  As long as the VISITOR-FUNCTION
returns NIL, the descent continues, at each step constructing a new BDD
(via BDD-ENSURE-NODE build of recursive walks of the positive and negative
children).
VISITOR-FUNCTION must be a function which returns NIL indicating to continue walking
   or a BDD indicating to terminate the descent and return this BDD."
  (declare (type bdd bdd)
	   (type (function (bdd) (or bdd null)) visitor-function))
  (labels ((recure (bdd)
             (cond
               ((typep bdd 'bdd-leaf)
                bdd)
               ((funcall visitor-function bdd))
               (t
                (bdd-ensure-node (bdd-label bdd)
				 (recure (bdd-positive bdd))
				 (recure (bdd-negative bdd))
				 :bdd-node-class bdd-node-class)))))
    (recure bdd)))

