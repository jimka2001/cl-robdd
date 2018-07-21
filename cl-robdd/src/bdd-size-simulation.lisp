;; Copyright (c) 2016 EPITA Research and Development Laboratory
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


(in-package :cl-robdd-analysis)


(defmacro setof (var data &body body)
  `(remove-if-not (lambda (,var) ,@body) ,data))

(defmacro while (test &body body)
  `(loop :while ,test
	 :do (progn ,@body)))

(defmacro forall (var data &body body)
  `(every #'(lambda (,var) ,@body) ,data))

(defun gen-min-term (i vars)
  ;; interpret the given I as a bit-mask
  ;; and generate an (AND ...) expression
  ;; the arguments of AND are the symbols in order in VAR
  ;; either as is or wrapped in (NOT ...)
  ;; e.g. if VARS='(a b), then 2 with bitmask 10 -> (and A (not B))
  ;; bits from right to left correspond to variables from left to right
  (cons 'and (mapcar (lambda (var)
                       (prog1 (if (oddp i)
                                  var
                                  `(not ,var))
                         (setf i (ash i -1))))
                     vars)))

(defun random-bdd (vars &aux (num-vars (length vars)))
  ;; generate 2^n bits
  ;; when the bit is 1, generate the minterm
  ;; BDD-OR together all the minterms incrementally
  (let (stack)
    (labels ((compactify ()
               (when (cdr stack)
                 (destructuring-bind ((n1 bdd1) (n2 bdd2) &rest tail) stack
                   (when (= n1 n2)
                     (setf stack (cons (list (* 2 n1) (bdd-or bdd1 bdd2)) tail))
                     (compactify))))))
               
      (dotimes (row (expt 2 num-vars))
        (unless (zerop (random 2))
          (push (list 1 (bdd (gen-min-term row vars)))
                stack)
          (compactify)))
      (bdd-list-to-bdd 'or (mapcar #'cadr stack)))))

(defun int-to-boolean-expression (n vars)
  "Returns a Boolean expression which is a Boolean combination of the given variable names.
VARS is a list of symbols indicating Boolean variable names
N is an integer: 0 <= N < 2^2^(length vars)

Denote M = (length VARS)
The truth table for a Boolean function of M has 2^M rows
   e.g., a 3 variable truth table has 8 rows.
If we want to generate such a truth table, we must supply 8 bits, i.e., an integer between
   0 and 255 (inclusive), i.e., 0 <= N < 2^2^M.
This function, INT-TO-BOOLEAN-EXPRESSION, iterates from 0 to 2^M - 1, and for each
iteration generates a min-term, by calling local function GEN-MIN-TERM.
E.g., (INT-TO-BOOLEAN-EXPRESSION #b00010011 '(a b c))
--> (OR (AND (NOT A) (NOT B) (NOT C)) 
        (AND A       (NOT B) (NOT C))
        (AND (NOT A) (NOT B) C)
        (AND A       B       C))
Why?  Because the truth table of this function is:
 CBA|
 000|1
 001|1
 010|0
 011|0
 100|1
 101|0
 110|0
 111|1
"
  (let ((num-vars (length vars)))
    (let ((max-n (expt 2 (expt 2 num-vars))))
      (assert (< n max-n) (n vars)
              "N=~D must be less than ~D for ~D variables=~A"
              n max-n num-vars vars))
    (cons 'or (loop for i from 0 below (expt 2 num-vars)
                    for m = n then (ash m -1)
                    when (oddp m)
                      nconc (list (gen-min-term i vars))))))


(defvar *bdd-boolean-variables* '(zm zl zk zj zi zh zg zf ze zd zc zb za z9 z8 z7 z6 z5 z4 z3 z2 z1))

(defun random-boolean-combination (vars)
  "return a randomly selection boolean combination of the given BOOLEAN variables in sum-of-minterms form (or (and ...) (and ...) ...)
VARS may be given as a positive integer, or as a list of symbols.
If VARS is a number, it should be <= (length *bdd-boolean-variables*)"
  (typecase vars
    (unsigned-byte
     (assert (<= vars (length *bdd-boolean-variables*)))
     (random-boolean-combination (nthcdr (- (length *bdd-boolean-variables*) vars) *bdd-boolean-variables*)))
    (list
     (int-to-boolean-expression (random (expt 2 (expt 2 (length vars))))
				vars))))

(defun median-a-list (a-list)
  (let ((a-list (copy-list a-list)))
    (loop while (cdr a-list)
          do (let ((couple (cons (car a-list) (last a-list))))
               (destructuring-bind ((low-index low-count)
                                    (high-index high-count)) couple
                 (setf a-list (if (and (null (cddr a-list))
                                       (= low-count high-count))
                                  (list (list (/ (+ low-index high-index) 2) 1))
                                  (merge 'list (cond ((= low-count high-count)
                                                      nil)
                                                     ((< low-count high-count)
                                                      (list (list high-index (- high-count low-count))))
                                                     (t
                                                      (list (list low-index (- low-count high-count)))))
                                         (copy-list (set-difference a-list couple :test #'eq))
                                         #'<
                                         :key #'car))))))
    (values (caar a-list) a-list)))

(defun difference-function (xys-a xys-b)
  "Given two lists of (x y) pairs, representing two functions, potentially
each list has different (more, less, or same) x values, calculate the difference
function (XYS-A - XYZ-B), which contains the union of the x values."
  (flet ((x-coord (pt)
           (declare (type (cons number (cons number)) pt))
           (car pt))
         (y-coord (pt)
           (declare (type (cons number (cons number)) pt))
           (cadr pt))
         (extrapolate (xy0 x1 xy2)
           (destructuring-bind (x0 y0) xy0
             (destructuring-bind (x2 y2) xy2
               (+ y0 (* (/ (- y2 y0) (- x2 x0)) (- x1 x0)))))))
    
    (let ((xys-a (sort (copy-list xys-a) #'< :key #'x-coord))
          (xys-b (sort (copy-list xys-b) #'< :key #'x-coord))
          (a-b nil))
      (dolist (cmp (list #'< #'>))
        (cond
         ((= (x-coord (car xys-a))
             (x-coord (car xys-b))))
         ((funcall cmp (x-coord (car xys-a))
                   (x-coord (car xys-b)))
          ;; push onto b
          (push (car xys-a) xys-b))
         (t
            ;; else push onto a
            (push (car xys-b) xys-a)))
        (setf xys-a (reverse xys-a)
              xys-b (reverse xys-b)))
      
      (push (list (x-coord (car xys-a)) 0.0)
            a-b)
      (while (or (cddr xys-a) (cddr xys-b))
        (destructuring-bind (a0 a1 &optional (a2 (nth 2 xys-b)) &rest _) xys-a
          (declare (ignore _))
          (destructuring-bind (b0 b1 &optional (b2 a2) &rest _) xys-b
            (declare (ignore _))
            (cond
              ((= (x-coord a1) (x-coord b1))
               (push (list (x-coord a1) (- (y-coord a1) (y-coord b1))) a-b)
               (pop xys-a)
               (pop xys-b))
              ((< (x-coord a1) (x-coord b1))
               (push (list (x-coord a1) (extrapolate b0 (x-coord a1) b2))
                     a-b)
               (pop xys-a))
              (t                       ; (> (x-coord a1) (x-coord b1))
               (push (list (x-coord b1) (extrapolate a0 (x-coord b1) a2))
                     a-b)
               (pop xys-b))))))
      
      (destructuring-bind (_ a2) xys-a
        (declare (ignore _))
        (destructuring-bind (_ b2) xys-b
        (declare (ignore _))
          (assert (equal (x-coord a2) (x-coord b2)) (xys-a xys-b))
          (push (list (x-coord a2) (- (y-coord a2) (y-coord b2))) a-b)))
      (nreverse a-b))))

(defun make-announcement-timer (min max interval announce)
  "Given a MIN and MAX iteration (integers) and an integer INTERVAL designating a number
 of seconds, and a unary function ANNOUNCE.
return a unary function which can later be called with each iteration from MIN to MAX and
will call the ANNOUNCE function if the elapsed time since the most recent call is more 
than INTERVAL number of seconds"
  (declare (type (function (integer number number) t) announce)
           (type integer min max)
           (type real interval))
  (let* ((start-time (get-internal-real-time))
         (internal-interval (* interval internal-time-units-per-second))
         (previous-announcement start-time))
    (lambda (iteration &aux (now (get-internal-real-time)))
      (cond
        ((equal min iteration))
        ((< (+ previous-announcement internal-interval)
            now)
         (setf previous-announcement now)
         (let* ((fraction-done (/ (- iteration min) (- max min)))
                (elapsed-seconds (/ (- now start-time) internal-time-units-per-second))
                (total-seconds (/ elapsed-seconds fraction-done))
                (remaining-seconds (- total-seconds elapsed-seconds)))
           (funcall announce iteration (coerce remaining-seconds 'double-float) (coerce total-seconds 'double-float))))))))

(defun calc-plist (histogram num-vars randomp &key (exponent 1))
  (declare (type cons histogram)
           (type fixnum num-vars))
  ;; histogram is a list of pairs, each pair is (sample occurances)
  (flet ((sqr (x) (* x x)))
    (let* ((num-samples (reduce (lambda (sum this)
                                  (declare (type integer sum)
                                           (type (cons integer (cons integer)) this))
                                  (destructuring-bind (sample occurances) this
                                    (declare (ignore sample))
                                    (+ sum occurances)))
                                histogram
                                :initial-value 0))
           (normalized-histogram (mapcar (lambda (pair)
                                           (destructuring-bind (sample count) pair
                                             (list sample (/ count num-samples))))
                                         histogram))
           (mean-size (reduce (lambda (sum this)
                           (destructuring-bind (sample probability) this
                             (+ sum (* probability sample))))
                         normalized-histogram
                         :initial-value 0))
           (stdev (sqrt (reduce (lambda (sum this)
                                  (destructuring-bind (sample probability) this
                                    (+ sum (* probability (sqr (- sample mean-size))))))
                                normalized-histogram :initial-value 0.0)))
           (ffff (1- (expt 2 (expt 2 num-vars))))
           (density (/ num-samples (1+ ffff))))

      (let (sum median)
        (setf sum (reduce #'+ histogram :initial-value 0 :key #'cadr))
        (setf median (median-a-list histogram))
        (list :sum sum
              :num-samples num-samples
              :exponent exponent
              :randomp randomp
              :num-vars num-vars
              :density (sci-notation density)
              :average-size mean-size
              :sigma stdev
              :median median
              :possible-sizes (mapcar #'car histogram)
              :unique-sizes (length histogram)
              :normalized-histogram normalized-histogram
              :counts (mapcar (lambda (pair)
                                (declare (type (cons integer (cons integer)) pair))
                                (list (car pair) ;; a bdd size
                                      (float (/ (cadr pair) sum)) ;; normalized number of bdds of this size as a fraction of total sample
                                      (cadr pair) ;; number of bdds of this size in sample
                                      ;; extrapolation
                                      ;;(truncate (cadr pair) density) ;; estimated number of unique bdds of this size
                                      ))
                              histogram))))))

(defun log-bdd-count (bdd-sizes-file num-vars bdd-count truth-table)
  (declare (type unsigned-byte bdd-count truth-table))
  (flet ((print-it (stream)
           ;; we print a line consisting of 4 items
           ;; 1. the number of Boolean variables in the sample
           ;; 2. the count of the the size of the corresponding ROBDD
           ;; 3. the base 36 representation of the integer representing the truth table of the ROBDD
           ;; 4. a semi-colon
           ;; The purpose of the semi-colon is to be able to recognize incomplete lines.
           ;;    The data is typically calucated and printed on a compute cluster, and from
           ;;    time to time the jobs are killed.  It is unlikely, but possible that a job
           ;;    gets killed in the middle of this call to format.   The
           ;;    COMBINE-BDD-SIZE-RESULTS identifies such lines and refuses to copy
           ;;    them.
           (format stream "~A ~A ~36R ;~%" num-vars bdd-count truth-table)
           (finish-output stream)))
    (typecase bdd-sizes-file
    (string
     (with-open-file (log-file bdd-sizes-file
                               :direction :output
                               :if-does-not-exist :create
                               :if-exists :append)
       (print-it log-file)))
    (t
     (print-it bdd-sizes-file)))))

(defun random-selection (data fraction)
  "DATA: list of objects
FRACTION: number between 0 and 1 to indicate which portion of the given population to choose"
  (declare (type (float 0.0 1.0) fraction)
           (type list data))
  (let* ((n (length data))
         (arr (make-array n :initial-contents data)))
    (loop :for i :from 0 :below n
          :for j = (random n)
          :do (rotatef (aref arr i) (aref arr j)))
    (loop :for i :from 0 :below (truncate (* n fraction))
          :collect (aref arr i))))

(defun generate-sample-files (bdd-sizes-file min-exponent max-exponent)
  ;; e.g., bdd-sizes-file = (format nil "/Users/jnewton/analysis/bdd-sizes-unique-~D.2-columns" n)
  ;; e.g., min-exponent = 2
  ;; e.g., max-exponent = 8
  (labels ((read-3 (stream)
             (values (read stream nil nil nil)
                     (read stream nil nil nil)
                     (read stream nil nil nil)))
           (read-samples (&aux triples (eof nil))
             (with-open-file (rstream bdd-sizes-file :direction :input :if-does-not-exist :error)
               (while (not eof)
                 (multiple-value-bind (num-vars bdd-count index) (read-3 rstream)
                   (cond
                     ((and num-vars bdd-count index)
                      (push (list num-vars bdd-count index) triples))
                     (t
                      (setf eof t))))))
             triples)
           (gen-sample (exponent samples
			&key (m (length samples))
			&aux
			  (half-m (truncate m 2))
			  (write-file (format nil "~A-exponent-~D" bdd-sizes-file exponent)))
             (when (<= exponent max-exponent)
               (with-open-file (wstream write-file :direction :output :if-exists :supersede :if-does-not-exist :create)
                 (format t "writing to ~A~%" wstream)
                 (dolist (triple samples)
                   (destructuring-bind (num-vars bdd-count index) triple
                     (format wstream "~A ~A ~A~%" num-vars bdd-count index))))
	       ;; the samples list is already shuffled, so just take half the list
	       ;;   using nthcdr
               (gen-sample (1+ exponent) (nthcdr half-m samples) :m half-m))))
    (gen-sample min-exponent (random-selection (read-samples) 0.5))))

(defun read-counts-from-log (target-num-vars bdd-sizes-file &key (exponent 1))
  (with-open-file (log-file (case exponent
                              ((1) bdd-sizes-file)
                              (t (format nil "~A-exponent-~D" bdd-sizes-file exponent)))
                            :direction :input
                            :if-does-not-exist :error)
    (format t "reading from ~A~%" log-file)
    (let (num-vars bdd-size samples)
      (while (setf num-vars (read log-file nil nil nil))
        ;; read the bdd-size integer
        (setf bdd-size (read log-file t nil nil))
        (when (= target-num-vars num-vars)
          (push bdd-size samples))
        ;; read and ignore the base-36 integer
        ;; read to end of line
        (let (char)
          (while (not (member char '(#\Linefeed #\Return)))
            (setf char (read-char log-file t nil nil)))))
      (format t "finished reading from ~A~%" log-file)
      samples)))

(defun bdd-count-nodes (bdd)
  (let ((c 0))
    (bdd-bfs bdd (lambda (node)
                   (declare (ignore node))
                   (incf c)))
    c))

(defun garbage-collect ()
  #+sbcl (sb-ext::gc :full t)
  #+allegro (excl:gc t)
)

(defun measure-bdd-size (vars num-samples &key (interval 2) (bdd-sizes-file "/dev/null") (read-from-log-p nil) (exponent 1))
  ;; READ-FROM-LOG-P specifies to read a bdd-size from the log file if possible.
  ;;      if there are fewer than num-samples in the log file, an error is triggered.
  ;;      If READ-FROM-LOG-P is TRUE, then BDD-SIZES-FILE should be a file already created
  ;;      and already uniquified.  We will assume it has no duplicate lines.
  ;;      The format of the file is line based.
  ;;      Each line has 3 numbers, the first 2 are in base 10, the 3rd is in base 36.
  ;;              num-vars bdd-size truth-table
  ;;      In the case that READ-FROM-LOG-P is TRUE, but the file does not have any lines beginning
  ;;      with (length VARS) as passed to MEASURE-BDD-SIZE, then it will be considered as
  ;;         BDD-SIZES-FILE=nil and READ-FROM-LOG-P=nil
  ;;     
  (setf num-samples (min (expt 2 (expt 2 (length vars)))
                         num-samples))
  (let* ((num-vars (length vars))
         (hash (make-hash-table))
         (ffff (1- (expt 2 (expt 2 num-vars))))
         (randomp (< num-samples (1+ ffff)))
         (start-time (get-internal-real-time))
         (bdd-sizes (when read-from-log-p
                      (read-counts-from-log num-vars bdd-sizes-file :exponent exponent))))

    (when (and read-from-log-p
               (null bdd-sizes))
      (setf read-from-log-p nil
            bdd-sizes-file nil))
    (when read-from-log-p
      (setf num-samples (length bdd-sizes)))
    (flet ((measure (truth-table)
             (cond
               ((null read-from-log-p)
                (bdd-with-new-hash ()               
                  (let* ((bdd (bdd (int-to-boolean-expression truth-table vars)))
                         (bdd-count (bdd-count-nodes bdd)))
                    (garbage-collect)
                    (log-bdd-count bdd-sizes-file num-vars bdd-count truth-table)
                    (incf (gethash bdd-count hash 0)))))
               (bdd-sizes
                (let ((bdd-size (pop bdd-sizes)))
                  (incf (gethash bdd-size hash 0))))
               (t
                (error "fewer than ~D samples in log file ~A" num-samples bdd-sizes-file))))
           (format-time (given-seconds prefix)
             (with-output-to-string (str)
               (let ((minutes (coerce (/ given-seconds 60) 'float))
                     (hours (coerce (/ given-seconds (* 60 60)) 'float)))
                 (when (> minutes 1)
                   (format str "~A ~D minutes" prefix (truncate minutes)))
                 (when (> hours 1)
                   (format str " ~D hours ~D minutes" (truncate hours)
                           (truncate (- minutes (* 60 (truncate hours))))))))))

      (let ((announcer (make-announcement-timer
                        2 (1- num-samples)
                        interval
                        (lambda (iteration remaining-seconds total-seconds)
                          (format t "~D iteration=~D: " num-vars iteration)
                          (format t "seconds remaining ~D~A" (truncate remaining-seconds) (format-time remaining-seconds " ="))
                          (format t "seconds estim total ~D~A" (truncate total-seconds) (format-time total-seconds " ="))
                          (format t "~%")))))
        (if read-from-log-p
            (format t "using pre-chosen ~D " num-samples)
            (format t "generating ~D " num-samples))
        (when randomp (format t "randomly chosen "))
        (format t "BDDs of possible ~D (~a%)~%   with ~D variables ~S~%"  (sci-notation-string (1+ ffff))
                (* 100.0 (/ num-samples (1+ ffff))) num-vars vars)
        (loop :for iteration :from 0 :below num-samples
              :do (measure (if read-from-log-p
                               0
                               (random (1- ffff))))
                  :do (funcall announcer iteration)))
      (let (histogram)
        (declare #+sbcl (notinline sort))
        (maphash (lambda (&rest args)
                   (push args histogram))
                 hash)
        (setf histogram (sort (copy-list histogram) #'< :key #'car))
        (list* :seconds (if read-from-log-p
                            -1
                            (float (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)))
               (calc-plist histogram num-vars randomp :exponent exponent))))))

(defun remove-duplicates-sorted-list (elements)
  (declare (optimize (speed 3) (debug 0)))
  (labels ((recure (elements tail)
             (cond
               ((cdr elements)
                (recure (cdr elements)
                        (if (eql (car elements) (cadr elements))
                            tail
                            (cons (car elements) tail))))
               (elements
                (cons (car elements) tail))
               (t
                tail))))
    (recure elements nil)))

(defun measure-bdd-sizes (vars num-samples min max &key (interval 2) (read-from-log-p nil) (bdd-sizes-file "/dev/null") (exponent 1))
  (mapcon (lambda (vars)
            (cond
              ((> min (length vars))
               nil)
              ((> (length vars) max)
               nil)
              (t
               (list (measure-bdd-size vars
                                       (min (expt 2 (expt 2 (length vars)))
                                            num-samples)
                                       :bdd-sizes-file bdd-sizes-file
                                       :read-from-log-p read-from-log-p
                                       :exponent exponent
                                       :interval interval)))))
          vars))

(defun convert-double-notation (stream string)
  (declare (type stream stream)
           (type string string))
  (loop for char across string
        do (if (char= #\d char)
               (format stream "e")
               (format stream "~A" char))))

(defun write-one-bdd-distribution-data (plist prefix &key (exponent 1))
  (let* ((num-vars (getf plist :num-vars))
         (data-file (case exponent
                      ((1) (format nil "~A/bdd-distribution-data-~D.sexp" prefix num-vars))
                      (t   (format nil "~A/bdd-distribution-data-~D-sample-~D.sexp" prefix num-vars exponent)))))
    (with-open-file (stream data-file
                            :direction :output :if-does-not-exist :create :if-exists :supersede)
      (when stream
        (format t "writing to ~A~%" data-file)
        (format stream "  (~%")
        (while plist
          (destructuring-bind (keyword obj &rest _others) plist
            (declare (ignore _others))
            (let ((*package* (find-package :keyword)))
              (format stream "    ~S ~A~%" keyword obj)))
          (pop plist)
          (pop plist))
	(format stream "  )~%")))))

(defun write-bdd-distribution-data (data prefix &key (exponent 1))
  (declare (type list data)
           (type string prefix))
  (dolist (plist data)
    (write-one-bdd-distribution-data plist prefix :exponent exponent)))

(defun read-bdd-distribution-data (prefix &key (min 1) (max (length *bdd-boolean-variables*)) (min-kolmogorov 8) (max-kolmogorov 18) (max-exponent 8) vars)

  (declare (ignore vars))
  (flet ((read-1-file (data-file exponent)
           (with-open-file (stream data-file
                                   :direction :input :if-does-not-exist nil)
             (when stream
               (format t "reading from ~A~%" stream)
               (let* ((plist (read stream))
                      (histogram (mapcar (lambda (this &aux (sample (car this)) (occurances (caddr this)))
                                           (list sample occurances))
                                         (getf plist :counts))))
                 (list (calc-plist histogram (getf plist :num-vars) (getf plist :randomp) :exponent exponent)))))))
    (append
     (loop for var from min to max
           for data-file = (format nil "~A/bdd-distribution-data-~D.sexp" prefix var)
           nconc (read-1-file data-file 1))
     (loop :for var :from min-kolmogorov :to max-kolmogorov
           :nconc
           (loop :for exponent :from 2 :to max-exponent
                 :for data-file = (format nil "~A/bdd-distribution-data-~D-sample-~D.sexp" prefix var exponent)
                 :nconc (read-1-file data-file exponent))))))
     

(defun measure-and-write-bdd-distribution (prefix num-vars num-samples bdd-sizes-file &key (exponent 1) (interval 2) (read-from-log-p nil))
  "PREFIX: string designating path name to directory to write analysis results, 
           e.g., \"/lrde/home/jnewton/analysis/.\"
   NUM-VARS: number of variables of the BDD to create
   NUM-SAMPLES: number of random truth tables to try
   BDD-SIZES-FILE: string (also accepts t an nil) indicate file to output log information
   INTERVAL: minimum number of seconds between progress updates"
  (write-bdd-distribution-data (measure-bdd-sizes *bdd-boolean-variables*
                                                  num-samples num-vars num-vars
                                                  :bdd-sizes-file bdd-sizes-file
                                                  :read-from-log-p read-from-log-p
                                                  :exponent exponent
                                                  :interval interval)
                               prefix
                               :exponent exponent))

(defun getter (field)
  (lambda (obj) (getf obj field)))


(defun print-option (axis-option)
  (typecase axis-option
    (string (format nil "~A" axis-option))
    ((cons string (cons string)) (format nil "~A=~A" (car axis-option) (cadr axis-option)))
    ((cons string (cons fixnum)) (format nil "~A=~D" (car axis-option) (cadr axis-option)))
    (t
     (error "unknown axis-option ~A" axis-option))))

(defun axis (stream axis-options continuation &key logx logy)
  (declare (type list axis-options)
           (type (function () t) continuation))
  (flet ((sanitize-axis-options (plot-options)
           (remove nil
                   `(,@(when (or logx logy)
                         '(("lua backend" "false")))
                     ,@(when logx
                         '(("xmode" "log")))
                     ,@(when logy
                         '(("ymode" "log")))
                     ,@plot-options))))
    (format stream "\\begin{axis}[~% ~A~%]~%"
            (join-strings (format nil ",~% ") (mapcar  #'print-option (sanitize-axis-options axis-options))))
    (prog1 (funcall continuation)
      (format stream "\\end{axis}~%"))))

(defun tikzpicture (stream comment continuation)
  ;; returns the values returned from CONTINUATION
  (declare (type string comment)
           (type (function () t) continuation))
  (cond ((string= "" comment)
         (error "inavalid comment empty-string for tikzpicture"))
        (t
         (format stream "% ~A~%" comment)))
  (format stream "\\begin{tikzpicture}~%")
  (prog1 (funcall continuation)
    (format stream "\\end{tikzpicture}~%")))

(defun addplot (stream plot-comment plot-options control-string points &key logx logy (addplot "addplot"))
  (declare (type (or null string) plot-comment)
           (type string control-string)
           (type list points plot-options)
           (type (function (list) number)))
                          
  ;; TODO check to see if all the point y values are equal, and if so
  ;;   create y min and max or marks to avoid latex warning
  ;; Package pgfplots Warning: Axis range for axis y is approximately empty;
  ;;   enlarging it (it is [2.0000000000:2.0000000000]) on input line 17.
  ;;
  (when plot-comment
    (format stream "% ~A~%" plot-comment))
  (format stream "\\~A[~A] coordinates {~%" addplot
          (join-strings "," (mapcar #'print-option plot-options)))
  (dolist (point points)
    (cond
      ((and logx
            (zerop (car point))))
      ((and logy
            (zerop (cadr point))))
      (t
       (apply #'format stream control-string point)
       (terpri stream))))
  (format stream "};~%"))

(defun latex-measure-bdd-sizes (prefix vars num-samples &key (min 1) (max (length vars)) (re-run t) (max-exponent 8) (min-kolmogorov 5) (max-kolmogorov 18))
  ;; example values
  ;; prefix = "/Users/jnewton/newton.16.edtchs/src"
  ;; vars   = *bdd-test-classes*
  ;; nums-samples = 1000
  (declare (type string prefix)
           (type list vars)
           (type fixnum num-samples)
           #+sbcl (notinline sort))
  (ensure-directories-exist prefix)
  (let* ((colors '("red" "goldenrod" "olive" "blue" "lavender" "greeny" "dark-cyan" "teal" "orange"))
         (data (if re-run
                   (sort (copy-list (measure-bdd-sizes vars num-samples min max)) #'< :key (getter :num-vars))
                   (read-bdd-distribution-data prefix :vars vars :max-exponent max-exponent
                                                      :min-kolmogorov min-kolmogorov :max-kolmogorov max-kolmogorov))))
    (when re-run
      (write-bdd-distribution-data data prefix))

    (labels ((get-data (exponent)
               (remove-if-not (lambda (plist)
                                (= exponent (getf plist :exponent 1))) data))
             (find-plist (num-vars exponent &key (data data))
               (cond
                 ((null data) nil)
                 (t
                  (destructuring-bind (plist &rest plists) data
                    (if (and (= num-vars (getf plist :num-vars))
                             (= exponent (getf plist :exponent)))
                        plist
                        (find-plist num-vars exponent :data plists))))))
             (samples-table (stream)
               (format stream "\\begin{tabular}{r|r|r}~%")
               (format stream "No.       & No.     & No. \\\\~%")
               (format stream "Variables & Samples & Unique \\\\~%")
               (format stream "$(n)$     & (M)     & Sizes \\\\~%")
               (format stream "\\hline~%")
               (loop :for n :from 5 :to max
                     :do (let ((sexp-file-name (format nil "~A/bdd-distribution-data-~D.sexp" prefix n)))
                           (with-open-file (sexp-file sexp-file-name :direction :input :if-does-not-exist :error)
                             (let ((sexp-plist (read sexp-file nil nil nil)))
                               (format stream "~D & ~:D & ~D\\\\~%" ;; ~:D prints with , separating 1000's
                                       n (getf sexp-plist :num-samples) (or (getf sexp-plist :unique-sizes)
                                                                            (length (getf sexp-plist :possible-sizes))))))))
               (format stream "\\hline~%")
               (format stream "\\end{tabular}~%"))
             (sqr (x)
               (* x x))
	     (scale-points (xys &key expo)
	       (destructuring-bind (expon scis) (scale-sci-notations (mapcar #'cdr xys) :expo expo)
		 (list expon
		       (mapcar (lambda (xy scaled)
				 (cons (car xy) scaled))
			       xys scis))))
             (individual-plot (stream num-vars &key (include-normal-distribution nil) (clip nil) (exponent 1) (plist (find-plist num-vars exponent))
                                                 (num-samples (getf plist :num-samples)) (counts (getf plist :counts))
                                                 (logx nil) (logy nil)
                                                 (comment nil)
                                                 (xlabel (lambda (num-vars)
                                                           (format nil "{Node count for \\numvars=~D, M=~D}" num-vars num-samples))))
               (when comment
                 (format stream "%~A~%" comment))
	       (destructuring-bind (&key num-samples
				      ((:density (alpha beta)) '(0 0))
				      (exponent 1)
				      sigma
				      ((:average-size mu)) &allow-other-keys) plist
		 (flet ((to-sci-notation (estimate)
			  (destructuring-bind (a b) (sci-notation (/ estimate alpha))
			    ;;   extrapolated estimate = normalized / density
			    ;;                         = normalized/alpha   * 10 ^ beta
			    ;;   if normalized/alpha   = a*10^b
			    ;;   then         estimate = a * 10 ^(b - beta)
			    (list (float a 1.0) (- b beta)))))
		   
		   (let* ((sigma^2 (sqr sigma))
			  (points 
			    ;; density is in form (alpha beta) meaning alpha * 10 ^ beta
			    (mapcan (lambda (item)
				      (destructuring-bind (bdd-size normalized number-of-bdds) item
					(declare (ignore normalized))
					(cond
					  ((and (<= number-of-bdds 2)
						clip)
					   nil)
					  (t
					   ;; normalized = normalized number of bdds of this size as a fraction of total sample
					   (destructuring-bind (a b) (to-sci-notation number-of-bdds)
					     (list (list bdd-size a b)))))))
				    counts)))
		     
		     ;; if exponent = 5, this means we only plot 1/(2^5= 1/32 of the points.
		     (tikzpicture stream
				  (format nil "individual plot ~D vars" num-vars) ; comment
				  (destructuring-bind (expo scaled) (scale-points points)
				    (lambda ()
				      (axis stream
					    (list 
					     (list "xlabel" (funcall xlabel num-vars))
					     "ymajorgrids"
					     "yminorgrids"
					     "xmajorgrids"
					     "xminorgrids"
					     (list "ylabel" (case expo
							      ((0) (format nil "{\\color{greeny} $\\HH{~D}{~D}(x)$}"
                                                                           num-samples num-vars))
							      ((1) (format nil "{\\color{greeny} $\\HH{~D}{~D}(x) \\times 10$}"
                                                                           num-samples num-vars))
							      (t   (format nil "{\\color{greeny} $\\HH{~D}{~D}(x) \\times 10^{~D}$}"
                                                                           num-samples num-vars expo))))
					     '("label style" "{font=\\Large}")
					     '("tick label style" "{font=\\Large}"))
					    (lambda ()
					      (addplot stream
						       nil
						       (list
							'("color" "greeny")
							'("mark" "*"))
						       "(~D,~Ae~A)"
						       scaled
						       :logx logx
						       :logy logy)
					      (when (and points include-normal-distribution)
						(let* ((x-min (reduce #'min points :key #'car))
						       (x-max (reduce #'max points :key #'car))
						       (x-step (/ (- x-max x-min) (length points) 4)))
						  (when (zerop x-step)
						    (setf x-step 1))
						  (let ((points (loop :for x :from x-min :to x-max :by x-step
								      :collect (let ((normalized (* (/ 1.0 (sqrt (* 2 pi sigma^2)))
												    (exp (- (/ (sqr (- x mu))
													       (* 2 sigma^2)))))))
										 (destructuring-bind (a b) (to-sci-notation (* num-samples normalized))
										   (list x a b))))))
						    (destructuring-bind (expo2 scaled) (scale-points points :expo expo)
						      (assert (= expo expo2) (expo expo2))
						      (addplot stream
							       (format nil "theoretical normal distrubution with N=~D M=~D exponent=~D sigma=~D and mu=~D"
								       num-vars num-samples exponent sigma mu)
							       '(("color" "red"))
							       "(~D,~Ae~A)"
							       scaled
							       :logx logx
							       :logy logy)))))
					      (format stream "\\legend{}~%"))
                                            :logx logx
                                            :logy logy))))))))
             (integral-plot (stream integral-xys &key num-vars)
               (tikzpicture stream
                            (format nil "Integral plot of N=~D" num-vars)
                            (lambda ()
                              (axis stream
                                    (list "xmajorgrids"
                                          "ymajorgrids"
                                          '("label style" "{font=\\Large}")
                                          '("xlabel" "M Number of points")
                                          (list "ylabel" (format nil "{$\\LL{M}{~D}$}" num-vars)))
                                    (lambda ()
                                      (addplot stream
                                               "integral plot"
                                               '(("mark" "triangle")
                                                 ("color" "blue"))
                                               "(~D,~D)"
                                               integral-xys
					       :logx t))
                                    :logx t))))
             (sigma-plot (stream &key (max max) (logy t) (xmarks nil) (exponent 1) (data (get-data exponent)))
               (tikzpicture stream
                            "sigma plot"
                            (lambda ()
                              (axis stream
                                    (list "ymajorgrids"
                                          '("xmin" 0)
                                          "yminorgrids"
                                          "xmajorgrids"
                                          '("xlabel" "Number of variables")
                                          '("ylabel" "Standard deviation")
                                          '("legend style" "{anchor=west,font=\\tiny}")
                                          (when xmarks
                                            (list "xtick"
                                                  (format nil "{~A}"
                                                          (join-strings ","
                                                                        (loop for xtick from 1
                                                                                to (reduce (lambda (max item)
                                                                                             (max max (getf item :num-vars)))
                                                                                           (cdr data)
                                                                                           :initial-value (getf (car data) :num-vars))
                                                                              collect (format nil "~D" xtick)))))))
                                    (lambda ()
                                      (addplot stream
                                               nil ; no comment
                                               '(("color" "blue")
                                                 ("mark" "*"))
                                               "(~D,~D)"
                                               (mapcan (lambda (plist)
                                                         (destructuring-bind (&key num-vars sigma &allow-other-keys) plist
                                                           (when (<= num-vars max)
                                                             (list (list num-vars
                                                                         (coerce sigma 'float))))))
                                                       data)
					       :logy logy))
                                    :logy logy))))
             (difference-plot (stream &key num-vars xys1 xys2 m1 m2)
               (flet ((3-tuple-to-2 (3-tuple)
                        (list (car 3-tuple) (cadr 3-tuple))))
                 (let* ((diff (difference-function (mapcar #'3-tuple-to-2 xys1)
                                                   (mapcar #'3-tuple-to-2 xys2)))
                        (integral (loop :for pts :on diff
                                        :when (cdr pts)
                                          :summing (destructuring-bind (pt0 pt1 &rest _) pts
                                                     (declare (ignore _))
                                                     (destructuring-bind (x0 y0) pt0
                                                       (destructuring-bind (x1 y1) pt1
                                                         (* (sqr (- y1 y0))
                                                                      (- x1 x0))))))))
                   (tikzpicture stream
                                (format nil "L2 distance between two successive curves N=~D M=~D vs M=~D"
                                        num-vars m1 m2)
                                (lambda ()
                                  (axis stream
                                        (list (list "ylabel" (format nil "{$\\Delta\\HH{~D}{~D}$}" m2  num-vars))
                                              "ymajorgrids"
                                              "xmajorgrids"
                                              '("label style" "{font=\\Large}")
                                              (list "xlabel"
                                                    (format nil "{~D-variable ROBDD size}" num-vars)))
                                        (lambda ()
                                          (addplot stream
                                                   "difference function"
                                                   '(("color" "blue"))
                                                   "(~D,~D)"
                                                   diff)
                                          integral)))))))
             (kolmogorov-sigma-plot (stream num-vars &key (logx t) (logy t) data)
               (when data
                 (tikzpicture stream
                              "standard deviation with successively more point samples"
                              (lambda (&aux (data (sort (copy-list data) #'< :key (getter :num-samples))))
				(list :average-excursion
				      (axis stream
					    (list "xmajorgrids"
						  '("scaled y ticks" "false")
						  "ylabel near ticks"
						  '("yticklabel pos" "right")
						  (list "ylabel" (format nil "{\\color{red} Average $\\mu_{~D}$}" num-vars))
						  (list "xlabel" (format nil "{Sample size M for \\numvars=~A}" num-vars)))
					    (lambda (&aux min-value max-value end-value)
					      (addplot stream
						       "average mu plot"
						       '(("color" "red")
							 ("mark" "*"))
						       "(~D,~D)"
						       (loop :for plist :in data
							     :for num-samples = (getf plist :num-samples)
							     :for average-size = (coerce (getf plist :average-size) 'float)
							     :minimize average-size :into mi
							     :maximize average-size :into ma
							     :collect (list num-samples average-size)
							     :finally (setf max-value ma
									    min-value mi
									    end-value average-size))
						       :logx logx
						       :logy logy)
					      (let ((excursion-percent (float (* 100.0 (/ (- max-value min-value) end-value)) 1.0)))
						(format stream "% mu min-value = ~A~%" (float min-value 1.0)) ; min
						(format stream "% mu max-value = ~A~%" (float max-value 1.0)) ; max
						(format stream "% mu end-value = ~A~%" (float end-value 1.0)) ; final
						(format stream "% mu excursion = ~A%~%" excursion-percent)
						(list :num-vars num-vars
						      :min-value min-value
						      :max-value max-value
						      :excursion excursion-percent
						      :end-value end-value)))
                                            :logx logx
                                            :logy logy)
				      :sigma-excursion
				      (axis stream
					    (list "ymajorgrids"
						  '("scaled y ticks" "false")
						  "yminorgrids"
						  "xmajorgrids"
						  (list "ylabel" (format nil "{\\color{blue} Standard Deviation $\\sigma_{~D}$}" num-vars))
						  (list "xlabel" (format nil "{Sample size M for \\numvars=~A}" num-vars)))
					    (lambda (&aux min-value max-value end-value)
					      (addplot stream
						       "standard deviation sigma plot"
						       '(("color" "blue")
							 ("mark" "*"))
						       "(~D,~D)"
						       (loop :for plist :in data
							     :for num-samples = (getf plist :num-samples)
							     :for sigma = (coerce (getf plist :sigma) 'float)
							     :minimize sigma :into mi
							     :maximize sigma :into ma
							     :collect (list num-samples sigma)
							     :finally (setf min-value mi
									    max-value ma
									    end-value sigma))
						       :logx logx
						       :logy logy)
					      (let ((excursion-percent (float (* 100.0 (/ (- max-value min-value) end-value)) 1.0)))
						(format stream "% sigma min-value = ~A~%" min-value) ; min
						(format stream "% sigm max-value = ~A~%" max-value) ; max
						(format stream "% sigma end-value = ~A~%" end-value) ; final
						(format stream "% sigma excursion = ~A%~%" excursion-percent)
						(list :num-vars num-vars
						      :min-value min-value
						      :max-value max-value
						      :end-value end-value
						      :excursion excursion-percent)))
                                            :logx logx
                                            :logy logy))))))
             (average-plot (stream &key (max max) (logy t) (xticks t) (exponent 1) (data (get-data exponent)))
               (when data
                 (tikzpicture stream
                              "generated by CL function average-plot"
                              (lambda ()
                                (axis stream
                                      (list (if logy
                                                nil
                                                '("ymin" "0"))
                                            "ymajorgrids"
                                            "yminorgrids"
                                            "xmajorgrids"
                                            '("xlabel" "Number of variables")
                                            '("ylabel" "ROBDD size")
                                            '("legend style" "{at={(0,1)},anchor=north west,font=\\tiny}")
                                            (when xticks
                                              (list "xtick"
                                                    (format nil "{~A}"
                                                            (join-strings "," (list* "0" "1"
                                                                                     (loop for xtick from 2
                                                                                             to (reduce (lambda (max item)
                                                                                                          (max max (getf item :num-vars)))
                                                                                                        (cdr data)
                                                                                                        :initial-value (getf (car data) :num-vars))
                                                                                           collect (format nil "~D" xtick))))))))
                                      (lambda ()
                                        (addplot stream
                                                 "worst case size"
                                                 '(("line width" "0.8pt")
                                                   ("style" "densely dotted")
                                                   ("color" "blue")
                                                   ("mark" "*"))
                                                 "(~D,~D)"
                                                 (mapcan (lambda (plist)
                                                           (destructuring-bind (&key num-vars counts &allow-other-keys) plist
                                                             (when (<= num-vars max)
                                                               (list (list num-vars
                                                                           (reduce #'max counts :key #'car :initial-value 0))))))
                                                         data)
						 :logy logy)
                                        (addplot stream
                                                 "average size"
                                                 '(("color" "teal")
                                                   ("mark" "triangle"))
                                                 "(~D,~D)"
                                                 (mapcan (lambda (plist)
                                                           (destructuring-bind (&key num-vars average-size &allow-other-keys) plist
                                                             (when (<= num-vars max)
                                                               (list (list num-vars
                                                                           (coerce average-size 'float))))))
                                                         data)
						 :logy logy)
                                        (addplot stream
                                                 "median size"
                                                 '(("line width" "0.8pt")
                                                   ("style" "dashed")
                                                   ("color" "greeny")
                                                   ("mark" "diamond"))
                                                 "(~D,~D)"
                                                 (mapcan (lambda (plist)
                                                           (destructuring-bind (&key num-vars median &allow-other-keys) plist
                                                             (when (<= num-vars max)
                                                               (list (list num-vars median)))))
                                                         data)
						 :logy logy)
                                        (format stream "\\legend{Worst case, Average, Median}~%"))
                                      :logy logy)))))
             (efficiency-plot (stream &key (exponent 1) (data (get-data exponent)))
               (when data
                 (flet ((residual-compression-ratio (value num-vars)
                          (/ value (1- (expt 2.0 (1+ num-vars))))))
                   (tikzpicture stream
                                "Residual compression ratio plot"
                                (lambda ()
                                  (axis stream
                                        (list "ymajorgrids"
                                              "yminorgrids"
                                              "xmajorgrids"
                                              '("xlabel" "Number of variables")
                                              '("ylabel" "Residual compression ratio")
                                              '("legend style" "{at={(1,1)},anchor=north east,font=\\tiny}")
                                              (list "xtick"
                                                    (format nil "{~A}"
                                                            (join-strings "," (loop for xtick from 2 by 2
                                                                                      to (+ 2 (reduce (lambda (max item)
                                                                                                   (max max (getf item :num-vars)))
                                                                                                 (cdr data)
                                                                                                 :initial-value (getf (car data) :num-vars)))
                                                                                    collect (format nil "~D" xtick))))))
                                        (lambda ()
                                          (addplot stream
                                                   "worst case"
                                                   '(("line width" "0.8pt")
                                                     ("style" "densely dotted")
                                                     ("color" "blue")
                                                     ("mark" "*"))
                                                   "(~D , ~D)"
                                                   (mapcar (lambda (plist)
                                                             (destructuring-bind (&key num-vars counts &allow-other-keys) plist
                                                               (list num-vars
                                                                     (residual-compression-ratio (reduce #'max counts :key #'car :initial-value 0.0)
                                                                                                 num-vars))))
                                                           data))
                                          (addplot stream
                                                   "average size"
                                                   '(("color" "teal")
                                                     ("mark" "triangle"))
                                                   "(~D , ~D)"
                                                   (mapcar (lambda (plist)
                                                             (destructuring-bind (&key num-vars average-size &allow-other-keys) plist
                                                               (list num-vars
                                                                     (residual-compression-ratio (coerce average-size 'float) num-vars))))
                                                           data))
                                          (addplot stream
                                                   "median"
                                                   '(("line width" "0.8pt")
                                                     ("style" "dashed")
                                                     ("color" "greeny")
                                                     ("mark" "diamond"))
                                                   "(~D , ~D)"
                                                   (mapcar (lambda (plist)
                                                             (destructuring-bind (&key num-vars median &allow-other-keys) plist
                                                               (list num-vars
                                                                     (residual-compression-ratio median num-vars))))
                                                           data))
                                          (format stream "\\legend{Worst case, Average, Median}~%"))
                                        :logy t))))))
             (size-plots (stream &key (max 19) (logx t) (mark t) (colors colors) ((:exponent given-exponent) 1) &aux legend)
               (declare (type unsigned-byte given-exponent))
               (tikzpicture stream
                            "normalized size plots"
                            (lambda ()
                              (axis stream
                                    (list '("xlabel" "BDD Size")
                                          "ymajorgrids"
                                          "yminorgrids"
                                          "xmajorgrids"
                                          "xminorgrids"
                                          '("ylabel" "Probability")
                                          '("legend style" "{font=\\tiny,at={(1,0)},anchor=south west}")
                                          '("label style" "{font=\\tiny}"))
                                    (lambda ()
                                      (dolist (datum data)
                                        (destructuring-bind (&key num-vars ((:exponent this-exponent) 1) counts &allow-other-keys) datum
                                          (declare (type unsigned-byte this-exponent))
                                          (when (and (> num-vars 1)
                                                     (= this-exponent given-exponent)
                                                     (<= num-vars max))
                                            (let ((label (format nil "Size with ~D variables" num-vars)))
                                              (push label legend)
                                              (addplot stream
                                                       label
                                                       (list (list "color" (cond
                                                                             ((null mark)
                                                                              "black")
                                                                             ((pop colors))
                                                                             (t
                                                                              "black"))))
                                                       "  (~D,~A)"
                                                       counts
						       :logx logx
                                                       :addplot (if mark "addplot+" "addplot") )))))
                                      (format stream "\\legend{~A}~%"
                                              (join-strings "," (reverse legend))))
                                    :logx logx))))
             (write-excursion-summary (stream average-excursion-summary sigma-excursion-summary)
               (format stream "\\begin{tabular}{crr}~%")
               (format stream "\\hline~%")
               (format stream "$\\numvars$~%")
               (format stream "& $\\frac{{\\mu_{\\numvars}}_{max} - {\\mu_{\\numvars}}_{min}}{{\\mu_{\\numvars}}_{final}} \\times 100\\%$~%")
               (format stream "& $\\frac{{\\sigma_{\\numvars}}_{max} - {\\sigma_{\\numvars}}_{min}}{{\\sigma_{\\numvars}}_{final}} \\times 100\\%$ \\\\~%")
               (format stream "\\hline~%")
               (loop :for sigma-excursion   :in (sort (copy-list sigma-excursion-summary) #'<
						      :key (getter :num-vars))
                     :for average-excursion :in (sort (copy-list average-excursion-summary) #'<
                                                      :key (getter :num-vars))
                     :do (format stream "~A~%" (getf sigma-excursion :num-vars))
                     :do (dolist (summary (list average-excursion sigma-excursion))
                           (destructuring-bind (&key max-value min-value end-value excursion &allow-other-keys) summary
                             (format stream "& $\\frac{~,3f - ~,3f}{~,3f} = ~,2f\\%$~%"
                                     (float max-value 1.0) (float min-value 1.0) (float end-value 1.0) (float excursion 1.0))))
                     :do (format stream "\\\\~%"))
               (format stream "\\hline~%")
               (format stream "\\end{tabular}~%")))

      (with-open-file (stream (format nil "~A/bdd-samples-table.ltxdat" prefix)
                              :direction :output :if-does-not-exist :create :if-exists :supersede)
        (format t "writing to ~A~%" stream)
        (samples-table stream))
      (with-open-file (stream (format nil "~A/bdd-distribution-sigma.ltxdat" prefix)
                              :direction :output :if-does-not-exist :create :if-exists :supersede)
        (format t "writing to ~A~%" stream)
        (sigma-plot stream :max max :logy t :xmarks nil))
      (with-open-file (stream (format nil "~A/bdd-distribution-sigma-2-8.ltxdat" prefix)
                              :direction :output :if-does-not-exist :create :if-exists :supersede)
        (format t "writing to ~A~%" stream)
        (sigma-plot stream :max 8 :logy nil :xmarks t))
      (with-open-file (stream (format nil "~A/bdd-distribution.ltxdat" prefix)
                              :direction :output :if-does-not-exist :create :if-exists :supersede)
        (format t "writing to ~A~%" stream)
        (size-plots stream :max max :logx t :mark nil))
      (with-open-file (stream (format nil "~A/bdd-distribution-2-8.ltxdat" prefix)
                              :direction :output :if-does-not-exist :create :if-exists :supersede)
        (format t "writing to ~A~%" stream)
        (size-plots stream :max 8 :logx nil :mark t))
      (with-open-file (stream (format nil "~A/bdd-distribution-expected.ltxdat" prefix)
                              :direction :output :if-does-not-exist :create :if-exists :supersede)
        (format t "writing to ~A~%" stream)
        (average-plot stream :logy t :xticks nil))
      (with-open-file (stream (format nil "~A/bdd-distribution-expected-2-8.ltxdat" prefix)
                              :direction :output :if-does-not-exist :create :if-exists :supersede)
        (format t "writing to ~A~%" stream)
        (average-plot stream :max 8 :logy nil :xticks t))
      (with-open-file (stream (format nil "~A/bdd-efficiency-sample.ltxdat" prefix)
                              :direction :output :if-does-not-exist :create :if-exists :supersede)
        (format t "writing to ~A~%" stream)
        (efficiency-plot stream))
      (loop for num-vars from min to max
            do (if (getf (find-plist num-vars 1) :counts)
                   (with-open-file (stream (format nil "~A/bdd-distribution-~D.ltxdat" prefix num-vars)
                                           :direction :output :if-does-not-exist :create :if-exists :supersede)
                     (format t "writing to ~A~%" stream)
                     (individual-plot stream num-vars :include-normal-distribution nil
                                                      :clip (> num-vars 10)
                                                      :counts (getf (find-plist num-vars 1) :counts)))
                   (warn "no data to plot, skipping ~A" (format nil "~A/bdd-distribution-~D.ltxdat" prefix num-vars))))
      (let (average-excursion-summary
            sigma-excursion-summary)
        (loop :for num-vars :from min-kolmogorov :to max-kolmogorov
              :do
                 (let (integral-xys)
                   (loop :for exponent :from 1 :to max-exponent
                         :do
                            (let ((fname (format nil "~A/bdd-distribution-kolmogorov-~D-~D.ltxdat" prefix exponent num-vars)))
                              (if (getf (find-plist num-vars exponent) :counts)
                                  (with-open-file (stream fname
                                                          :direction :output :if-does-not-exist :create :if-exists :supersede)
                                    (format t "writing to ~A~%" stream)
                                    (individual-plot stream num-vars
                                                     :exponent exponent
                                                     :counts (getf (find-plist num-vars exponent) :counts)
                                                     :clip t
                                                     :xlabel (lambda (num-vars)
                                                               (format nil "{~D-var ROBDD size}" num-vars))))
                                  (warn "no data to plot ~A~%" fname)))
                            (let ((fname (format nil "~A/bdd-distribution-kolmogorov-~D-~D+normal.ltxdat" prefix exponent num-vars)))
                              (if (getf (find-plist num-vars exponent) :counts)
                                  (with-open-file (stream fname
                                                          :direction :output :if-does-not-exist :create :if-exists :supersede)
                                    (format t "writing to ~A~%" stream)
                                    (individual-plot stream num-vars
                                                     :include-normal-distribution t
                                                     :exponent exponent
                                                     :logx nil
                                                     :logy nil
                                                     :clip t
                                                     :counts (getf (find-plist num-vars exponent) :counts)
                                                     :xlabel (lambda (num-vars)
                                                               (format nil "{~D-var ROBDD size}" num-vars))))
                                  (warn "no data to plot ~A~%" fname)))
                            (when (> exponent 1)
                              (let ((fname (format nil "~A/delta-N+~D-exp+~D-exp+~D.ltxdat" prefix num-vars exponent (1- exponent))))
                                (if (getf (find-plist num-vars exponent) :counts)
                                    (with-open-file (stream fname
                                                            :direction :output :if-does-not-exist :create :if-exists :supersede)
                                      (format t "writing to ~A~%" stream)
                                      (push (list (getf (find-plist num-vars exponent) :num-samples)
                                                  (difference-plot stream
                                                                   :num-vars num-vars
								   :m1 (getf (find-plist num-vars exponent) :num-samples)
								   :m2 (getf (find-plist num-vars (1- exponent)) :num-samples)
                                                                   :xys1 (getf (find-plist num-vars exponent) :counts)
                                                                   :xys2 (getf (find-plist num-vars (1- exponent)) :counts)))
                                            integral-xys))
                                    (warn "no data to plot ~A~%" fname)))))
                   (let ((fname (format nil "~A/integral-~D.ltxdat" prefix num-vars)))
                     (when (cdr integral-xys)
                       (with-open-file (stream fname :direction :output :if-does-not-exist :create :if-exists :supersede)
                         (format t "writing to ~A~%" stream)
                         (integral-plot stream integral-xys :num-vars num-vars)))))
              :do
                 (let ((sigma-name (format nil "~A/sigma-kolmogorov-~D.ltxdat" prefix num-vars))
                       (data (setof plist data
                               (= num-vars (getf plist :num-vars)))))
                   (with-open-file (stream sigma-name :direction :output :if-does-not-exist :create :if-exists :supersede)
                     (format t "writing to ~A~%" stream)
                     (when data
		       (destructuring-bind (&key sigma-excursion average-excursion) (kolmogorov-sigma-plot stream num-vars :data data :logy nil)
			 (push sigma-excursion sigma-excursion-summary)
			 (push average-excursion average-excursion-summary))))))
        (with-open-file (stream (format nil "~A/excursion-summary.ltxdat" prefix) :direction :output :if-does-not-exist :create :if-exists :supersede)
          (format t "writing to ~A~%" stream)
          (write-excursion-summary stream average-excursion-summary sigma-excursion-summary))))
    data))

(defun all-possible-bdds (prefix vars &aux (num-vars (length vars)))
  (declare #+sbcl (notinline sort)
           (type string prefix)
           (type list vars)
          )
  (let ((bdd-data (bdd-with-new-hash ()
                    (loop for truth-table from 0 below (expt 2 (expt 2 num-vars))
                          collect (let* ((expr (int-to-boolean-expression truth-table vars))
                                         (bdd (bdd expr)))
                                    (list :bdd bdd
                                          :node-count (bdd-count-nodes bdd)
                                          :expr (bdd-to-dnf bdd))))
                       :verbose nil))
        (uniq 1000))
    
    (sort (loop for data in (sort bdd-data #'< :key (getter :node-count))
          collect (destructuring-bind (&key bdd expr node-count &allow-other-keys) data
                    (list :node-count node-count
                          :num-vars num-vars
                          :path (bdd-to-png bdd :reduced t
                                                :basename (format nil "~A/vars=~D-~D-~D"
                                                                  prefix num-vars node-count (incf uniq)))
                          :expr expr)))
          #'< :key (getter :node-count))))

(defun all-possible-bdds-latex (prefix vars)
  (declare (type list vars))
  (with-open-file (latex (format nil "~A/all-robdds-~A.ltx" prefix (length vars))
                         :direction :output :if-exists :supersede :if-does-not-exist :create)
    (when latex
      (format latex "\\begin{table}~%")
      (format latex "\\begin{center}~%")
      (format latex "\\begin{tabular}{c|c|l}~%")
      (format latex "No. Nodes & ROBDD & Boolean Expression\\\\~%")
      (format latex "\\hline~%")
      (dolist (data (all-possible-bdds prefix vars))
	(destructuring-bind (&key path node-count expr &allow-other-keys) data
          (format latex "~D " node-count)
          (format latex "& \\includegraphics[width=0.5in]{~A}~%" (pathname-name (pathname path)))
          (format latex "&~%")
          (format latex "\\begin{minipage}{2in}")
          (format latex "\\begin{verbatim}~%")
          (format latex "~A~%" expr)
          (format latex "\\end{verbatim}~%")
          (format latex "\\end{minipage}\\\\~%")))
      (format latex "\\hline~%")
      (format latex "\\end{tabular}~%")
      (format latex "\\end{center}~%")
      (format latex "\\caption{All ROBDDs of ~r Variable~:p }~%" (length vars))
      (format latex "\\label{fig.robdds.of.size.~D}~%" (length vars))
      (format latex "\\end{table}~%"))))


;; e.g. input-pattern "/lrde/home/jnewton/cluster.*/bdd-sizes.*.master.lrde.epita.*"
;; output-directory "/lrde/cluster/jnewton/bdd-sizes"
(defun combine-bdd-size-results (output-directory input-paths)
  (let* ((stream (list :stream (open "/dev/null" :direction :output :if-exists :append :if-does-not-exist :error)
                      :num-vars 0)))
        
    (labels ((stream-to (num-vars)
               (cond
                 ((= num-vars (getf stream :num-vars))
                  (getf stream :stream))
                 (t
                  (close (getf stream :stream))
                  (setf (getf stream :num-vars) num-vars
                        (getf stream :file) (format nil "~a/bdd-sizes-unique-~D.new"
                                                    output-directory
                                                    num-vars)
                        (getf stream :stream) (open (getf stream :file)
                                                    :if-does-not-exist :create
                                                    :if-exists :append
                                                    :direction :output))
                  (getf stream :stream))))
             (process-file (fname)
               (with-open-file (log-stream fname :direction :input :if-does-not-exist :error)
                 (let (num-vars (line-num 1))
                   (loop :while (setf num-vars (read log-stream nil nil nil))
                         :do (let* ((out-stream (stream-to num-vars))
                                    ;; get file write position
                                    ;;(start-of-line (file-position out-stream))
                                    )
                               
			       (format out-stream "~D " num-vars)
			       (let ((char (read-char log-stream nil nil nil)))
				 (loop :while (not (member char '(#\; #\Linefeed #\Return)))
				       :do (write-char char out-stream)
				       :do (setf char (read-char log-stream nil nil nil)))
                                 ;; we have read to the first occurnace of either EOL or ;,
                                 ;; if we found EOL before ; then then line is corrupt
                                 ;; and we need to discard it.
                                 (cond
                                   ((char= char '#\;)
                                    (terpri out-stream))
                                   (t
                                    (error "ignoring corrupted line=~D of ~A" line-num log-stream)
                                    ;;(file-position out-stream start-of-line)
                                    ))))
                         :do (incf line-num))))))
      (mapcar #'process-file input-paths))
    (close (getf stream :stream))))

(defun generate-latex-plots (&key (analysis-dir "/Users/jnewton/analysis")
                               (bin-dir "/Users/jnewton/sw/regular-type-expression/bin")
                               (autogen-dir "/Users/jnewton/research/autogen/.")
			       (gen-samples nil)
			       (max-num-vars 18)
			       (max-exponent 8))
  (loop :for n :from 5 :to max-num-vars
        :do (measure-and-write-bdd-distribution (format nil "~A/." analysis-dir) n 1 
                                                (format nil "~A/bdd-sizes-unique-~D.2-columns" analysis-dir n)
                                                :read-from-log-p t))
  
  (loop :for n :from 5 :to max-num-vars
	:for sample-fname = (format nil "~A/bdd-sizes-unique-~D.2-columns" analysis-dir n)
        :when (or gen-samples
		  (not (probe-file sample-fname)))
	  :do (generate-sample-files sample-fname 2 max-exponent)
        :do (loop :for exponent :from 2 :to max-exponent
                  :do (measure-and-write-bdd-distribution analysis-dir n 1 
                                                          (format nil "~A/bdd-sizes-unique-~D.2-columns" analysis-dir n)
                                                          :exponent exponent
                                                          :read-from-log-p t)))
  (latex-measure-bdd-sizes analysis-dir *bdd-boolean-variables* 1000
                           :min 2 :max max-num-vars :re-run nil
                           :max-exponent max-exponent
                           :max-kolmogorov max-num-vars
                           :min-kolmogorov 5)
  (when (and bin-dir
             autogen-dir
             (not (string= autogen-dir analysis-dir)))
    (run-program (format nil "~A/copy-latex.sh" bin-dir) (list autogen-dir))))
