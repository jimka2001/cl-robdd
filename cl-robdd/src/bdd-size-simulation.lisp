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


(defun random-boolean-combination (vars)
  "return a randomly selection boolean combination of the given BOOLEAN variables in sum-of-minterms form (or (and ...) (and ...) ...)"
  ;; vars is a list of symbols
  (int-to-boolean-expression (random (expt 2 (expt 2 (length vars))))
                             vars))

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
                                         (set-difference a-list couple :test #'eq)
                                         #'<
                                         :key #'car))))))
    (values (caar a-list) a-list)))

(defun make-announcement-timer (min max interval announce)
  "Given a MIN and MAX iteration (integers) and an integer INTERVAL designating a number
 of seconds, and a unary function ANNOUNCE.
return a unary function which can later be called with each iteration from MIN to MAX and
will call the ANNOUNCE function if the elapsed time since the most recent call is more 
than INTERVAL number of seconds"
  (declare (type (function (integer number) t) announce)
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
           (funcall announce iteration (coerce remaining-seconds 'double-float))))))))

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
           (mean (reduce (lambda (sum this)
                           (destructuring-bind (sample probability) this
                             (+ sum (* probability sample))))
                         normalized-histogram
                         :initial-value 0))
           (stdev (sqrt (reduce (lambda (sum this)
                                  (destructuring-bind (sample probability) this
                                    (+ sum (* probability (sqr (- sample mean))))))
                                normalized-histogram :initial-value 0.0)))
           (ffff (1- (expt 2 (expt 2 num-vars))))
           (density (/ num-samples (1+ ffff))))

      (let (sum average-size median)
        (declare #+sbcl (notinline sort))
        (setf sum (reduce #'+ histogram :initial-value 0 :key #'cadr))
        (setf average-size (/ (reduce (lambda (old item)
                                        (destructuring-bind (sample occurances) item
                                          (+ old (* sample occurances)))) histogram :initial-value 0) sum))
        (setf median (median-a-list histogram))
        (list :sum sum
              :num-samples num-samples
              :exponent exponent
              :randomp randomp
              :num-vars num-vars
              :density (sci-notation density)
              :average-size mean
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
           (format stream "~A ~A ~36R~%" num-vars bdd-count truth-table)))
    (typecase bdd-sizes-file
    (string
     (with-open-file (log-file bdd-sizes-file
                               :direction :output
                               :if-does-not-exist :create
                               :if-exists :append)
       (print-it log-file)))
    (t
     (print-it bdd-sizes-file)))))

(defun read-counts-from-log (target-num-vars bdd-sizes-file &key (exponent 1))
  (with-open-file (log-file bdd-sizes-file
                            :direction :input
                            :if-does-not-exist :error)
    (let (num-vars bdd-size samples)
      (while (setf num-vars (read log-file nil nil nil))
        ;; read the bdd-size integer
        (setf bdd-size (read log-file t nil nil))
        (when (and (= target-num-vars num-vars)
                   (zerop (random (expt 2 (1- exponent)))))
          (push bdd-size samples))
        ;; read and ignore the base-36 integer
        ;; read to end of line
        (let (char)
          (while (not (member char '(#\Linefeed #\Return)))
            (setf char (read-char log-file nil nil nil)))))
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
                (error "fewer than ~D samples in log file ~A" num-samples bdd-sizes-file)))))

      (let ((announcer (make-announcement-timer
                        2 (1- num-samples)
                        interval
                        (lambda (iteration remaining-seconds)
                          (format t "~D iteration=~D: " num-vars iteration)
                          (let ((seconds (truncate remaining-seconds))
                                (minutes (coerce (/ remaining-seconds 60) 'float))
                                (hours (coerce (/ remaining-seconds (* 60 60)) 'float)))
                            (format t "seconds remaining ~D" seconds)
                            (when (> minutes 1)
                              (format t " = ~D minutes" (truncate minutes)))
                            (when (> hours 1)
                              (format t " = ~D hours ~D minutes" (truncate hours)
                                      (truncate (- minutes (* 60 (truncate hours))))))
                            (format t "~%"))))))
        (if read-from-log-p
            (format t "using pre-chosen ~D" num-samples)
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
        (setf histogram (sort histogram #'< :key #'car))
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
                      (t (format nil "~A/bdd-distribution-data-~D-sample-~D.sexp" prefix num-vars exponent)))))
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

(defvar *bdd-boolean-variables* '(zm zl zk zj zi zh zg zf ze zd zc zb za z9 z8 z7 z6 z5 z4 z3 z2 z1))

(defun read-bdd-distribution-data (prefix &key (min 1) (max (length *bdd-boolean-variables*)) (kolmogorov '(8 11 18)) vars)
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
     (loop :for var :in kolmogorov
           :nconc
           (loop :for exponent :from 2 :to 8
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

(defun latex-measure-bdd-sizes (prefix vars num-samples &key (min 1) (max (length vars)) (re-run t))
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
                   (sort (measure-bdd-sizes vars num-samples min max) #'< :key (getter :num-vars))
                   (read-bdd-distribution-data prefix :vars vars))))
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
               (format stream "No.      & No.      & No. \\\\~%")
               (format stream "Variables & Samples & Unique \\\\~%")
               (format stream "$(n)$         &          & Sizes \\\\~%")
               (format stream "\\hline~%")
               (loop :for n :from 5 :to max
                     :do (let ((sexp-file-name (format nil "~A/bdd-distribution-data-~D.sexp" prefix n)))
                           (with-open-file (sexp-file sexp-file-name :direction :input :if-does-not-exist :error)
                             (let ((sexp-plist (read sexp-file nil nil nil)))
                               (format stream "~D & ~D & ~D\\\\~%"
                                       n (getf sexp-plist :num-samples) (or (getf sexp-plist :unique-sizes)
                                                                            (length (getf sexp-plist :possible-sizes))))))))
               (format stream "\\hline~%")
               (format stream "\\end{tabular}~%")

               )
             (join-strings (delimeter strings)
               (with-output-to-string (str)
                 (when (car strings)
                   (format str "~A" (car strings)))
                 (dolist (string (cdr strings))
                   (format str "~A~A" delimeter string))))
             (print-option (axis-option)
               (typecase axis-option
                 (string (format nil "~A" axis-option))
                 ((cons string (cons string)) (format nil "~A=~A" (car axis-option) (cadr axis-option)))
                 ((cons string (cons fixnum)) (format nil "~A=~D" (car axis-option) (cadr axis-option)))
                 (t
                  (error "unknown axis-option ~A" axis-option))))
             (tikzpicture (stream comment axis-options continuation)
               (declare (type (or null string) comment)
                        (type list axis-options)
                        (type (function () t) continuation))
               (when comment
                 (format stream "% ~A~%" comment))
               (format stream "\\begin{tikzpicture}~%")
               (format stream "\\begin{axis}[~% ~A~%]~%"
                       (join-strings (format nil ",~% ") (mapcar  #'print-option (remove nil axis-options))))
               (funcall continuation)
               (format stream "\\end{axis}~%")
               (format stream "\\end{tikzpicture}~%"))
             (addplot (stream comment plot-options control-string points &key (addplot "addplot"))
               (declare (type (or null string) comment)
                        (type list plot-options)
                        (type string control-string)
                        (type list points))
               (when comment
                 (format stream "% ~A~%" comment))
               (format stream "\\~A[~A] coordinates {~%" addplot
                       (join-strings "," (mapcar #'print-option plot-options)))
               (dolist (point points)
                 (apply #'format stream control-string point)
                 (terpri stream))
               (format stream "};~%"))
             (individual-plot (stream num-vars &key (plist (find-plist num-vars 1)) (counts (getf plist :counts))
                                                 (xlabel (lambda (num-vars)
                                                           (format nil "Node count for ~D variables" num-vars))))
               ;; if exponent = 5, this means we only plot 1/(2^5= 1/32 of the points.
               (tikzpicture stream
                            (format nil "individual plot ~D vars" num-vars) ; comment
                            (list 
                             (list "xlabel" (funcall xlabel num-vars))
                             "ymajorgrids"
                             "yminorgrids"
                             "xmajorgrids"
                             "xminorgrids"
                             '("ylabel" "Number of Boolean functions")
                             '("label style" "{font=\\large}")
                             '("tick label style" "{font=\\Large}"))
                            (lambda ()
                              (addplot stream
                                       nil ; no comment
                                       '(("color" "blue")
                                         ("mark" "*"))
                                       "(~D,~Ae~A) % ~D"
                                       (destructuring-bind (alpha beta) (getf plist :density)
                                         ;; density is in form (alpha beta) meaning alpha * 10 ^ beta
                                         (mapcan (lambda (item)
                                                   (destructuring-bind (bdd-size normalized number-of-bdds) item
                                                     (declare (ignore normalized))
                                                     (cond
                                                       ((and (<= number-of-bdds 2)
                                                             (> num-vars 10))
                                                        nil)
                                                       (t
                                                        ;; normalized = normalized number of bdds of this size as a fraction of total sample
                                                        (destructuring-bind (x y) (sci-notation (/ number-of-bdds alpha))
                                                          ;;   extrapolated estimate = normalized / density
                                                          ;;                         = normalized/alpha   * 10 ^ beta
                                                          ;;   if normalized/alpha   = x*10^y
                                                          ;;   then         estimate = x * 10 ^(y - beta)
                                                          (list (list bdd-size (float x 1.0) (- y beta) number-of-bdds)))))))
                                                 counts)))
                              (format stream "\\legend{}~%"))))
             (sigma-plot (stream &key (max max) (logy t) (xmarks nil) (exponent 1) (data (get-data exponent)))
               (tikzpicture stream
                            "sigma plot"
                            (list "ymajorgrids"
                                  '("xmin" 0)
                                  (if logy
                                    '("ymode" "log")
                                    '("ymin" 0))
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
                                               data)))))
             (kolmogorov-sigma-plot (stream num-vars &key (logx t) (logy t) data)
               (tikzpicture stream
                            "standard deviation with successively more point samples"
                            (list "ymajorgrids"
                                  (when logx
                                    '("xmode" "log"))
                                  (when logy
                                    '("ymode" "log"))
                                  '("scaled y ticks" "false")
                                  "yminorgrids"
                                  "xmajorgrids"
                                  (list "xlabel" (format nil "~A-variable number of samples" num-vars))
                                  '("ylabel" "Standard deviation"))
                            (lambda ()
                              (addplot stream
                                       nil ; no comment
                                       '(("color" "blue")
                                         ("mark" "*"))
                                       "(~D,~D)"
                                       (mapcar (lambda (plist)
                                                 (destructuring-bind (&key num-samples sigma &allow-other-keys) plist
                                                   (list num-samples (coerce sigma 'float))))
                                               (sort (copy-list data) #'< :key (getter :num-samples)))))))
             (kolmogorov-average-plot (stream num-vars &key (logx t) (logy t) data)
               (tikzpicture stream
                            "average with successively more point samples"
                            (list "ymajorgrids"
                                  (when logy
                                    '("ymode" "log"))
                                  (when logx
                                    '("xmode" "log"))
                                  '("scaled y ticks" "false")
                                  "yminorgrids"
                                  "xmajorgrids"
                                  (list "xlabel" (format nil "~A-variable number of samples" num-vars))
                                  '("ylabel" "Average"))
                            (lambda ()
                              (addplot stream
                                       nil ; no comment
                                       '(("color" "blue")
                                         ("mark" "*"))
                                       "(~D,~D)"
                                       (mapcar (lambda (plist)
                                                 (destructuring-bind (&key num-samples average-size &allow-other-keys) plist
                                                   (list num-samples (coerce average-size 'float))))
                                               (sort (copy-list data) #'< :key (getter :num-samples)))))))                            
             (average-plot (stream &key (max max) (logy t) (xticks t) (exponent 1) (data (get-data exponent)))
               (tikzpicture stream
                            "generated by CL function average-plot"
                            (list (if logy
                                      '("ymode" "log")
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
                                                  (join-strings "," (list* "0" "1" (loop for xtick from 2
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
                                               data))
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
                                               data))
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
                                               data))
                              (format stream "\\legend{Worst case, Average, Median}~%"))))
             (efficiency-plot (stream &key (exponent 1) (data (get-data exponent)))
               (flet ((residual-compression-ratio (value num-vars)
                        (/ value (1- (expt 2.0 (1+ num-vars))))))
                 (tikzpicture stream
                              "Residual compression ratio plot"
                              (list '("ymin" 0)
                                    "ymajorgrids"
                                    "yminorgrids"
                                    "xmajorgrids"
                                    '("xlabel" "Number of variables")
                                    '("ylabel" "Residual compression ratio")
                                    '("legend style" "{at={(1,1)},anchor=north east,font=\\tiny}")
                                    '("ytick" "{0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0}")
                                    (list "xtick"
                                          (format nil "{~A}"
                                                  (join-strings "," (list* "0" "1"
                                                                           (loop for xtick from 2
                                                                                   to (reduce (lambda (max item)
                                                                                                (max max (getf item :num-vars)))
                                                                                              (cdr data)
                                                                                              :initial-value (getf (car data) :num-vars))
                                                                                 collect (format nil "~D" xtick)))))))
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
                                (format stream "\\legend{Worst case, Average, Median}~%")))))
             (size-plots (stream &key (max 19) (logx t) (mark t) (colors colors) ((:exponent given-exponent) 1) &aux legend)
               (declare (type unsigned-byte given-exponent))
               (tikzpicture stream
                             "normalized size plots"
                             (list (when logx
                                     '("xmode" "log"))
                                   '("xlabel" "BDD Size")
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
                                                :addplot (if mark "addplot+" "addplot") )))))
                               (format stream "\\legend{")
                               (let ((first t))
                                 (dolist (label (reverse legend))
                                   (unless first
                                     (format stream ","))
                                   (format stream "~S" label)
                                   (setf first nil)))
                               (format stream "}~%")))))

      (with-open-file (stream (format nil "~A/bdd-samples-table.ltx" prefix)
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
                     (individual-plot stream num-vars :counts (getf (find-plist num-vars 1) :counts)))
                   (warn "no data to plot, skipping ~A" (format nil "~A/bdd-distribution-~D.ltxdat" prefix num-vars))))
      (dolist (num-vars '(8 11 18))
        (dolist (exponent '(1 2 3 4 5 6 7 8))
          (let ((fname (format nil "~A/bdd-distribution-kolmogorov-~D-~D.ltxdat" prefix exponent num-vars)))
            (if (getf (find-plist num-vars exponent) :counts)
                (with-open-file (stream fname
                                        :direction :output :if-does-not-exist :create :if-exists :supersede)
                  (format t "writing to ~A~%" stream)
                  (individual-plot stream num-vars
                                   :counts (getf (find-plist num-vars exponent) :counts)
                                   :xlabel (lambda (num-vars)
                                             (format nil "~D-var distrib. w/ ~D samples"
                                                     num-vars (getf (find-plist num-vars exponent) :num-samples)))))
                (warn "no data to plot ~A~%" fname))))
        (let ((sigma-name (format nil "~A/sigma-kolmogorov-~D.ltxdat" prefix num-vars))
              (average-name (format nil "~A/average-kolmogorov-~D.ltxdat" prefix num-vars))
              (data (setof plist data
                      (= num-vars (getf plist :num-vars)))))
          (with-open-file (stream sigma-name :direction :output :if-does-not-exist :create :if-exists :supersede)
            (format t "writing to ~A~%" stream)
            (kolmogorov-sigma-plot stream num-vars :data data :logy nil))
          (with-open-file (stream average-name :direction :output :if-does-not-exist :create :if-exists :supersede)
            (format t "writing to ~A~%" stream)
            (kolmogorov-average-plot stream num-vars :data data :logy nil)))))
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
                      :num-vars 0))
         (num-files (length input-paths)))
        
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
               (format t "processing ~D ~A~%" (decf num-files) fname)
               (with-open-file (log-stream fname :direction :input :if-does-not-exist :error)
                 (let (num-vars)
                   (loop :while (setf num-vars (read log-stream nil nil nil))
                         :do (let ((out-stream (stream-to num-vars)))
			       (format out-stream "~D " num-vars)
			       (let ((char (read-char log-stream nil nil nil)))
				 (loop :while (not (member char '(#\Linefeed #\Return)))
				       :do (write-char char out-stream)
				       :do (setf char (read-char log-stream nil nil nil))))
			       (terpri out-stream)))))))
      (mapcar #'process-file input-paths))
    (close (getf stream :stream))))
