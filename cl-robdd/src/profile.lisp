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

(in-package :cl-robdd-analysis)


#+nil(defun gather-profiling (thunk)
  (let* ((graph       (sb-sprof::make-call-graph most-positive-fixnum))
         (interesting (map 'list (lambda (name)
                                   (find name (sb-sprof::call-graph-vertices graph)
                                         :key #'sb-sprof::node-name))
                           (funcall thunk))))
    (map 'list (lambda (node)
                 (cons (sb-sprof::node-name node)
                       (/ (sb-sprof::node-accrued-count node)
                          (sb-sprof::call-graph-nsamples graph))))
         interesting)))

(defun delimiterp (c)
  (or (char= c #\Newline)))

(defun split-str (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
      :while end))

(defun clean-sprofiling-plist (plist)
  (flet ((clean-count-percent (key &aux (value (getf plist key)))
           (let ((count (getf value :count 0))
                 (percent (getf value :percent 0.0)))
             (cond
               ((and (= count 0)
                     (= percent 0.0))
                nil)
               ((= count 0)
                `(,key (:percent ,percent)))
               ((= percent 0.0)
                `(,key (:count ,count)))
               (t
                (list key value))))))
    `(,@(when (getf plist :nr)
          (list :nr (getf plist :nr)))
      ,@(when (getf plist :text)
	  (list :text (getf plist :text)))
      ,@(when (getf plist :calls)
	  (list :calls (getf plist :calls)))
      ,@(when (getf plist :function)
          (list :function (getf plist :function)))
      ,@(clean-count-percent :self)
      ,@(clean-count-percent :total)
      ,@(clean-count-percent :cumul))))

(defun parse-sprofiler-output (profiler-text)
  "PROFILER-TEXT is the string printed by sb-sprof:report"
  (labels ((read-next (stream)
             (handler-case (list (read stream nil nil))
               (error (e)
                 (warn "Error ~S encountered while reading the string profiler-text=~S"
                       e profiler-text)
                 nil)))
           (dashes (str)
             (every (lambda (c)
                      (char= c #\-)) str))
           (collect (n stream)
             (cond
               ((zerop n)
                nil)
               (t
                (let ((data (read-next stream)))
                  (cond
                    ((null data)
                     nil)
                    (t
                     (cons (car data)
                           (collect (1- n) stream)))))))))
    (let* ((lines-str profiler-text)
           ;; dash-1 and dash-2 are lists starting with the  1st and 2nd
           ;; occurance of "-----..." in line-str after being split into a list of lines.
           (dash-1 (member-if #'dashes (split-str lines-str)))
           (dash-2 (member-if #'dashes (cdr dash-1)))
           ;; profile-lines is the list of lines between the dashes
           (profile-lines (ldiff (cdr dash-1) dash-2))
           (*package* (find-package :cl-user)))
      ;; "           Self        Total        Cumul"
      ;; "  Nr  Count     %  Count     %  Count     %    Calls  Function"
      ;; "------------------------------------------------------------------------"
      ;; "   1    121  15.6    121  15.6    121  15.6        -  LDIFF"
      ;; "   2    113  14.5    176  22.6    234  30.1        -  (LABELS TO-DNF :IN TYPE-TO-DNF)"
      ;; "   3      1  12.2      1  11.3      1  32.1        -  \"#<trampoline #<CLOSURE (SB-KERNEL::CONDITION-SLOT-READER"
      ;; "                    COMMON-LISP:SIMPLE-CONDITION-FORMAT-CONTROL) {100012C11B}> {2039012F}>\""
      ;; "   4     69   8.9    553  71.1    303  38.9        -  DISJOINT-TYPES-P"
      ;; "   5     66   8.5    199  25.6    369  47.4        -  CACHED-SUBTYPEP"
      ;; "------------------------------------------------------------------------"
      ;; "          0   0.0                                     elsewhere")
      (loop :for line :in profile-lines
            :for stream = (make-string-input-stream line)
            :for parsed = (collect 9 stream)
            :when (= 8 (length parsed)) ;; skip incomplete lines, e.g., the line between 3 and 4 above.
              :collect (prog1 (clean-sprofiling-plist
                               (list :nr (pop parsed)
				     :text line
                                     :self (list :count (pop parsed)
                                                 :percent (pop parsed))
                                     :total (list :count (pop parsed)
                                                  :percent (pop parsed))
                                     :cumul (list :count (pop parsed)
                                                  :percent (pop parsed))
				     :calls (pop parsed)
                                     :function (format nil "~A" (pop parsed))))
                         (close stream))))))

(defun call-with-sprofiling (thunk consume-prof consume-n)
  (declare (type (function () t) thunk)
           (type (function (list) t) consume-prof)
           (type (function ((and fixnum unsigned-byte)) t) consume-n))
  (labels ((recur (n-times)
             ;;(format t "recur ~D~%" n-times)
             (sb-sprof:reset)
             (funcall consume-n 0)
             (let* (thunk-ret-val
                    (val (block nil
                           (handler-bind ((warning (lambda (w &aux (filter-me "No sampling progress;"))
                                                     ;; No sampling progress; run too short, sampling interval too
                                                     ;; long, inappropriate set of sampled thread, or possibly a
                                                     ;; profiler bug.
                                                     (when (string= filter-me
                                                                    (subseq (format nil "~A" w)
                                                                            0
                                                                            (length filter-me)))
                                                       (return nil)))))
                             (sb-sprof:with-profiling (:loop nil)
                               (dotimes (n n-times thunk-ret-val)
                                 (funcall consume-n (1+ n))
                                 (setf thunk-ret-val (funcall thunk)))))))
                   (prof (parse-sprofiler-output
                          (with-output-to-string (str)
                            (let ((*standard-output* str))
                              (sb-sprof:report :type :flat))))))
               (cond
                 (prof
                  (funcall consume-prof prof)
                  val)
                 (t
                  (recur (* 2 n-times)))))))
    (let ((*debug-io* *standard-output*))
      (recur 1))))
 
(defun skip-char (stream c)
  (unless (char= c (read-char stream nil nil))
    (skip-char stream c)))

(defun clean-dprofiling-plist (plist)
  "remove key/value pairs if the value is 0 or 0.0"
  (labels ((recur (left right)
             (cond
               (right
                (destructuring-bind (key value &rest tail) right
                  (if (member value '(0 0.0))
                      (recur left tail)
                      (recur `(,value ,key ,@left) tail))))
               (t
                (nreverse left)))))
    (recur nil plist)))
                    

(defun parse-dprofiler-output (profiler-text)
  "PROFILER-TEXT is the string printed by sb-sprof:report"
  (flet ((read-next (stream)
           (handler-case (read stream nil nil)
             (error (e)
               (error "Error ~S encountered while reading the string profiler-text=~S"
                      e profiler-text))))
         (dashes (str)
           (every (lambda (c)
                    (char= c #\-)) str)))
    (let* ((lines-str profiler-text)
           ;; dash-1 and dash-2 are lists starting with the  1st and 2nd
           ;; occurance of "-----..." in line-str after being split into a list of lines.
           (dash-1 (member-if #'dashes (split-str lines-str)))
           (dash-2 (member-if #'dashes (cdr dash-1)))
           (total-line-str (cadr dash-2))
           ;; profile-lines is the list of lines between the dashes
           (profile-lines (ldiff (cdr dash-1) dash-2))
           (*package* (find-package :cl-user)))

      ;;   seconds  |     gc     |    consed   |  calls |  sec/call  |  name  
      ;; ----------------------------------------------------------
      ;;      1.314 |      0.000 | 763,854,800 | 14,718 |   0.000089 | RND-ELEMENT
      ;;      0.974 |      0.967 |           0 |     10 |   0.097396 | GARBAGE-COLLECT
      ;;      0.317 |      0.000 |     293,328 |     20 |   0.015849 | RUN-PROGRAM
      ;;      0.007 |      0.000 |     360,448 |     10 |   0.000707 | CHOOSE-RANDOMLY
      ;;      0.004 |      0.000 |           0 |     10 |   0.000397 | REPORT-HASH-LOSSAGE
      ;;      0.001 |      0.000 |           0 |  2,120 |   0.000000 | FIXED-POINT
      ;;      0.000 |      0.000 |           0 |    520 |   0.000001 | CACHED-SUBTYPEP
      ;;      0.000 |      0.000 |           0 |    520 |   0.000000 | ALPHABETIZE
      ;;      0.000 |      0.000 |           0 |    840 |   0.000000 | CMP-OBJECTS
      ;;      0.000 |      0.000 |           0 |    520 |   0.000000 | REDUCE-MEMBER-TYPE
      ;;      0.000 |      0.000 |           0 |     10 |   0.000000 | BDD-RECENT-COUNT
      ;;      0.000 |      0.000 |           0 |     10 |   0.000000 | GETTER
      ;;      0.000 |      0.000 |           0 |      1 |   0.000000 | BDD-NEW-HASH
      ;;      0.000 |      0.000 |           0 |     10 |   0.000000 | SHUFFLE-LIST
      ;;      0.000 |      0.000 |           0 |  2,120 |   0.000000 | TYPE-TO-DNF
      ;;      0.000 |      0.000 |   1,622,880 |  1,040 |   0.000000 | CACHING-CALL
      ;;      0.000 |      0.000 |           0 |     20 |   0.000000 | BDD-HASH
      ;;      0.000 |      0.000 |           0 |  3,160 |   0.000000 | ALPHABETIZE-TYPE
      ;;      0.000 |      0.000 |          16 |    520 |   0.000000 | SLOW-DISJOINT-TYPES-P
      ;;      0.000 |      0.000 |           0 |    520 |   0.000000 | DISJOINT-TYPES-P
      ;; ----------------------------------------------------------
      ;;      2.618 |      0.967 | 766,131,472 | 26,699 |            | Total

      
      (values

       ;; value-0
       (loop :for line :in profile-lines
             :for stream = (make-string-input-stream
                            (remove #\| (remove #\,  line)
                                    :count 5))
             :collect (prog1 (clean-dprofiling-plist
			      (let ((seconds (read-next stream))
				    (gc (read-next stream))
				    (cons (read-next stream))
				    (calls (read-next stream))
				    (sec/call (read-next stream))
				    (name (read-next stream)))
				(list :seconds seconds
				      :gc      gc
				      :cons    cons
				      :calls   calls
				      :sec/call sec/call
				      :package (when (and (symbolp name)
							  (symbol-package name))
						 (package-name (symbol-package name)))
				      :name    (format nil "~A" name))))
                        (close stream)))
       ;; value-1
       (let ((stream (make-string-input-stream (remove #\| (remove #\,  total-line-str)
                                                       :count 5))))
         (prog1
             (list :seconds (read-next stream)
                   :gc (read-next stream)
                   :consed (read-next stream)
                   :calls  (read-next stream))
           (close stream)))))))

(defun call-with-dprofiling (thunk packages consume-prof consume-n &key (time-thresh 0.1))
  (declare (type (function () t) thunk)
           (type list packages) ;; list of strings or symbols
           (type (function (list number) t) consume-prof)
           (type (function ((and fixnum unsigned-byte)) t) consume-n))
  (funcall consume-n 0)
  (labels ((recur (n-times)
             (sb-profile:unprofile)
             (sb-profile:reset)
             ;;(sb-profile:profile "package")
             (sb-profile::mapc-on-named-funs #'sb-profile::profile-1-fun packages)
             (let* (thunk-ret-val
                    (val (dotimes (n n-times thunk-ret-val)
                           (funcall consume-n (1+ n))
                           (setf thunk-ret-val (funcall thunk))))
                    (prof-total (multiple-value-list
                                 (parse-dprofiler-output
                                  (with-output-to-string (str)
                                    (let ((*trace-output* str))
                                      (sb-profile:report :print-no-call-list nil))))))
                    (prof (car prof-total))
                    (total (cadr prof-total)))
               (sb-profile:unprofile)
               ;; did the profiler produce any output?
               (cond
                 ((and prof
                       (or (> n-times 200)
                           (> (getf total :seconds 0) time-thresh)))
                  ;; if yes, then consume the lines
                  (funcall consume-prof prof (getf total :seconds 0))
                  val)
                 (t
                  ;; if no, then try again by running the thunk twice as many times as before.
                  (recur (* 2 n-times)))))))
    (let ((*debug-io* *standard-output*))
      (recur 1))))


(defun test-profiler ()

  (labels ((loc1 (x y)
             (let ((z 0.0))
               (dotimes (i 10000 z)
                 (setf z (* z (max (abs (sin (* x i)))
                                   (abs (cos (* y i))))))))))
    (format t "test-profiler~%")
    (let ((m 0.0))
      (do ((x 1.1 (1+ x))
           (y 2.1 (1+ y))
           (n 0 (1+ n)))
          ((> n 1000) m)
        (setf m (max m (loc1 x y)))))))

(defun test-profile ()
  (let (s-prof-plists
        d-prof-plists
        d-prof-seconds
        (n-stimes 1)
        (n-dtimes 1))
    (labels ((set-sprofile-plists (plists)
               (setf s-prof-plists plists))
             (set-n-stimes (n)
               (format t "set n-times=~D~%" n)
               (setf n-stimes n))
             (set-dprofile-plists (plists total-seconds)
               (setf d-prof-seconds total-seconds)
               (setf d-prof-plists plists))
             (set-n-dtimes (n)
               (setf n-dtimes n)))
             
      (call-with-sprofiling (lambda ()
                              (test-profiler))
                            #'set-sprofile-plists
                            #'set-n-stimes)
      
      (call-with-dprofiling (lambda ()
                              (test-profiler))
                            '("LISP-TYPES" "LISP-TYPES.TEST" "CL")
                            #'set-dprofile-plists
                            #'set-n-dtimes)
      )))


