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


(in-package :cl-robdd-analysis-test)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow-all-symbols :package-from :cl-robdd-analysis
		      :package-into :cl-robdd-analysis-test))


(defun profile/test1 () ; not a unit test because it takes far to long to run to be considered a unit test
  (flet ((test-function ()
           (let (nums)
             (dotimes (a 10)
               (dotimes (b 10)
                 (pushnew (random 1.0) nums)))
             (list (sort nums #'<)
		   (sort nums #'>)
		   (sort nums #'>=)
		   (sort nums #'<=)))))
    (let (profiling )
      (call-with-sprofiling (lambda ()
                              (test-function))
                            (lambda (plists)
                              (setf profiling plists))
                            (lambda (n)
                              (assert-true (typep n 'fixnum))))
      (forall item profiling
        (assert-true (numberp (getf item :nr))))
      (forall item profiling
        (assert-true (consp (getf item :total))))
      (forall item profiling
        (assert-true (consp (getf item :cumul))))
      (forall item profiling
        (assert-true (getf item :function))))))
      

(defvar *profile/2-text*
"
Number of samples:   1
Sample interval:     0.01 seconds
Total sampling time: 0.01 seconds
Number of cycles:    0
Sampled threads:
 #<SB-THREAD:THREAD \"main thread\" RUNNING {1001970083}>

           Self        Total        Cumul
  Nr  Count     %  Count     %  Count     %    Calls  Function
------------------------------------------------------------------------
   1      1 100.0      1 100.0      1 100.0        -  \"#<trampoline #<CLOSURE (SB-KERNEL::CONDITION-SLOT-READER
                        COMMON-LISP:SIMPLE-CONDITION-FORMAT-CONTROL) {100012C11B}> {2039012F}>\"
   2      0   0.0      1 100.0      1 100.0        -  (COMMON-LISP:LABELS SB-IMPL::HANDLE-IT :IN SB-KERNEL:OUTPUT-OBJECT)
   3      0   0.0      1 100.0      1 100.0        -  COMMON-LISP:PRINC
   4      0   0.0      1 100.0      1 100.0        -  (COMMON-LISP:FLET \"H0\" :IN CL-ROBDD-ANALYSIS:CALL-WITH-SPROFILING)
   5      0   0.0      1 100.0      1 100.0        -  SB-KERNEL::%SIGNAL
   6      0   0.0      1 100.0      1 100.0        -  (COMMON-LISP:FLET SB-KERNEL::%WARN :IN \"SYS:SRC;CODE;WARM-ERROR.LISP\")
   7      0   0.0      1 100.0      1 100.0        -  CL-ROBDD-ANALYSIS:CALL-WITH-SPROFILING
   8      0   0.0      1 100.0      1 100.0        -  CL-ROBDD-ANALYSIS::BEST-TIME
   9      0   0.0      1 100.0      1 100.0        -  CL-ROBDD-ANALYSIS::%CALL-WITH-TIMEOUT
  10      0   0.0      1 100.0      1 100.0        -  LISP-TYPES-ANALYSIS:TYPES/CMP-PERF
  11      0   0.0      1 100.0      1 100.0        -  (COMMON-LISP:LAMBDA COMMON-LISP:NIL :IN LISP-TYPES-ANALYSIS:TYPES/CMP-PERFS)
  12      0   0.0      1 100.0      1 100.0        -  LISP-TYPES-ANALYSIS:TYPES/CMP-PERFS
  13      0   0.0      1 100.0      1 100.0        -  (COMMON-LISP:LAMBDA COMMON-LISP:NIL :IN LISP-TYPES-ANALYSIS::BIG-TEST-REPORT)
  14      0   0.0      1 100.0      1 100.0        -  CL-ROBDD:BDD-CALL-WITH-NEW-HASH
  15      0   0.0      1 100.0      1 100.0        -  SB-EXT:CALL-WITH-TIMING
  16      0   0.0      1 100.0      1 100.0        -  (COMMON-LISP:LAMBDA COMMON-LISP:NIL :IN \"/var/spool/torque6/mom_priv/jobs/451946.master.lrde.epita.fr.SC\")
  17      0   0.0      1 100.0      1 100.0        -  SB-INT:SIMPLE-EVAL-IN-LEXENV
  18      0   0.0      1 100.0      1 100.0        -  SB-EXT:EVAL-TLF
  19      0   0.0      1 100.0      1 100.0        -  (COMMON-LISP:LABELS SB-FASL::EVAL-FORM :IN SB-INT:LOAD-AS-SOURCE)
  20      0   0.0      1 100.0      1 100.0        -  (COMMON-LISP:LAMBDA (SB-KERNEL:FORM COMMON-LISP:&KEY :CURRENT-INDEX COMMON-LISP:&ALLOW-OTHER-KEYS) :IN SB-INT:LOAD-AS-SOURCE)
  21      0   0.0      1 100.0      1 100.0        -  SB-C::%DO-FORMS-FROM-INFO
  22      0   0.0      1 100.0      1 100.0        -  SB-INT:LOAD-AS-SOURCE
  23      0   0.0      1 100.0      1 100.0        -  (COMMON-LISP:FLET SB-FASL::THUNK :IN COMMON-LISP:LOAD)
  24      0   0.0      1 100.0      1 100.0        -  SB-FASL::CALL-WITH-LOAD-BINDINGS
  25      0   0.0      1 100.0      1 100.0        -  (COMMON-LISP:FLET SB-FASL::LOAD-STREAM :IN COMMON-LISP:LOAD)
  26      0   0.0      1 100.0      1 100.0        -  COMMON-LISP:LOAD
  27      0   0.0      1 100.0      1 100.0        -  (COMMON-LISP:FLET SB-IMPL::LOAD-SCRIPT :IN SB-IMPL::PROCESS-SCRIPT)
  28      0   0.0      1 100.0      1 100.0        -  (COMMON-LISP:FLET SB-UNIX::BODY :IN SB-IMPL::PROCESS-SCRIPT)
  29      0   0.0      1 100.0      1 100.0        -  (COMMON-LISP:FLET \"WITHOUT-INTERRUPTS-BODY-3\" :IN SB-IMPL::PROCESS-SCRIPT)
  30      0   0.0      1 100.0      1 100.0        -  SB-IMPL::PROCESS-SCRIPT
  31      0   0.0      1 100.0      1 100.0        -  SB-IMPL::TOPLEVEL-INIT
  32      0   0.0      1 100.0      1 100.0        -  (COMMON-LISP:FLET SB-UNIX::BODY :IN SB-EXT:SAVE-LISP-AND-DIE)
  33      0   0.0      1 100.0      1 100.0        -  (COMMON-LISP:FLET \"WITHOUT-INTERRUPTS-BODY-27\" :IN SB-EXT:SAVE-LISP-AND-DIE)
  34      0   0.0      1 100.0      1 100.0        -  (COMMON-LISP:LABELS SB-IMPL::RESTART-LISP :IN SB-EXT:SAVE-LISP-AND-DIE)
------------------------------------------------------------------------
          0   0.0                                     elsewhere
")


(define-test profile/test2
  (parse-sprofiler-output *profile/2-text*))
