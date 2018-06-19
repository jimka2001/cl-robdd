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


(defpackage :cl-robdd-analysis
  (:use :cl :cl-robdd)
  (:export
   "MEASURE-AND-WRITE-BDD-DISTRIBUTION"
   "RANDOM-BOOLEAN-COMBINATION"
   "CALL-WITH-TIMEOUT"
   "QSTAT-F"
   "CALL-WITH-SPROFILING"
   "CALL-WITH-DPROFILING"))

(in-package :cl-robdd-analysis)

(defun compare-hash-strength (num-tests vars)
  (loop :for *bdd-hash-strength*
          :in '(:weak :strong :weak-dynamic :strong-dynamic)
        :do (format t "~A~%" *bdd-hash-strength*)
        :do (time (bdd-ops-test num-tests vars))))

(defun bdd-ops-test (num-tests vars)
  (garbage-collect)
  (bdd-with-new-hash ()
    (dotimes (_ num-tests)
      (bdd-with-new-hash ()
        (let* ((a (bdd (random-boolean-combination vars)))
               (b (bdd (random-boolean-combination vars)))
               (c (bdd-or a b))
               (d (bdd-and a b))
               (e (bdd-and-not a b))
               (f (bdd-and-not b a))
               (bdds (list c d e f)))
          (dolist (x bdds)
            (dolist (y bdds)
              (bdd-and x y)
              (bdd-or x y)
              (bdd-and-not x y)
              (bdd-and-not y x))))))
    (format t "~A~%" (bdd-hash))))

(defun qstat-f ()
  "call qstat -f and write the output to a file with the .sXXXX extension
similar to where current Output_Path is indicating."
  (let ((pbs-jobid (sb-posix:getenv "PBS_JOBID"))
        (delimeter (format nil "~%~a" '#\Tab )))
    ;; call qstat -f and get the output;
    ;; qstat -f wraps long lines, so we first have to unwrap them by finding
    ;; "\n\t" and replacing with ""
    (let* ((qstat-out (replace-all (with-output-to-string (str)
                                    (sb-ext:run-program "qstat" (list "-f" pbs-jobid)
                                                        :search t
                                                        :output str))
                                   delimeter ""))
           ;; we want to write into file.sXXXXX, so we have to
           ;; find the Output_Path = /path/to/file.oXXXXXX and parse that
           ;; to get the desired name of the new status output file.
           (pos-host (search "Output_Path = " qstat-out))
           (pos-path (search ":" qstat-out :start2 pos-host))
           (pos-eol  (search (format nil "~%") qstat-out :start2 pos-host))
           ;; output-path = "/path/to/file.oXXXXXX"
           (output-path (subseq qstat-out (1+ pos-path) pos-eol))
           (pos-extension (1+ (search "." output-path :from-end t)))
           ;; extension = "XXXXXX" ; excluding the ".o"
           (extension (subseq output-path (1+ pos-extension)))
           ;; leading = "/path/to/file." ;; including the "."
           (leading   (subseq output-path 0 pos-extension))
           ;; status-filename = "/path/to/file.sXXXXXX"
           (status-filename (concatenate 'string leading "s" extension)))
      
      (with-open-file (stream status-filename
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (format stream "~%")
        (write-line qstat-out stream)))))
