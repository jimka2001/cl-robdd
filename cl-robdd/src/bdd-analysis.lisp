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
   "CALL-WITH-DPROFILING"
   "CALL-WITH-SPROFILING"
   "CALL-WITH-TIMEOUT"
   "DEMAND-ENV-VAR"
   "MEASURE-AND-WRITE-BDD-DISTRIBUTION"
   "QSTAT-F"
   "RANDOM-BOOLEAN-COMBINATION"
   "WITH-DUP-STREAM"
))

(in-package :cl-robdd-analysis)

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
            while pos)))

(defun demand-env-var (env-var-name)
  (or (sb-posix:getenv env-var-name)
      (error "Missing env var ~s" env-var-name)))

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

(defmacro with-dup-stream ((stream file) &body body)
  "Rebind the named STREAM to a broadcast-stream which duplicates it to the named FILE, evaluating the BODY within the dynamic extend of the rebinding."
  (let ((log-file (gensym)))
    `(with-open-file (,log-file ,file
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
       (let ((,stream (make-broadcast-stream ,stream ,log-file)))
         ,@body))))

