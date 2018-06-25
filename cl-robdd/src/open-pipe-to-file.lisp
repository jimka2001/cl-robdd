
;; From: Philipp Marek <philipp@marek.priv.at>
;; Subject: Re: [Sbcl-devel] help with unix pipelining
;; Date: 25 June 2018 at 11:54:27 GMT+2
;; To: jnewton@lrde.epita.fr
;; Cc: SBCL Devel-list <sbcl-devel@lists.sourceforge.net>



;; (with-open-pipe-to-file (stream #p"/tmp/result"
;;                                          '(("grep" "-i" "a")
;;                                            ("sort")))
;;  (loop for i from 0 to #xff
;;        do (format stream "0x~x~%" i)))


(defpackage :open-pipe-to-file
 (:use :cl :iterate :alexandria)
 (:export "OPEN-PIPE-TO-FILE"
          "WITH-OPEN-PIPE-TO-FILE"))

(in-package :open-pipe-to-file)

(defun open-pipe-to-file (filename commands)
  "..."
  (iterate (with processes = ())
      (for output-spec initially filename
                then last-input)
      (for command in (reverse commands))
      (for process = (funcall #'uiop:launch-program
                              command
                              :input :stream
                              :if-output-exists :supersede
                              :output output-spec
                              :error-output *error-output*))
      ;; we need the last-started process first in the list
      (push process processes)
      (for last-input = (uiop:process-info-input process))
      (finally
        (return (values last-input processes)))))


(defmacro with-open-pipe-to-file ((stream filename commands) &body body)
  (with-gensyms (processes process)
    `(multiple-value-bind (,stream ,processes)
         (open-pipe-to-file ,filename ,commands)
       (unwind-protect
           (progn ,@ body)
         (iterate (for ,process in ,processes)
             (close (uiop:process-info-input ,process))
             ;; Last process writes into a file
             (if (uiop:process-info-output ,process)
               (close (uiop:process-info-output ,process))))))))
