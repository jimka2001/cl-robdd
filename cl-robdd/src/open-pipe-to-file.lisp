
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
 (:use :cl )
 (:export "OPEN-PIPE-TO-FILE"
          "WITH-OPEN-PIPE-TO-FILE"))

(in-package :open-pipe-to-file)

(defun open-pipe-to-file (writable-stream commands)
  "Returns two values (new-stream clean-up-thunk),
NEW-STREAM is the stream which the calling function may write to, to effectively write into the pipe chain.
EOF-THUNK is a 0-ary function which the calling function must call to close the pipes and assure the
   data gets written into into WRITABLE-STREAM"
  (labels ((recure (p-in commands cleanup-processes )
             (if commands
                 (let ((process (uiop:launch-program (car commands)
                                                     :input :stream
                                                     :output p-in
                                                     :error-output *error-output*)))
                   (format t "started ~A connected to ~A~%" (car commands) p-in)
                   (recure (uiop:process-info-input process)
                           (cdr commands)
                           (cons (list process (car commands)) cleanup-processes)))
                 (values p-in (lambda ()
                                (dolist (process cleanup-processes)
                                  (format t "closing  ~A~%" (cadr process))
                                  (close (uiop:process-info-input (car process)))
                                  ;;(format t "waiting on ~A~%" (cadr process))
                                  ;;(uiop:wait-process (car process))
                                  ))))))

    (recure writable-stream (reverse commands) nil)))

(defmacro with-open-pipe-to-file ((stream-pipe-input stream-pipe-output commands) &body body)
  (let ((eof (gensym "eof")))
    `(multiple-value-bind (,stream-pipe-input ,eof)
         (open-pipe-to-file ,stream-pipe-output ,commands)
       (unwind-protect
           (progn ,@ body)
         (funcall ,eof)))))
