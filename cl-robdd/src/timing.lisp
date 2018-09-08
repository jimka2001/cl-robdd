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

(defvar *profile-functions* '(cl:subtypep))

(defun best-time (num-tries thunk
                  &key profile
                    (profile-packages (remove-if-not #'find-package '("CL-ROBDD" "CL-ROBDD-TEST" "CL-ROBDD-ANALYSIS-TEST" "CL-ROBDD-ANALYSIS"
								      ;; "FR.EPITA.LRDE.SUBTYPEP"
                                                                      "LISP-TYPES" "LISP-TYPES.TEST")))
                    (sprofile-plists nil)
                    (dprofile-plists nil)
                    (dprofile-total-seconds 0)
                    (set-sprofile-plists (lambda (plists)
                                           (setf sprofile-plists plists)))
                    (set-dprofile-plists (lambda (plists total-seconds)
                                           (setf dprofile-total-seconds total-seconds)
                                           (setf dprofile-plists plists)))                    
                    (n-stimes 1)
                    (set-n-stimes (lambda (n)
                                    (setf n-stimes n)))
                    (get-n-stimes (lambda ()
                                    n-stimes))
                    (n-dtimes 1)
                    (set-n-dtimes (lambda (n)
                                    (setf n-dtimes n)))
                    (get-n-dtimes (lambda ()
                                    n-dtimes))
                    (get-profile-plists (lambda ()
                                          (list :n-stimes (funcall get-n-stimes)
                                                :n-dtimes (funcall get-n-dtimes)
                                                :dprofile-total dprofile-total-seconds
                                                :dprof dprofile-plists
                                                :sprof sprofile-plists))))
  "returns a plist with the fields :wall-time :run-time :value"
  (declare (type (and fixnum unsigned-byte) num-tries)
           (type (function () t) thunk))
  (let (result
	(profile (case profile
		   ((nil) nil)
		   ((t) '(:dprof :sprof))
		   (t profile))))
    (dotimes (try num-tries)
      (funcall set-n-stimes 1)
      (funcall set-n-dtimes 1)
      (let* ((run-time-t1 (get-internal-run-time))
             (start-real-time (get-internal-real-time))
             (s2 (if (member :sprof profile)
                     (call-with-sprofiling thunk
                                           set-sprofile-plists
                                           set-n-stimes)
                     (funcall thunk)))
             (run-time-t2 (get-internal-run-time))
             (wall-time (/ (- (get-internal-real-time) start-real-time) internal-time-units-per-second
                           (1+ (funcall get-n-stimes))))
             (run-time  (/ (- run-time-t2 run-time-t1) internal-time-units-per-second
                           (1+ (funcall get-n-stimes)))))
        (when (member :dprof profile)
          (call-with-dprofiling thunk
                                (append profile-packages *profile-functions)
                                set-dprofile-plists
                                set-n-dtimes))
        (setf result
              (cond
                ((not result) ; if first time through dotimes loop
                 (list :wall-time (the real wall-time)
                       :run-time run-time
                       :value s2
                       :profile-plists (funcall get-profile-plists)))
                ((< run-time (the real (getf result :run-time #+sbcl 0)))
                 (list :wall-time wall-time
                       :run-time run-time
                       :value s2
                       :profile-plists (funcall get-profile-plists)))
                (t
                 result)))))
    result))


(defun call-with-timeout (time-out thunk num-tries &key profile)
  "TIME-OUT, integer, the wall-time allowed to call the function THUNK.
 THUNK is a 0-ary function returning some type X.
 Call the function THUNK, in one thread, and start a 2nd observer thread.  The 2nd thread
 is responsible for monitoring the wall time and killing the 1st thread if the TIME-OUT has
 passed.  The function will be called num-tries times, and the minimum time is the
 value reported in the returned plist.
 If TIME-OUT is nil, then the function is not run in a separate thread, but rather 
 is run directly.
 Returns a plist, one of the following:
 (:wall-time rational :run-time rational :time-out integer) or
 (:wall-time rational :run-time rational :value X :profile-plists list-of-plists)"
  (declare (type (or null (and fixnum unsigned-byte)) time-out)
           (type (function () t) thunk)
           (type (and fixnum unsigned-byte) num-tries))
  (let (sprofile-plists
        dprofile-plists
        (dprofile-total-seconds 0)
        (n-dtimes 1)
        (n-stimes 1))
    (labels ((get-profile-plists ()
               (list :n-stimes (get-n-stimes)
                     :n-dtimes (get-n-dtimes)
                     :dprofile-total dprofile-total-seconds
                     :dprof dprofile-plists
                     :sprof sprofile-plists))
             (set-sprofile-plists (plists)
               (setf sprofile-plists plists))
             (set-dprofile-plists (plists total-seconds)
               (setf dprofile-total-seconds total-seconds)
               (setf dprofile-plists plists))
             (get-n-stimes ()
               n-stimes)
             (set-n-stimes (n)
               (setf n-stimes n))
             (get-n-dtimes ()
               n-dtimes)
             (set-n-dtimes (n)
               (setf n-dtimes n)))
      (if (not time-out)
          (best-time num-tries thunk :profile profile
                                     :get-profile-plists #'get-profile-plists
                                     :set-sprofile-plists #'set-sprofile-plists
                                     :set-dprofile-plists #'set-dprofile-plists
                                     :get-n-stimes #'get-n-stimes
                                     :get-n-dtimes #'get-n-dtimes
                                     :set-n-stimes #'set-n-stimes
                                     :set-n-dtimes #'set-n-dtimes)
          (%call-with-timeout time-out thunk num-tries :profile profile
                                                       :get-profile-plists #'get-profile-plists
                                                       :set-sprofile-plists #'set-sprofile-plists
                                                       :set-dprofile-plists #'set-dprofile-plists
                                                       :get-n-stimes #'get-n-stimes
                                                       :get-n-dtimes #'get-n-dtimes
                                                       :set-n-stimes #'set-n-stimes
                                                       :set-n-dtimes #'set-n-dtimes)))))

#+allegro
(defun %call-with-timeout (time-out thunk num-tries &key profile)
  (declare (type (and fixnum unsigned-byte) time-out num-tries)
           (type (function () t) thunk)
           (ignore profile))
  (let ((start-run-time (get-internal-run-time))
        (start-real-time (get-internal-real-time)))
    (sys:with-timeout (time-out (let ((run-time (get-internal-run-time))
                                      (real-time (get-internal-real-time)))
                                  (list :wall-time (/ (- real-time start-real-time) internal-time-units-per-second)
                                        :run-time  (/ (- run-time start-run-time) internal-time-units-per-second)
                                        :time-out time-out)))
      (best-time num-tries thunk :profile nil))))

#+sbcl
(defun %call-with-timeout (time-out thunk num-tries &rest profiler-args &key get-profile-plists &allow-other-keys)
  (declare (type (and fixnum unsigned-byte) time-out num-tries)
           (type (function () t) thunk))
  (let ((start-run-time (get-internal-run-time))
        (start-real-time (get-internal-real-time)))
    (handler-bind ((sb-ext:timeout (lambda (c)
                                     (declare (ignore c))
                                     (let ((run-time (get-internal-run-time))
                                           (real-time (get-internal-real-time)))
                                       (return-from %call-with-timeout
                                         (list :wall-time (/ (- real-time start-real-time) internal-time-units-per-second)
                                               :run-time  (/ (- run-time start-run-time) internal-time-units-per-second)
                                               :profile-plists (funcall get-profile-plists)
                                               :time-out time-out))))))
      (sb-ext:with-timeout time-out
        (apply #'best-time num-tries thunk profiler-args)))))

