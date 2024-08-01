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


(defun display-theta (n theta)
  (flet ((e2 (theta)
           (- (expt 2 (expt 2 theta))
              (expt 2 (expt 2 (1- theta)))))
         (e1 (theta)
           (expt 2 (- n theta 1)))
         (pr (n fn)
           (format t " ~A=" fn)
           (if (< (log n) 10)
               (format t "~D" n)
               (format t "2^~A" (log n 2)))))
    (loop for i from (1- theta) to (1+ theta)
          do (format t "n=~D log_2(~D)=~D theta=~D" n n (floor (log n 2)) i)
          do (pr (e2 i) "e2")
          do (format t " ~A"
                     (cond
                       ((< (e2 i) (e1 i))
                        "<")
                       ((= (e2 i) (e1 i))
                        "=")
                       (t ">")))
          do (pr (e1 i) "e1")
          do (terpri))))


(defun delta (i n)
  (labels ((e2 (i)
             (- (expt 2 (expt 2 (1+ (- n i))))
                (expt 2 (expt 2 (- n i)))))
           (e1 (i)
             (expt 2 (1- i)))
           (delta-bar (i)
             (- (e2 i) (e1 i))))
    (format t "     n=~A e1=~A   e2=~A~%" n (e1 (- n 1)) (e2 (- n 1)))
    (delta-bar (- n i))))

(defun delta-print-table (n)
  (let ((d -1)
        (i 0))

    (loop :while  (< d 0)
          :do (setf d (delta i n))
          :do (format t "i=~A  delta= ~A~%" i d)
          :do (incf i)
          :finally (format t "n = ~A theta=~A~%" n (1- i)))
    ))


(defun theta-bounds (n)
  (list
   (if (< (log n 2) (1- n))
       (ceiling (log (- n (log n 2) 1) 2))
       0)
   (floor (log n 2))))




(defun 2^ (n) (expt 2 n))
(defun 2^^ (n) (2^ (2^ n)))

(defun power-diff (n)
  (- (2^^ n)
     (2^^ (1- n))))

(defun cmp-power-diff (n)
  (let ((theta (floor (log n 2))))
    (- (power-diff theta)
       (expt 2 (- n theta 1)))))

(defun row-r (n i)
  (declare (ignore n))
  (2^ (1- i)))

(assert (= 2 (row-r 3 2)) ())
(assert (= 4 (row-r 3 3)) ())
(assert (= 1 (row-r 2 1)) ())



(defun row-RR (n i)
  (- (2^^ (+ n (- i) 1))
     (2^^ (- n i))))

(assert (= 12 (row-RR 3 2)) ())
(assert (= 2 (row-RR 3 3)) ())
(assert (= 12 (row-RR 2 1)) ())
(assert (= 2 (row-RR 2 2)) ())

(defun log2 (n)
  (log n 2))

(defun theta (n)
  (let ((max (floor (log2 n))))
    ;;(format t "log=~A~%" max)
    (case max
      ((0)
       (values 0 0))
      (t
       (loop :for theta :downfrom max :downto 0
             :for r = (row-r n (- n theta))
             :for RR = (row-RR n (- n theta))
             :for iterations = 1 :then (1+ iterations)
             ;; :do (format t "n=~A theta=~A n-theta=~A r<=R?  ~A ~A ~A?~%"
             ;;             n theta (- n theta) r
             ;;             (cond ((< r RR) "<")
             ;;                   ((= r RR) "=")
             ;;                   (t ">"))
             ;;             RR)
             :do (when (>= r RR)
                   (return-from theta (values (1+ theta) iterations))))))))

(defun find-theta (limit)
  (let ((hash-iterations (make-hash-table)))
    (loop for n from 2 to limit
          do (multiple-value-bind (theta iterations) (theta n)
               (declare (ignore theta))
               (incf (gethash iterations hash-iterations 0))))
    (maphash (lambda (key value)
               (format t "iterations=~D occurances=~D~%" key value))
             hash-iterations)))

(defun estim (n nterms)
  (labels ((rec (l i accum)
             (if (zerop i)
                 accum
                 (rec (log (- n l) 2) (1- i) (cons l accum)))))
    (let ((seq (nreverse (rec (log n 2) nterms nil))))
      (maplist (lambda (tail)
                 (if (cdr tail)
                     (destructuring-bind (a b &rest ignored) tail
                       (declare (ignore ignored))
                       (list a (cond ((< a b) 1)
                                     ((= a b) 0)
                                     (t -1)) (- a b)))
                     (car tail)))
               seq))))

(defun robdd-size (n)
  (let* ((theta (theta n))
         (robdd-size (+ (1- (2^ (- n theta)))
                        (2^^ theta)))
         (bdd-size (1- (2^ (1+ n)))))
    (list :n n :theta theta :robdd robdd-size :bdd bdd-size :compression (float (/ (+ 1d0 robdd-size) bdd-size) 1.0))))

(defun log-based-compression (min max)
  (loop :for n :from min :to max
        :collect (list n (let ((theta (float (log n 2))))
                           (/ (+ (expt 2d0 (expt 2d0 theta)) (expt 2d0 (- n theta)) -1)
                              (1- (expt 2d0 (1+ (float n) ))))))))
  
(defun robdd-compressions (min max)
  (loop :for n :from min :to max
        :collect (destructuring-bind (&key compression &allow-other-keys) (robdd-size n)
                   (list n compression))))

(defun theta-latex-table (n-min n-max) ;; normally 1 21
  (format t "$~A$ & $~A$ & $~A$ & $~A$ & $~A$ & $~A$ & $~A$\\\\~%"
          "\\numvars"
          "\\lfloor \\log_2\\numvars \\rfloor"
          "\\theta"
          "2^{\\numvars -\\theta}-1"
          "2^{2^\\theta}"
          "|ROBDD_{\\numvars}|"
          "\\frac{|ROBDD_{\\numvars}|}{|UOBDD_{\\numvars}|}"
          )
  (loop :for n :from n-min :to n-max
        :do (let* ((theta (theta n))
                   (theta-max (floor (log2 n)))
                   (|ROBDD_n| (+ (1- (2^ (- n theta)))
                                 (2^ (2^ theta))))
                   (|UOBDD_n| (1- (2^ (1+ n))))
                   (compression (/ (float |ROBDD_n|)
                                   |UOBDD_n|)))
              (format t "~A & ~A & ~A & ~A & ~A & ~A & ~6,3f\\%\\\\~%"
                      n
                      theta-max
                      theta
                      (1- (2^ (- n theta))) 
                     (2^ (2^ theta))
                      |ROBDD_n|
                      (* 100 compression)))))

(defun compression (n)
  (let ((theta (theta n)))
    (format t "theta = ~A~%" theta)
    (format t "(2^^~A + 2^(~A - ~A) - 1) / (2^~A - 1)~%" theta n theta (1+ n))
    (format t "= (2^~A + 2^(~A - ~A) - 1) / (2^~A - 1)~%" (2^ theta) n theta (1+ n))
    (format t " = (~A + ~A - 1) / (~A - 1)~%" (2^^ theta) (2^ (- n theta)) (2^ (1+ n)))
    (format t " = ~A / ~A~%" (+ (2^^ theta) (2^ (- n theta)) -1) (1- (2^ (1+ n))))
    (format t " = ~A~%" (/ (+ (2^^ (float theta)) (2^ (- n (float theta))) -1) (1- (2^ (1+ (float n))))))))

(defun graph-theta (n-min n-max)
  (loop :for n :from n-min :to n-max
        :for theta = (theta n)
        :do (format t "(~A, ~A)~%" n theta))
  (format t "~%")
  (loop :for n :from n-min :to n-max
        :do (format t "(~A, ~A)~%" n (log2 n)))
  (format t "~%")

  (loop :for n :from n-min :to n-max
        :do (format t "(~A, ~A)~%" n (- (log2 (- n 2 (log2 n))) 2))))
