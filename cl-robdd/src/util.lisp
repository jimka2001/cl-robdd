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

(in-package :cl-robdd)



(defun scale-sci-notations (sci-notations &key expo)
  (declare (type (or null fixnum) expo))
  (let ((max-exponent (or expo
			  (reduce #'max sci-notations :key #'cadr :initial-value (cadr (car sci-notations))))))
    (list max-exponent
	  (loop :for sci :in sci-notations
	  :collect (destructuring-bind (mant exp) sci
		     (list mant (- exp max-exponent)))))))
    
(defun sci-notation (bignum)
  (cond
    ((zerop bignum)
     (list 0.0 1))
    ((minusp bignum)
     (destructuring-bind (alpha beta) (sci-notation (- bignum))
       (list (- alpha) beta)))
    (t
     ;; bignum = alpha * 10^beta
     (let* ((log_x (log bignum 10))
            (beta (truncate (log bignum 10.0)))
            (log_alpha (- log_x beta))
            (alpha (expt 10d0 log_alpha)))
       (if (< alpha 1)
           (list (float (* 10 alpha) 1.0) (1- beta))
           (list (float alpha 1.0) beta))))))

(defun sci-notation-string (num)
  (typecase num
    (bignum (destructuring-bind (alpha beta) (sci-notation num)
              (format nil "~Ae~A" alpha beta)))
    (t (format nil "~A" num))))


