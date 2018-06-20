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

(defun shadow-all-symbols (&key package-from package-into)
  (let ((package-into (or (find-package  package-into)
                          (error "cannot find package ~A" package-into)))
        (package-from (or (find-package  package-from)
                          (error "cannot find package ~A" package-from)))
        (*package* (find-package :keyword)))
    (let (symbols)
      (do-symbols (symbol package-from)
        (pushnew symbol symbols))
      (dolist (symbol (sort symbols #'string< ))
        (when (and (eq package-from (symbol-package symbol))
                   (or (not (find-symbol (symbol-name symbol) package-into))
                       (not (eq (find-symbol (symbol-name symbol) package-from)
                                (find-symbol (symbol-name symbol) package-into)))))
          ;;(format t "importing name=~S into ~S " symbol package-into)
          (shadowing-import symbol package-into)
          ;;(unless (equal '(:internal) (cdr (multiple-value-list (find-symbol (symbol-name symbol) package-into))))
          ;; (format t "~S~%" (cdr (multiple-value-list (find-symbol (symbol-name symbol) package-into)))))
          ;;(format t "~%")
          )))
    (let (names)
      (do-symbols (symbol package-from)
        (pushnew (symbol-name symbol) names :test #'string=))
      (do-symbols (symbol package-into)
        (pushnew (symbol-name symbol) names :test #'string=))
      (setf names (sort names #'string<))
      (setf names (remove-if-not (lambda (name)
                                   (and (find-symbol name package-into)
                                        (find-symbol name package-from)
                                        (not (eq (find-symbol name package-into)
                                                 (find-symbol name package-from)))))
                                 names))
      (when names
        (error "The following distinct symbols exist in both packages: ~A" names)))))

(defun lconc (buf items)
  (cond
    ((null buf)
     (cons items (last items)))
    ((null (car buf))
     (setf (car buf) items)
     (setf (cdr buf) (last items))
     buf)
    ((null items)
     buf)
    (t
     (setf (cdr (cdr buf)) items)
     (setf (cdr buf) (last items))
     buf)))

(defun tconc (buf &rest items)
  (lconc buf items))

(defmacro while (test &body body)
  `(loop :while ,test
	 :do (progn ,@body)))

(defmacro setof (var data &body body)
  `(remove-if-not (lambda (,var) ,@body) ,data))

(defun getter (field)
  (lambda (obj) (getf obj field)))

(defun run-program (program args &rest options)
  #+sbcl (apply #'sb-ext:run-program program args :search t options)
  #+allegro (apply #'excl:run-shell-command
                   (apply #'vector (cons program args))
                   :wait t
                   options
                   )
  )


(defvar *tmp-dir* (format nil "/tmp/~A/" (or (sb-posix:getenv "USER")
                                             "unknown-user")))

(defun make-temp-dir (suffix)
  (format nil "~A/~A/" *tmp-dir* suffix))

(defun boolean-expr-to-latex (expr &optional (stream t))
  (etypecase expr
    ((eql nil)
     (format stream "\\bot"))
    ((eql t)
     (format stream "\\top"))
    ((not list)
     (format stream "~A" expr))
    ((cons (eql and))
          (format stream "(")
     (boolean-expr-to-latex (cadr expr) stream)
     (dolist (subexpr (cddr expr))
       (format stream " \\wedge ")
       (boolean-expr-to-latex subexpr stream))
     (format stream ")")
     )
    ((cons (eql or))
     (format stream "(")
     (boolean-expr-to-latex (cadr expr) stream)
     (dolist (subexpr (cddr expr))
       (format stream " \\vee ")
       (boolean-expr-to-latex subexpr stream))
     (format stream ")")
     )
    ((cons (eql not))
     (format stream "\\neg ")
     (boolean-expr-to-latex (cadr expr) stream))))


(defun garbage-collect ()
  #+sbcl (sb-ext::gc :full t)
  #+allegro (excl:gc t)
)
