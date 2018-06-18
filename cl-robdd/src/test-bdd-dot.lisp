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

(in-package :cl-robdd-test)

(let ((package-into (find-package  :cl-robdd-test))
      (package-from (find-package  :cl-robdd))
      (*package* (find-package :keyword)))
  (do-symbols (name package-from)
    (when (and (eq package-from (symbol-package name))
               (not (find-symbol (symbol-name name) package-into)))
      (format t "importing name=~A into ~S ~%" name package-into)
      (shadowing-import name package-into))))

(define-test bdd-to-png
  (bdd-to-png (bdd t))
  (bdd-to-png (bdd nil))
  (bdd-to-png (bdd '(and x1 (or x2 (not x3))))))


(define-test bdd-to-dot
  (bdd-to-png (bdd t))
  (bdd-to-png (bdd nil))
  (bdd-to-png (bdd '(and x1 (or x2 (not x3))))))


