(in-package :cl-user)

(let ((x 100)
      (y 200))
  (print (list x y)))

(defun factorial (n)
  (if (plusp n)
      (* n (factorial (- n 1)))
      1))


(member-if (lambda (x) (> x 6)) '(1 2 4 3 6 5 7))
(defmacro exists (var some-list expr)
  `(member-if (lambda (,var) ,expr) ,some-list))

(exists x (list 1 2 4 3 6 5 7)
  (> x 6))

(defmacro forall (var some-list expr)
  `(not (exists ,var ,some-list (not ,expr))))

(forall x (list 1 2 4 3 6 5 7)
  (> x 6))


(defun forall-fun (predicate list)
  (not (member-if (lambda (x) (not (funcall predicate x)))
                  list)))
