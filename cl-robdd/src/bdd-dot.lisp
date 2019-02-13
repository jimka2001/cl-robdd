;; Copyright (c) 2017,2018 EPITA Research and Development Laboratory
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

(in-package   :cl-robdd)

(defun bdd-to-dot (bdd stream &key (reduced t) (pen-width 2) (draw-false-leaf t))
  "Create a graphviz dot file representing the given BDD.
STREAM may be given as t (for *standard-output*),
or a stream to print to, 
or nil (to print to output string)
or STRING indicating name of file to write to.
:DRAW-FALSE-LEAVE nil may be used to simplify the display of the bdd, by omitting all
paths to the false leaf."
  (typecase stream
    (null
     (with-output-to-string (str)
       (bdd-to-dot bdd str :reduced reduced)))
    (string
     (with-open-file (output stream :direction :output :if-exists :supersede :if-does-not-exist :create)
       (bdd-to-dot bdd output :reduced reduced)))
    ((or stream (eql t))
     ;; header
     (format stream "digraph G {~%")
     
     (labels ((draw-node-p (bdd)
                (typecase bdd
                  ((or bdd-node bdd-true)
                   t)
                  (bdd-false
                   draw-false-leaf)))
              (dot-node (bdd node-num)
                (typecase bdd
                  (bdd-node
                   (format stream "~D [shape=~A,label=~S]~%"
                           node-num
                           "ellipse"
			   (bdd-label bdd)))
                  (bdd-true
                   (format stream "~D [shape=~A,label=~S,fontname=~S]~%"
                           node-num
                           "box"
			   (bdd-label bdd)
                           "sans-serif"))
                  (bdd-false
                   (when draw-false-leaf
                     (format stream "~D [shape=~A,label=~S]~%"
                             node-num
                             "box"
			     "&perp;"
                             ))))))
       (cond
         (reduced
          (let* ((num 0)
                 (buf (tconc nil (list :bdd bdd :node-num (incf num))))
                 labels
                 (nodes (car buf)))
            ;; BFS: first print the node delcarations and remember the node list, and remember the labels
            (while nodes
              (destructuring-bind (&key node-num bdd) (car nodes)
                (pushnew (bdd-label bdd) labels :test #'equal)
                (dot-node bdd node-num)
                (typecase bdd
                  (bdd-node
                   (unless (find (bdd-positive bdd) (car buf) :key (getter :bdd))
                     (tconc buf (list :bdd (bdd-positive bdd)  :node-num (incf num))))
                   (unless (find (bdd-negative bdd) (car buf) :key (getter :bdd))
                     (tconc buf (list :bdd (bdd-negative bdd) :node-num (incf num)))))))
              (pop nodes))
            ;; now print the rank=same lines
            (dolist (label labels)
              (let ((common-labels (setof node (car buf)
                                     (equal label (bdd-label (getf node :bdd))))))
                (when (cdr common-labels)
                  (format stream "{rank=same")
                  (dolist (common common-labels)
                    (format stream " ~D" (getf common :node-num)))
                  (format stream "}~%"))))
            ;; now print the connections
            (dolist (node (car buf))
              (destructuring-bind (&key node-num bdd) node
                (typecase bdd
                  (bdd-node
                   (when (draw-node-p (bdd-positive bdd))
                     (let ((positive-num (getf (find (bdd-positive  bdd) (car buf)
                                                     :key (getter :bdd))
                                               :node-num)))
                       (format stream "~D -> ~D [style=~A,color=~A,penwidth=~D]~%"
                               node-num positive-num  "solid" "green" pen-width)))
                   (when (draw-node-p (bdd-negative bdd))
                     (let ((negative-num (getf (find (bdd-negative bdd) (car buf)
                                                     :key (getter :bdd))
                                               :node-num)))
                       (format stream "~D -> ~D [style=~A,color=~A,penwidth=~D,arrowhead=~s,arrowtail=~s,dir=~s]~%"
                               node-num negative-num "dashed" "red" pen-width "normal" "odot" "both")))))))))
         (t
          (let (nodes
                (num 0))
            (labels ((visit (bdd action path)
                       (funcall action bdd path)
                       (typecase bdd
                         (bdd-node
                          (visit (bdd-positive bdd) action (cons :L path))
                          (visit (bdd-negative bdd) action (cons :R path)))))
                     (name-node (bdd path)
                       (push (list :bdd bdd :node-num (incf num) :path path) nodes))
                     (print-node (bdd path &aux
                                             (node (find-node bdd path))
                                             (node-num (getf node :node-num)))
                       (dot-node bdd node-num))
                     (find-node (bdd path)
                       (find-if #'(lambda (node)
                                    (and (eq bdd (getf node :bdd))
                                         (equal path (getf node :path))))
                                nodes))
                     (print-connections (bdd path)
                       (typecase bdd
                         (bdd-node
                          (let* ((node-num  (getf (find-node bdd             path)           :node-num))
                                 (positive-num  (getf (find-node (bdd-positive  bdd) (cons :L path)) :node-num))
                                 (negative-num (getf (find-node (bdd-negative bdd) (cons :R path)) :node-num)))
                            (declare (type fixnum positive-num negative-num))
                            (format stream "~D -> ~D [style=~A]~%" node-num positive-num  "solid")
                            (format stream "~D -> ~D [style=~A]~%" node-num negative-num "dashed"))))))
              (visit bdd #'name-node ())
              (visit bdd #'print-node ())
              (visit bdd #'print-connections ())))))
         
       ;; footer
       (format stream "}~%")))))

(defun bdd-to-png (bdd &key (reduced t) (basename (format nil "~A/~A" (make-temp-dir "graph") (bdd-ident bdd)))
                         (draw-false-leaf t) (pen-width 2))
  "Generate a PNG (graphics) file to graphically view an ROBDD.  The special var adjuvant:*DOT-PATH* is used to locate
the dot (graphviz) program which will convert a .dot file to .png . Full path of the .png is returned.
:DRAW-FALSE-LEAVE nil may be used to simplify the display of the bdd, by omitting all
paths to the false leaf."
  (let ((dot-path (replace-all (format nil "~A.dot" basename) "//" "/"))
        (png-path (replace-all (format nil "~A.png" basename) "//" "/")))
    (ensure-directories-exist dot-path)
    (with-open-file (stream dot-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (bdd-to-dot bdd stream :reduced reduced :draw-false-leaf draw-false-leaf :pen-width pen-width))
    (run-program *dot-path*
                 (list "-Tpng" dot-path
                       "-o" png-path))
    png-path))
  

