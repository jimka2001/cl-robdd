;; Copyright (c) 2020 EPITA Research and Development Laboratory
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

(in-package   :graph-coloring)

(defun uni-graph-to-bi-graph (uni-graph &key (test #'eql))
  (let ((hash (make-hash-table :test test)))
    (maphash (lambda (state1 neighbors)
               (dolist (state2 neighbors)
                 (push state2 (gethash state1 hash nil))
                 (push state1 (gethash state2 hash nil))))
             uni-graph)
    hash))


(defvar *usa-graph*
  (let* ((all-states '("AL"
                      ;;"AK"
                      "AZ"
                      "AR"
                      "CA"
                      "CO"
                      "CT"
                      "DC"
                      "DE"
                      "FL"
                      "GA"
                      ;;"HI"
                      "ID"
                      "IL"
                      "IN"
                      "IA"
                      "KS"
                      "KY"
                      "LA"
                      "ME"
                      "MD"
                      "MA"
                      "MI"
                      "MN"
                      "MS"
                      "MO"
                      "MT"
                      "NE"
                      "NV"
                      "NH"
                      "NJ"
                      "NM"
                      "NY"
                      "NC"
                      "ND"
                      "OH"
                      "OK"
                      "OR"
                      "PA"
                      "RI"
                      "SC"
                      "SD"
                      "TN"
                      "TX"
                      "UT"
                      "VT"
                      "VA"
                      "WA"
                      "WV"
                      "WI"
                      "WY"))
        (uni-graph-alist '(("CA".("OR" "NV" "AZ"))
                           ("OR".("WA" "ID" "NV"))
                           ("NV".("AZ" "UT" "ID"))
                           ("WA".("ID"))
                           ("ID".("MT" "WY" "UT"))
                           ("UT".("WY" "CO" "AZ"))
                           ("AZ".("NM"))
                           ("MT".("ND" "SD" "WY"))
                           ("WY".("SD" "NE" "CO"))
                           ("CO".("NE" "KS" "OK" "NM"))
                           ("NM".("OK" "TX"))
                           ("ND".("SD" "MN"))
                           ("SD".("MN" "IA" "NE"))
                           ("NE".("IA" "MO" "KS"))
                           ("KS".("MO" "OK"))
                           ("OK".("MO" "AR" "TX"))
                           ("TX".("AR" "LA"))
                           ("MN".("WI" "IA"))
                           ("IA".("WI" "IL" "MO"))
                           ("MO".("IL" "KY" "TN" "AR"))
                           ("AR".("TN" "MS" "LA"))
                           ("LA".("MS"))
                           ("WI".("MI" "IL"))
                           ("IL".("IN" "KY"))
                           ("MS".("TN" "AL"))
                           ("MI".("OH" "IN"))
                           ("IN".("OH" "KY"))
                           ("KY".("OH" "WV" "VA" "TN"))
                           ("TN".("VA" "NC" "GA" "AL"))
                           ("AL".("GA" "FL"))
                           ("OH".("PA" "WV"))
                           ("WV".("PA" "MD" "VA"))
                           ("VA".("MD" "DC" "NC"))
                           ("GA".("NC" "SC" "FL"))
                           ("PA".("NY" "NJ" "DE" "MD"))
                           ("MD".("DE" "DC"))
                           ("NC".("SC"))
                           ("VT".("NH" "MA" "NY"))
                           ("NY".("MA" "CT" "NJ"))
                           ("NJ".("DE"))
                           ("NH".("ME" "MA"))
                           ("MA".("RI" "CT"))
                           ("CT".("RI"))
                           ))
         (state-uni-graph (assoc-to-hash uni-graph-alist :test #'equal :assoc-get #'cdr)))
        
    (list :all-states all-states
          :state-uni-graph state-uni-graph
          :state-bi-graph (uni-graph-to-bi-graph state-uni-graph :test #'equal))))

(assert (gethash "AL" (getf *usa-graph* :state-bi-graph)))
(assert (typep (gethash "AL" (getf *usa-graph* :state-bi-graph)) 'list))

(defun make-state-to-var-map (all-states)
  (let ((hash (make-hash-table :test #'equal))
        (n 0))
    (loop :for state :in all-states
          :do (setf (gethash state hash) (cons (+ 1 (* 2 n))
                                               (+ 2 (* 2 n))))
          :do (incf n))
    hash))

(defun graph-to-bdd (states uni-graph)
  (let ((state-to-var (make-state-to-var-map states)))
    (flet ((get-constraints (ab)
             ;; convert the connection (neighbor) information from a state (ab)
             ;;   to a Bdd representing the color constraints because neighboring
             ;;   states cannot have the same color.   a and b are the color bits
             ;;   of state ab.  c and d are the color bits of the neighbor.
             ;;   The constraint (per neighbor) is that either a and c are different
             ;;   or b and d are different.
             ;;   The getConstraints function AND's all these constraints for the
             ;;   neighbors of a given state.
             (destructuring-bind (a . b) (gethash ab state-to-var)
               (let ((neighbors (gethash ab uni-graph ())))
                 (reduce (lambda (acc cd)
                           (destructuring-bind (c . d) (gethash cd state-to-var)
                             (bdd-and acc (bdd-or (bdd-xor (bdd b) (bdd d))
                                                  (bdd-xor (bdd a) (bdd c))))))
                         neighbors
                         :initial-value *bdd-true*)))))
      (values state-to-var
              (tree-reduce #'bdd-and
                           states
                           :initial-value *bdd-true*
                           :key #'get-constraints)))))

(defun find-candidate (current-states complete-bi-graph possible-candidates)
  ;; graph is Hash[String List[String]]
  ;; find the element of POSSIBLE-CANDIDATES which has the most connections
  ;; to the current-graph
  (let* ((priority-list (sort (loop :for st1 :in possible-candidates
                                    :unless (member st1 current-states :test #'string=)
                                      :collect (cons (loop :for st2 :in current-states
                                                           :when (member st2 (gethash st1 complete-bi-graph)
                                                                         :test #'string=)
                                                             :count :it)
                                                     st1))
                              #'>
                              :key #'car)))
    (cdr (car priority-list))))

;; find some subgraph of usa-graph->state-bi-graph which contains at least the starting state
;;   and a total of num-nodes - 1 other states.
(defun find-sub-graph (start num-nodes state-bi-graph all-states)
  (declare (type hash-table state-bi-graph))
  ;; state-bi-graph e.g., (getf *usa-graph* :state-bi-graph)
  ;; returns (values List[String] Hash[String List[String]])
  (labels ((recur (size states-in-current-graph current-graph-assoc)
             (assert (not (member nil states-in-current-graph)))
             (cond ((= size num-nodes)
                    (values states-in-current-graph
                            (assoc-to-hash current-graph-assoc :assoc-get #'cdr)))
                   (t
                    (let ((st1 (find-candidate states-in-current-graph state-bi-graph all-states)))
                      (declare (type string st1))
                      (recur (1+ size)
                             (adjoin st1 states-in-current-graph)
                             (cons (cons st1 (remove-if-not (lambda (st2)
                                       (member st2 (gethash st1 state-bi-graph)))
                                                            states-in-current-graph))
                                   current-graph-assoc)))))))
    (recur 1
           (list start)
           (list (cons start nil)))))

;; calculate a mapping from graph node to color given that the hard work
;;   of solving the Boolean equation has already been done.
(defun assign-colors (colorization assign-true assign-false colors)
  (declare (ignore assign-false)
           (type vector colors)
           (type hash-table colorization)
           (type list assign-true assign-false))

  ;; colorization maps the graph node to a pair of integers which represent the bitmask of the color
  ;;      which the node has been assigned.  such a Map[String,(Int,Int)] can be obtained from graphToBdd(...)
  ;; assign-true and assign-false are lists of bdd labels corresponding which are assigned true and
  ;;     false respectively.  is an object which specifies which variables in the Bdd are set to true.
  ;;      such values can  be obtained from 
  ;; colors is an Array of length 4, each array entry is a user color,
  ;;      e.g. Array("red","green","blue","yellow")
  (assert (= (length colors) 4))
  (let ((hash (make-hash-table :test #'equal)))

    (maphash (lambda (node value)
               (destructuring-bind (v1 . v2) value
                 ;; v1 and v2 are labels (or variables within the Bdd), their Boolean value
                 ;;  represents two bits of a color.

                 (let* ((c1 (member v1 assign-true :test #'equal))
                        (c2 (member v2 assign-true :test #'equal))
                        (color (+ (* 2 (if c1 1 0))
                                  (if c2 1 0))))
                   ;; if a variable is missing, it is a don't care
                   ;;   we implicitly assume it is false
                               
                   (setf (gethash node hash) (aref colors color)))))
             colorization)
    hash))

(defun sanity-check (num-nodes)
  (bdd-with-new-hash ()
    (multiple-value-bind (states sub-graph) (find-sub-graph "ME" num-nodes
                                                            (getf *usa-graph* :state-bi-graph)
                                                            (getf *usa-graph* :all-states))
      (multiple-value-bind (colorization bdd) (graph-to-bdd states sub-graph)
        ;; (bdd-view bdd :draw-false-leaf nil)
        (multiple-value-bind (assign-true assign-false found-p) (bdd-find-satisfying-assignment bdd)
          (if found-p
              (format t "num-nodes=~A~% assign=true=~A colorization=~A~%" num-nodes
                      assign-true
                      (hash-to-assoc (assign-colors colorization assign-true assign-false
                                                    (vector "red"
                                                            "green"
                                                            "blue"
                                                            "yellow"))))
              (format t "num-nodes=~A no colorization found~%" num-nodes)))))))

(defun sanity-check-2 ()
  (loop :for num-nodes :from 8 :to 26
        :do (sanity-check num-nodes)))
