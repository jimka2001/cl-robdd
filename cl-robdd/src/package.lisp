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

(cl:defpackage :cl-robdd
  (:use :cl :adjuvant)
  (:export
   "*BDD-CMP-FUNCTION*"
   "*BDD-FALSE*"
   "*BDD-HASH-STRENGTH*"
   "*BDD-REDUCE-FUNCTION*"
   "*BDD-TRUE*"
   "BDD"
   "BDD-ALLOCATE"
   "BDD-AND"
   "BDD-AND-NOT"
   "BDD-BFS"
   "BDD-CALL-WITH-NEW-HASH"
   "BDD-DNF-WRAP"
   "BDD-FACTORY"
   "BDD-FALSE"
   "BDD-FIND"
   "BDD-IDENT"
   "BDD-LABEL"
   "BDD-LEAF"
   "BDD-NEGATIVE"
   "BDD-NODE"
   "BDD-NOT"
   "BDD-OR"
   "BDD-POSITIVE"
   "BDD-STD-NUMERICAL-CMP"
   "BDD-TO-DNF"
   "BDD-TO-DOT"
   "BDD-TO-EXPR"
   "BDD-TO-PNG"
   "BDD-TRUE"
   "BDD-VIEW"
   "BDD-WALK"
   "BDD-WITH-NEW-HASH"
   "BDD-XOR"
   "BDD-XNOR"
   "BDD-VISIT-SATISFYING-ASSIGNMENTS"
   "BDD-FIND-SATISFYING-ASSIGNMENT"
   ;; we have to export XNOR and XNOR becasuse they are not part of the CL package.
   ;; If we don't, then (BDD '(XOR A B)) will not work in user code as
   ;;  XOR will intern as USER-APPLICATION::XOR rather than CL-ROBDD::XOR.
   ;;  similar for XNOR.
   "XOR"  
   "XNOR"
   ))

(cl:defpackage :graph-coloring
  (:use :cl :adjuvant :cl-robdd)
  (:export
))

