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
   "%BDD-CMP"
   "%BDD-NODE"
   "*BDD-CMP-FUNCTION*"
   "*BDD-FALSE*"
   "*BDD-HASH-STRENGTH*"
   "*BDD-HASH-STRUCT*"
   "*BDD-TRUE*"
   "BDD"
   "BDD-DNF-WRAP"
   "BDD-ALLOCATE"
   "BDD-AND"
   "BDD-AND-NOT"
   "BDD-BFS"
   "BDD-CALL-WITH-NEW-HASH"
   "BDD-CMP"
   "BDD-FACTORY"
   "BDD-FALSE"
   "BDD-FIND"
   "BDD-HASH"
   "BDD-IDENT"
   "BDD-LABEL"
   "BDD-LEAF"
   "BDD-LIST-TO-BDD"
   "BDD-MAKE-KEY"
   "BDD-NEGATIVE"
   "BDD-NEW-HASH"
   "BDD-NODE"
   "BDD-NODE-TYPE"
   "BDD-NOT"
   "BDD-OR"
   "BDD-POSITIVE"
   "BDD-SERIALIZE"
   "BDD-TO-DNF"
   "BDD-TO-EXPR"
   "BDD-TO-PNG"
   "BDD-TRUE"
   "BDD-WITH-NEW-HASH"
   "BDD-XOR"
   "BOOLEAN-EXPR-TO-LATEX"
   "INCR-HASH"
   "SCALE-SCI-NOTATIONS"
   "SCI-NOTATION-STRING"
   "SCI-NOTATION"
   "TREE-REDUCE"
   ))
