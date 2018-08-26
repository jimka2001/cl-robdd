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


(defpackage :cl-robdd-analysis
  (:use :cl :cl-robdd ;;:open-pipe-to-file
	)
  (:export
   "ADDPLOT"
   "AXIS"
   "COLOR-TO-RGB"
   "CALL-WITH-DPROFILING"
   "CALL-WITH-SPROFILING"
   "GENERATE-LATEX-PLOTS"
   "CALL-WITH-TIMEOUT"
   "DEMAND-ENV-VAR"
   "JOIN-STRINGS"
   "MEASURE-AND-WRITE-BDD-DISTRIBUTION"
   "*COLORS*"
   "QSTAT-F"
   "RANDOM-BOOLEAN-COMBINATION"
   "TIKZPICTURE"
   "WITH-DUP-STREAM"))

(in-package :cl-robdd-analysis)

(defvar *colors* 
 '(
   ;; TODO add these colors '("red" "goldenrod" "olive" "blue" "lavender" "greeny" "dark-cyan" "teal" "orange"))
    ;;"000000"
   ;; this list comes from
   ;;  http://godsnotwheregodsnot.blogspot.com/2012/09/color-distribution-methodology.html
   ;;"FFFF00"
   "1CE6FF"
   "FF34FF" ;; decompose-types
   "6A3A4C" ;; decompose-types-rtev2
   "008941" ;; decompose-types-sat
   "006FA6" ;; decompose-types-graph
   "A30059" ;; bdd-decompose-types-strong
   ;;"FFDBE5"
   "7A4900" ;; bdd-decompose-types-weak
   "0000A6" ;; bdd-decompose-types-weak-dynamic
   "63FFAC" ;; bdd-decompose-types
   "B79762" ;; decompose-types-bdd-graph-strong
   "004D43" ;; decompose-types-bdd-graph-weak-dynamic
   "8FB0FF" ;; decompose-types-bdd-graph
   "997D87" ;; decompose-types-bdd-graph-weak
   ;; "5A0007" 
   ;; "809693" 
   ;; "FEFFE6"
   ;;"1B4400"
   "3B5DFF" ;; decompose-types-bdd-graph-baker
   "4FC601" ;; decompose-types-graph-baker
   "4A3B53" "FF2F80"
   "61615A" "BA0900" "6B7900" "00C2A0" "FFAA92" "FF90C9" "B903AA" "D16100"
   "DDEFFF" "000035" "7B4F4B" "A1C299" "300018" "0AA6D8" "013349" "00846F"
   "372101" "FFB500" "C2FFED" "A079BF" "CC0744" "C0B9B2" "C2FF99" "001E09"
   "00489C" "6F0062" "0CBD66" "EEC3FF" "456D75" "B77B68" "7A87A1" "788D66"
   "885578" "FAD09F" "FF8A9A" "D157A0" "BEC459" "456648" "0086ED" "886F4C"
   "34362D" "B4A8BD" "00A6AA" "452C2C" "636375" "A3C8C9" "FF913F" "938A81"
   "575329" "00FECF" "B05B6F" "8CD0FF" "3B9700" "04F757" "C8A1A1" "1E6E00"
   "7900D7" "A77500" "6367A9" "A05837" "6B002C" "772600" "D790FF" "9B9700"
   "549E79" "FFF69F" "201625" "72418F" "BC23FF" "99ADC0" "3A2465" "922329"
   "5B4534" "FDE8DC" "404E55" "0089A3" "CB7E98" "A4E804" "324E72" ;; "FF4A46"
   ;; I don't remember where this list comes from
   
   "e6194b"      ;;Red	(230, 25, 75)	(0, 100, 66, 0)
   "3cb44b"      ;;Green	(60, 180, 75)	(75, 0, 100, 0)
   "ffe119"	;;Yellow	(255, 225, 25)	(0, 25, 95, 0)
   "0082c8"	;;Blue	(0, 130, 200)	(100, 35, 0, 0)
   "f58231"	;;Orange	(245, 130, 48)	(0, 60, 92, 0)
   "911eb4"	;;Purple	(145, 30, 180)	(35, 70, 0, 0)
   "46f0f0"	;;Cyan	(70, 240, 240)	(70, 0, 0, 0)
   "f032e6"	;;Magenta	(240, 50, 230)	(0, 100, 0, 0)
   "d2f53c"	;;Lime	(210, 245, 60)	(35, 0, 100, 0)
   "fabebe"	;;Pink	(250, 190, 190)	(0, 30, 15, 0)
   "008080"	;;Teal	(0, 128, 128)	(100, 0, 0, 50)
   "e6beff"	;;Lavender	(230, 190, 255)	(10, 25, 0, 0)
   "aa6e28"	;;Brown	(170, 110, 40)	(0, 35, 75, 33)
   "fffac8"	;;Beige	(255, 250, 200)	(5, 10, 30, 0)
   "800000"	;;Maroon	(128, 0, 0)	(0, 100, 100, 50)
   "aaffc3"	;;Mint	(170, 255, 195)	(33, 0, 23, 0)
   "808000"	;;Olive	(128, 128, 0)	(0, 0, 100, 50)
   "ffd8b1"	;;Coral	(255, 215, 180)	(0, 15, 30, 0)
   "000080"	;;Navy	(0, 0, 128)	(100, 100, 0, 50)
   "808080"	;;Grey	(128, 128, 128)	(0, 0, 0, 50)
    ))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
            while pos)))

(defun demand-env-var (env-var-name)
  (or (sb-posix:getenv env-var-name)
      (error "Missing env var ~s" env-var-name)))

(defun qstat-f ()
  "call qstat -f and write the output to a file with the .sXXXX extension
similar to where current Output_Path is indicating."
  (let ((pbs-jobid (sb-posix:getenv "PBS_JOBID"))
        (delimeter (format nil "~%~a" '#\Tab )))
    ;; call qstat -f and get the output;
    ;; qstat -f wraps long lines, so we first have to unwrap them by finding
    ;; "\n\t" and replacing with ""
    (let* ((qstat-out (replace-all (with-output-to-string (str)
                                    (sb-ext:run-program "qstat" (list "-f" pbs-jobid)
                                                        :search t
                                                        :output str))
                                   delimeter ""))
           ;; we want to write into file.sXXXXX, so we have to
           ;; find the Output_Path = /path/to/file.oXXXXXX and parse that
           ;; to get the desired name of the new status output file.
           (pos-host (search "Output_Path = " qstat-out))
           (pos-path (search ":" qstat-out :start2 pos-host))
           (pos-eol  (search (format nil "~%") qstat-out :start2 pos-host))
           ;; output-path = "/path/to/file.oXXXXXX"
           (output-path (subseq qstat-out (1+ pos-path) pos-eol))
           (pos-extension (1+ (search "." output-path :from-end t)))
           ;; extension = "XXXXXX" ; excluding the ".o"
           (extension (subseq output-path (1+ pos-extension)))
           ;; leading = "/path/to/file." ;; including the "."
           (leading   (subseq output-path 0 pos-extension))
           ;; status-filename = "/path/to/file.sXXXXXX"
           (status-filename (concatenate 'string leading "s" extension)))
      
      (with-open-file (stream status-filename
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (format stream "~%")
        (write-line qstat-out stream)))))

(defmacro with-dup-stream ((stream file) &body body)
  "Rebind the named STREAM to a broadcast-stream which duplicates it to the named FILE, evaluating the BODY within the dynamic extend of the rebinding."
  (let ((log-file (gensym)))
    `(with-open-file (,log-file ,file
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
       (let ((,stream (make-broadcast-stream ,stream ,log-file)))
         ,@body))))


(defun join-strings (delimeter strings)
  (with-output-to-string (str)
    (when (car strings)
      (format str "~A" (car strings)))
    (dolist (string (cdr strings))
      (format str "~A~A" delimeter string))))

(defun cmp-xor-implementations-1 (n)
  (let ((a (random-boolean-combination n))
	(b (random-boolean-combination n)))
    (labels ((f2 (a b)
	       (bdd-or (bdd-and-not a b)
		       (bdd-and-not b a)))
	     (f3 (a b)
	       (bdd-and-not (bdd-or a b)
			    (bdd-and a b)))
	     (timing (thunk &aux plist)
	       (sb-ext:call-with-timing (lambda (&rest timing-args)
					  (setf plist timing-args))
					thunk)
	       plist)
	     (cmp (op)
	       (garbage-collect)
	       (bdd-with-new-hash ()
		 (let ((bdd-a (bdd a))
		       (bdd-b (bdd b)))
		   (timing (lambda ()
			     (funcall op bdd-a bdd-b)))))))

      (loop :for name :in '("bdd-xor" "ab! or a!b" "(a+b)(ab)!")
	    :for op   :in (list #'bdd-xor #'f2 #'f3)
	    :collect (list* :name name (cmp op))))))

(defun cmp-xor-implementations (&key (min 2) (max 22) (repeat 1) (time-key :user-run-time-us) (verbose nil))
  (let (xys-1 xys-2 xys-3)
    (loop :for n :from min :to max
	  :do (let ((total-1 0)
		    (total-2 0)
		    (total-3 0))
		(dotimes (i repeat)
		  (when verbose
		    (format t "n=~D:~D~%" n i)
		    (finish-output t))

		  (destructuring-bind (plist-1 plist-2 plist-3) (cmp-xor-implementations-1 n)
		    (incf total-1 (getf plist-1 time-key))
		    (incf total-2 (getf plist-2 time-key))
		    (incf total-3 (getf plist-3 time-key))))
		(let ((average-1 (/ total-1 (float repeat 1.0) 1e6))
		      (average-2 (/ total-2 (float repeat 1.0) 1e6))
		      (average-3 (/ total-3 (float repeat 1.0) 1e6)))
		  (push (list n average-1) xys-1)
		  (push (list n average-2) xys-2)
		  (push (list n average-3) xys-3))))
    (list (nreverse xys-1)
	  (nreverse xys-2)
	  (nreverse xys-3))))

(defun cmp-xor-implementations-latex (fname &key (max 22) (repeat 1))
  ;; fname "/Volumes/Disk2/jimka/research/autogen/xor-cmp.ltxdat"
  (destructuring-bind (f1-xys f2-xys f3-xys) (cmp-xor-implementations :max max :repeat repeat :verbose t)
    (with-open-file (stream fname :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format t "writing to ~A~%" fname)
      (tikzpicture stream
		   "comparing 3 implementaions of bdd xor"
		   (lambda ()
		     (axis stream
			   '(("ylabel" "time (seconds)")
			     ("xlabel" "{Number of Boolean variables $\\numvars$}")
			     ("xtick" "{0,5,10,15,20}")
			     "ymajorgrids"
			     "xmajorgrids"
			     ("legend style" (("at" "{(0,1)}")
					      ("anchor" "north west")
					      ("font" "\\tiny"))))
			   (lambda ()
			     (addplot stream
				      "xor"
				      ()
				      "(~D,~D)"
				      f1-xys
				      :logy t
				      :addplot "addplot+")
			     (addplot stream
				      "ab! or a!b" 
				      ()
				      "(~D,~D)"
				      f2-xys
				      :logy t
				      :addplot "addplot+")
			     (addplot stream
				      "(a+b)(ab)!"
				      ()
				      "(~D,~D)"
				      f3-xys
				      :logy t
				      :addplot "addplot+")
			     (format stream "\\legend{${A\\xor B}$,${(A\\setminus B)\\vee(B\\setminus A)}$,${(A\\vee B)\\setminus(A\\wedge B)}$}~%"))
			   :logy t)
		     )))))

(defun cmp-fold-implementations-1 (n &key (density 1.0))
  (let ((bool-comb (random-boolean-combination n :density density)))
    (labels ((timing (thunk &aux plist)
	       (sb-ext:call-with-timing (lambda (&rest timing-args)
					  (setf plist timing-args))
					thunk)
	       plist)
	     (cmp (cl-robdd::*bdd-reduce-function*)
	       (garbage-collect)
	       (bdd-with-new-hash ()
		 (timing (lambda ()
			   (bdd bool-comb))))))

      (reverse (loop :for name :in '("tree" "linear")
		     :for reduce-function   :in (list #'cl-robdd::tree-reduce #'reduce)
		     :collect (list* :name name (cmp reduce-function)))))))

(defun profile-linear-tree-like (n &key ((:reduce cl-robdd::*bdd-reduce-function*) #'reduce))
  (sb-profile:profile "CL-ROBDD" "CL-ROBDD-ANALYSIS")

  (let ((bool-comb (random-boolean-combination n :density 0.2)))
    (garbage-collect)
    ;; (sb-sprof:reset)
    ;; (sb-sprof:profile-call-counts "CL-ROBDD" "CL-ROBDD-ANALYSIS")
    ;; (sb-sprof:with-profiling (:loop nil)
    ;;   (dotimes (_ 1000)
    ;; 	(bdd-with-new-hash ()
    ;; 	  (bdd bool-comb))))
    ;; (sb-sprof:report :type :flat)

    (let ((cl-robdd::*bdd-reduce-function* #'reduce))
      (format t "=== profile with ~A~%" cl-robdd::*bdd-reduce-function*)
      (sb-profile:reset)
      (bdd-with-new-hash ()
	(bdd bool-comb))
      (sb-profile:report :print-no-call-list nil))

    (garbage-collect)
    (let ((cl-robdd::*bdd-reduce-function* #'tree-reduce))
      (format t "=== profile with ~A~%" cl-robdd::*bdd-reduce-function*)
      (sb-profile:reset)
      (bdd-with-new-hash ()
	(bdd bool-comb))
      (sb-profile:report :print-no-call-list nil)
      )))
  
(defun cmp-fold-implementations (&key (min 2) (max 22) (repeat 1) (density 1.0) (time-key :user-run-time-us) (scale 1e6) (verbose nil))
  (let (xys-linear xys-tree)
    (loop :for n :from min :to max
	  :do (let ((total-linear 0)
		    (total-tree 0))
		(dotimes (i repeat)
		  (when verbose
		    (format t "n=~D:~D~%" n i)
		    (finish-output t))
		  (destructuring-bind (plist-linear plist-tree) (cmp-fold-implementations-1 n :density density)
		    (incf total-linear (getf plist-linear time-key))
		    (incf total-tree   (getf plist-tree time-key))))
		(let ((average-linear (/ total-linear (float repeat 1.0) scale))
		      (average-tree   (/ total-tree (float repeat 1.0) scale)))
		  (push (list n average-linear) xys-linear)
		  (push (list n average-tree)   xys-tree))))
    (list (nreverse xys-linear)
	  (nreverse xys-tree))))

(defun cmp-fold-latex (fname &key (max 22) (repeat 1))
  ;; fname "/Volumes/Disk2/jimka/research/autogen/cmp-fold-bdd-construction.ltxdat"
  (with-open-file (stream fname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format t "writing to ~A~%" fname)
    (tikzpicture stream
		 "comparing linear vs tree fold implentation for bdd construction"
		 (lambda (&aux (colors *colors*))
		   (axis stream
			 '(("ylabel" "time (seconds)")
			   ("xlabel" "{Number of Boolean variables $\\numvars$}")
			   ("xtick" "{0,5,10,15,20}")
			   "ymajorgrids"
			   "xmajorgrids"
			   ("legend style" (("at" "{(1,0)}")
					    ("anchor" "south west")
					    ("font" "\\tiny"))))
			 (lambda (&aux legend)
			   (dolist (density '(1.0 0.22 0.05))
			     (format t "density=~A~%" density)
			     (destructuring-bind (linear-xys tree-xys) (cmp-fold-implementations :max max :repeat repeat
												 :verbose t :density density)
			       (push (format nil "{linear-fold $\\eta=~A$}" density) legend)
			       (addplot stream
					(car legend)
					()
					"(~D,~D)"
					linear-xys
					:color (pop colors)
					:logy t
					:addplot "addplot+")
			       (push (format nil "{tree-fold $\\eta=~A$}" density) legend)
			       (addplot stream
 					(car legend)
					() 
					"(~D,~D)"
					tree-xys
					:color (pop colors)
					:logy t
					:addplot "addplot+")))
			   (format stream "\\legend{~A}~%"
				   (join-strings "," (reverse legend))))
			 :logy t)
		   ))))
