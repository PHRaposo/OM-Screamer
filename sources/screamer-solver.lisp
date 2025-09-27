;; Copyright (c) 2024 Paulo Henrique Raposo

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCREAMER-SOLVER

(defmethod! screamer-solver ((variables t)
                             &key (p-variables nil)
                                  (constraints nil)
                                  (p-constraints nil)
                                  (constraints-all nil)
                                  (screamer-valuation "one-value")
                                  (n-ith-value nil)
                                  (force-function "static-ordering linear-force")
                                  (map-solutions nil)
                                  (output nil) (count-failures? nil)
                                  (objective-form nil) (form2 nil))

    :initvals '(nil nil nil nil nil "one-value" nil "static-ordering linear-force" nil nil nil nil nil)

    :indoc '("variable or list" "propagation-variables<lambda-patch>" "constraint<lambda-patch> or list"
		"propagation-constraints<lambda-patch>" "constraints-all<lambda-patch>"  "one-value, all-values, listener, n-values, ith-value or best-value"
                "integer" "ordering-force-functions" "map-solutions<lambda-patch>" "list" "symbol t or nil" "<lambda-patch>"  "<lambda-patch>")

    :doc "Screamer Constraint Solver
  <VARIABLES> variable or list of variables.
  <P-VARIABLES> lambda patch or list of lambda patches. Generates a new-list of variables.
  <CONSTRAINTS> lambda patch or list of lambda patches => constraint to variables.
  <P-CONSTRAINTS> lambda patch or list of lambda patches => constraints to propagation variables.
  <CONSTRAINTS-ALL> lambda patch or list of lambda patches with two inputs (vars / p-vars) => constraints to all variables.
  <SCREAMER-VALUATION> menuin with four options (one-value, all-values, listener, n-values or ith-value).
  <N-ITH-VALUE> integer to be used with n-values or ith-value.
  <FORCE-FUNCTION> a string or should be connected to force-function.
  <MAP-SOLUTIONS> lambda patch or list of lambda patches => constraints to solutions. Will backtrack if fails.
  <OUTPUT> List of positions (for variables only) or list of lists of positions (for variables and propagation variables).
   For all-values and n-values returns all variables.
 <COUNT-FAILURES?> string -> on or off.
 <OBJECTIVE-FORM>and<FORM2>: lambda patches with one input (variables only) or two inputs (variables and propagation variables) -> best-value forms."

    :menuins '((5 (("one-value" "one-value") ("all-values" "all-values")
                   ("listener" "listener") ("n-values" "n-values") ("ith-value" "ith-value") ("best-value" "best-value") ))
               (10 (("off" nil) ("on" t))) )
    :icon 486

 (screamer-solution variables
                   p-variables
                   constraints
                   p-constraints
                   constraints-all
                   screamer-valuation
                   n-ith-value
                   force-function
                   map-solutions
                   output
                   count-failures?
                   objective-form
                   form2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCREAMER-SOLUTION

(defun screamer-solution (vars p-vars cs p-cs cs-all valuation n-ith ordering-force map-s out count-fail? obj-form form2)
   (let* ((compiled-p-vars ;==> PROPAGATION VARIABLES
           (if (functionp p-vars)
               (fdefinition (compile-screamer-constraint p-vars))
               (mapcar #'(lambda (x) (fdefinition (compile-screamer-constraint x))) p-vars)))
          (compiled-constraints ;==> COMPILED CONSTRAINTS TO VARIABLES
           (if (functionp cs)
               (fdefinition (compile-screamer-constraint cs))
               (mapcar #'(lambda (x) (fdefinition (compile-screamer-constraint x))) cs)))
          (compiled-p-constraints ;==> COMPILED CONSTRAINTS PROPAGATION VARIABLES
           (if p-cs
              (if (functionp p-cs)
               (fdefinition (compile-screamer-constraint p-cs))
               (mapcar #'(lambda (x) (fdefinition (compile-screamer-constraint x))) p-cs))))
           (compiled-constraints-all ;==> COMPILED CONSTRAINTS ALL VARIABLES
            (if cs-all
               (if (functionp cs-all)
                (fdefinition (compile-screamer-constraint cs-all))
                (mapcar #'(lambda (x) (fdefinition (compile-screamer-constraint x))) cs-all))))
            (compiled-map-sol ;==> COMPILED CONSTRAINTS MAP SOLUTIONS
             (if map-s
                (if (functionp map-s)
                 (fdefinition (compile-screamer-constraint map-s))
                 (mapcar #'(lambda (x) (fdefinition (compile-screamer-constraint x))) map-s))))
         (compiled-forms ;==> BEST-VALUE FORMS
		 (if obj-form
		     (if form2
		         (mapcar #'(lambda (x) (fdefinition (compile-screamer-constraint x))) (list obj-form form2))
		   	 (fdefinition (compile-screamer-constraint obj-form)))
		      nil))
		 (vars-name (intern (string (gensym "vars")) :om)) ;==> NAME FOR VARIABLES (SYMBOL)
		 (p-vars-name (intern (string (gensym "p-vars")) :om)) ;==> NAME FOR PROPAGATION VARIABLES (SYMBOL)
		 (constraints ;==> CONSTRAINTS CODE
		    (if compiled-constraints
		     (if (functionp compiled-constraints)
		         `(apply ',compiled-constraints (list ,vars-name))
		         `(mapcar #'(lambda (cs) (apply cs (list ,vars-name))) ',compiled-constraints))
		        nil ))
	    (p-constraints ;==> P-CONSTRAINTS CODE
	      (if compiled-p-constraints
	         (if (functionp compiled-p-constraints)
	          `(apply ',compiled-p-constraints (list ,p-vars-name))
	          `(mapcar #'(lambda (cs) (apply cs (list ,p-vars-name))) ',compiled-p-constraints))
	        nil ))
	    (constraints-all ;==> CONSTRAINTS-ALL CODE
	      (if compiled-constraints-all
	         (if (functionp compiled-constraints-all)
	          `(apply ',compiled-constraints-all (list ,vars-name ,p-vars-name))
	          `(mapcar #'(lambda (cs) (apply cs (list ,vars-name ,p-vars-name))) ',compiled-constraints-all))
	        nil ))
	    (val (cond ((equal valuation "one-value") 0)
	                 ((equal valuation "all-values") 1)
	                 ((equal valuation "listener") 2)
	                 ((equal valuation "n-values") 3)
	                 ((equal valuation "ith-value") 4)
	                 ((equal valuation "best-value") 5)
			         (t (progn (om-message-dialog "UNKNOWN VALUATION OPTION!") (om-abort)))))
	   (n-ith-value n-ith)
	   (ordering ;==> ORDERING
		(let ((ord (first (string-to-list ordering-force))))
	     (cond ((equal "static-ordering" ord) 0)
		       ((equal "reorder" (first ord)) 1)
		       (t 0))))
	  (force-function ;==> FORCE-FUNCTION FOR STATIC-ORDERING
	   (if (= 1 ordering)
		    nil
		   (let ((force (second (string-to-list ordering-force))))
		    (cond ((equal "linear-force" force) 0)
		          ((equal "divide-and-conquer-force" force) 1)
		          ((equal "random-force" force) 2)
		          (t 0)))))
          (cost-function (if (= ordering 1) ;==>  REORDER COST-FUNCTION
                             (cond ((equal "domain-size" (second ordering-force)) 0)
 	                           ((equal "range-size" (second ordering-force)) 1)
 	                           ((equal "score-position" (second ordering-force)) 2)
 	                           (t 0)) nil))
          (terminate? (if (= ordering 1) ;==> REORDER TERMINATE?
		          (cond ((equal "(declare (ignore x))" (third ordering-force)) 0)
                                ((equal "(< x 1e-6)" (third ordering-force)) 1)
			        ((functionp (third ordering-force)) 2)
                                (t 0)) nil))
          (order (if (= ordering 1) ;==> REORDER ORDER
		     (cond ((equal ">" (fourth ordering-force)) 0)
			   ((equal "<" (fourth ordering-force)) 1)
	                   (t 0)) nil))
          (reorder-force (if (= ordering 1) ;==> REORDER FORCE-FUNCTION
	                     (cond ((equal "linear-force" (fifth ordering-force)) 0)
				   ((equal "divide-and-conquer-force" (fifth ordering-force)) 1)
				   ((equal "random-force" (fifth ordering-force)) 2)
	                           (t 0)) nil))
          (terminate?-fn (if (and (= ordering 1) (functionp (third ordering-force))) ;==> REORDER COMPILED TERMINATE?-FUNCTION
			          (fdefinition (compile-screamer-constraint (third ordering-force)))
			   	   nil))
          (solution-code ;==> CODE FOR SCREAMER VALUATION AND SOLUTION
	   (case ordering
	    (0 ;==>  STATIC-ORDERING
	     (case val
	      (0 `(s::one-value
			  (map-solutions ',compiled-map-sol
	       (s::solution ,(if p-vars `(list ,vars-name ,p-vars-name)
                                                  vars-name)
	      (s::static-ordering ,(case force-function
	                                     (0 `#'s::linear-force)
	                                     (1 `#'s::divide-and-conquer-force)
                                          (2 `#'s::random-force)))))))
	      (1 `(s::all-values
			   (map-solutions ',compiled-map-sol
	              (s::solution  ,(if p-vars `(list ,vars-name ,p-vars-name)
                                                  vars-name)
	               (s::static-ordering ,(case force-function
	                                             (0 `#'s::linear-force)
	                                             (1 `#'s::divide-and-conquer-force)
                                                  (2 `#'s::random-force)))))))
	      (2  `(s::print-values
			    (map-solutions ',compiled-map-sol
	               (s::solution ,(if p-vars `(list ,vars-name ,p-vars-name)
                                                  vars-name)
	                (s::static-ordering ,(case force-function
	                                              (0 `#'s::linear-force)
	                                              (1 `#'s::divide-and-conquer-force)
                                                   (2 `#'s::random-force)))))))
	      (3  `(s::n-values ,n-ith-value
		  	 (map-solutions ',compiled-map-sol
	               (s::solution  ,(if p-vars `(list ,vars-name ,p-vars-name)
                                                  vars-name)
	                (s::static-ordering ,(case force-function
	                                              (0 `#'s::linear-force)
	                                              (1 `#'s::divide-and-conquer-force)
                                                   (2 `#'s::random-force)))))))
	      (4  `(s::ith-value ,n-ith-value
		  	 (map-solutions ',compiled-map-sol
	               (s::solution ,(if p-vars `(list ,vars-name ,p-vars-name)
                                                  vars-name)
	                (s::static-ordering ,(case force-function
	                                              (0 `#'s::linear-force)
	                                              (1 `#'s::divide-and-conquer-force)
                                                   (2 `#'s::random-force)))))))
	      (5  `(s::best-value
			   (map-solutions ',compiled-map-sol
	            (s::solution ,(if p-vars `(list ,vars-name ,p-vars-name)
                                                  vars-name)
	             (s::static-ordering ,(case force-function
	                                   (0 `#'s::linear-force)
	                                   (1 `#'s::divide-and-conquer-force)
                                        (2 `#'s::random-force)))))
			   ,(cond ((not (null form2))
	  			      `(apply ',(first compiled-forms) ,(if p-vars `(list ,vars-name ,p-vars-name)
                                                  `(list ,vars-name)))
	                  `(apply ',(second compiled-forms) ,(if p-vars `(list ,vars-name ,p-vars-name)
                                                  `(list ,vars-name)) ))
					  (t `(apply ',compiled-forms ,(if p-vars `(list ,vars-name ,p-vars-name)
                                                  `(list ,vars-name)))))))
		))
	  (1 ;==> REORDER
	     (case val
	      (0  `(s::one-value
			   (map-solutions ',compiled-map-sol
	       (s::solution ,(if p-vars `(list ,vars-name ,p-vars-name)
                                                  vars-name)
	      (s::reorder ,(case cost-function
	                            (0 `#'s::domain-size)
	                            (1 `#'s::range-size)
	                            (2 `#'s::score-position)
								)
	                       ,(case terminate?
	                            (0 `#'(lambda (x) (declare (ignore x)) nil))
	                            (1 `#'(lambda (x) (< x 1e-6)))
				    (2 `#'(lambda (x) (apply ',terminate?-fn (list x)))))
	                       ,(case order
	                            (0 `#'>)
	                            (1 `#'<))
	                       ,(case reorder-force
	                            (0 `#'s::linear-force)
	                            (1 `#'s::divide-and-conquer-force)
                                 (2 `#'s::random-force)))))))
	      (1 `(s::all-values
			   (map-solutions ',compiled-map-sol
	            (s::solution  ,(if p-vars `(list ,vars-name ,p-vars-name)
                                                  vars-name)
	      (s::reorder ,(case cost-function
	                            (0 `#'s::domain-size)
	                            (1 `#'s::range-size)
	                            (2 `#'s::score-position)
				    )
	                       ,(case terminate?
	                            (0 `#'(lambda (x) (declare (ignore x)) nil))
	                            (1 `#'(lambda (x) (< x 1e-6)))
				    (2 `#'(lambda (x) (apply ',terminate?-fn (list x)))))
	                       ,(case order
	                            (0 `#'>)
	                            (1 `#'<))
	                       ,(case reorder-force
	                            (0 `#'s::linear-force)
	                            (1 `#'s::divide-and-conquer-force)
                                 (2 `#'s::random-force)))))))
	      (2  `(s::print-values
			   (map-solutions ',compiled-map-sol
	             (s::solution  ,(if p-vars `(list ,vars-name ,p-vars-name)
                                                  vars-name)
	             (s::reorder ,(case cost-function
	                                (0 `#'s::domain-size)
	                                (1 `#'s::range-size)
	                                (2 `#'s::score-position)
					)
	                       ,(case terminate?
	                            (0 `#'(lambda (x) (declare (ignore x)) nil))
	                            (1 `#'(lambda (x) (< x 1e-6)))
				    (2 `#'(lambda (x) (apply ',terminate?-fn (list x)))))
	                       ,(case order
	                            (0 `#'>)
	                            (1 `#'<))
	                       ,(case reorder-force
	                            (0 `#'s::linear-force)
	                            (1 `#'s::divide-and-conquer-force)
                                 (2 `#'s::random-force)))))))

	      (3  `(s::n-values ,n-ith-value
		   (map-solutions ',compiled-map-sol
	               (s::solution  ,(if p-vars `(list ,vars-name ,p-vars-name)
                                                  vars-name)
	             (s::reorder ,(case cost-function
	                                (0 `#'s::domain-size)
	                                (1 `#'s::range-size)
	                                (2 `#'s::score-position)
					)
	                       ,(case terminate?
	                            (0 `#'(lambda (x) (declare (ignore x)) nil))
	                            (1 `#'(lambda (x) (< x 1e-6)))
				    (2 `#'(lambda (x) (apply ',terminate?-fn (list x)))))
	                       ,(case order
	                            (0 `#'>)
	                            (1 `#'<))
	                       ,(case reorder-force
	                            (0 `#'s::linear-force)
	                            (1 `#'s::divide-and-conquer-force)
                                    (2 `#'s::random-force)))))))

	      (4  `(s::ith-value ,n-ith-value
		   (map-solutions ',compiled-map-sol
	               (s::solution  ,(if p-vars `(list ,vars-name ,p-vars-name)
                                                  vars-name)
	             (s::reorder ,(case cost-function
	                                (0 `#'s::domain-size)
	                                (1 `#'s::range-size)
	                                (2 `#'s::score-position)
					)
	                       ,(case terminate?
	                            (0 `#'(lambda (x) (declare (ignore x)) nil))
	                            (1 `#'(lambda (x) (< x 1e-6)))
				    (2 `#'(lambda (x) (apply ',terminate?-fn (list x)))))
	                       ,(case order
	                            (0 `#'>)
	                            (1 `#'<))
	                       ,(case reorder-force
	                            (0 `#'s::linear-force)
	                            (1 `#'s::divide-and-conquer-force)
                                 (2 `#'s::random-force)))))))
	      (5  `(s::best-value
			   (map-solutions ',compiled-map-sol
	            (s::solution ,(if p-vars `(list ,vars-name ,p-vars-name)
                                                  vars-name)
	             (s::reorder ,(case cost-function
	                                (0 `#'s::domain-size)
	                                (1 `#'s::range-size)
	                                (2 `#'s::score-position)
					)
	                       ,(case terminate?
	                            (0 `#'(lambda (x) (declare (ignore x)) nil))
	                            (1 `#'(lambda (x) (< x 1e-6)))
				    (2 `#'(lambda (x) (apply ',terminate?-fn (list x)))))
	                       ,(case order
	                            (0 `#'>)
	                            (1 `#'<))
	                       ,(case reorder-force
	                            (0 `#'s::linear-force)
	                            (1 `#'s::divide-and-conquer-force)
                                 (2 `#'s::random-force)))))
			   ,(cond ((not (null form2))
	  			      `(apply ',(first compiled-forms) ,(if p-vars `(list ,vars-name ,p-vars-name)
                                                  `(list ,vars-name)) )
	                             `(apply ',(second compiled-forms) ,(if p-vars `(list ,vars-name ,p-vars-name)
                                                  `(list ,vars-name)) ))
		              (t `(apply ',compiled-forms ,(if p-vars `(list ,vars-name ,p-vars-name)
                                                  `(list ,vars-name)) )))))
	       ))))
	(scode ;==> SOLVER CODE
	 (if count-fail? ;===> COUNT FAILURES ON
	 `(s::count-scs-failures
	   (let* ((,vars-name ,(reclist-vars vars))

	            (,p-vars-name ,(if (functionp compiled-p-vars)
                                   `(apply ',compiled-p-vars (list ,vars-name))
                                   `(mapcar #'(lambda (cs) (apply cs (list ,vars-name))) ',compiled-p-vars))))
	         ,constraints

	         ,p-constraints

		     ,constraints-all

	         ,solution-code))

	   `(let* ((,vars-name ,(reclist-vars vars));===> COUNT FAILURES OFF

	            (,p-vars-name ,(if (functionp compiled-p-vars)
                                   `(apply ',compiled-p-vars (list ,vars-name))
                                   `(mapcar #'(lambda (cs) (apply cs (list ,vars-name))) ',compiled-p-vars))))
	         ,constraints

	         ,p-constraints

		     ,constraints-all

	         ,solution-code)))
		)

;(print scode) ;<== FOR DEBUG

  (let ((scs-function (compile nil `(lambda ()
	  						  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
		                            ,scode))) ;==> COMPILED SOLVER FUNCTION
        (scs-time (om-timing-start)))

 (print "Timing evaluation of screamer-solver...")

  (let ((solution (select-solution out (funcall scs-function) (if (member val '(1 3 5)) val nil) p-vars))) ;==> GET THE SOLUTION
   (om-timing-stop scs-time)

   
   solution
	)
   )
  )
 )

(defun reclist-vars (vars)
 (labels ((reclist (x)
           (cond ((atom x) x)
                 ((and (listp x) (every #'atom x))
		       `(list ,.x))
		       (t `(list ,.(mapcar #'reclist x))))))
  (reclist vars)))
  
(defun map-solutions (funs args)
 (cond ((null funs) args)
       ((atom funs)
        (let ((results nil))
        (setf results (apply funs (list args)))
        (if (screamer::booleanp results)
             args
             results))) 
       ((listp funs)
        (let ((results nil))
	 (loop for fun in funs
               if (null results)
               do (if fun 
                      (let ((new-results (apply fun (list args))))
                       (if (screamer::booleanp new-results)
                            (setf results args)
                            (setf results new-results)))
                      (setf results args))
               else
               do (when fun 
                   (let ((new-results (apply fun (list results))))
                    (when (not (screamer::booleanp new-results))
                     (setf results new-results))))
               finally (return results))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OUTPUT OPTIONS

(defun select-solution (out solution &optional valuation p-variables?)
(if valuation
    (cond ((or (= valuation 1) (= valuation 3)) ;<== all-values or n-values
               (cond ((null out) (if p-variables? (first (mat-trans solution)) solution))
                     ((and (not (null out)) (listp out))
                      (progn (om-message-dialog "List of positions as <OUTPUT> is not supported in ALL-VALUES or N-VALUES.")
                             (om-abort)))
              (t solution)))

	      ((equal valuation 5) ;<== best-value
            (x-append (select-solution out (first solution) nil p-variables?)
                      (second solution)))
           (t (error "This shouldn't happen!")))

       (cond ((null out) (if p-variables? (first solution) solution))
                  ((and (symbolp out) (equal out :all)) solution)
                  ((and (not (null out))  (listp out) p-variables?)
                   (let ((variables
                           (cond ((null (first out)) nil)
                                 ((atom (first solution)) (first solution))
                                 ((equal (first out) :all) (first solution))
                                 ((listp (first out)) (posn-match (first solution) (first out)))
                                 (t nil)))
                         (p-variables
                          (cond ((null (second out)) nil)
                                ((atom (second solution)) (second solution))
                                ((equal (second out) :all) (second solution))
                                ((listp (second out)) (posn-match (second solution) (second out)))
                                (t nil))))
                     (if variables
                         (if p-variables (list variables p-variables) variables)
                       p-variables)))
                 ((and (not (null out)) (listp out) (null p-variables?))
                   (posn-match solution out))
                  (t (progn (om-message-dialog "ERROR: the <OUTPUT> should be a symbol :all or nil, or a list with two symbols [ex. (:all nil)] or a list containing two lists of positions [ex. ((0 2) (1 3))]")
                           (om-abort))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORCE-FUNCTION

(defmethod! force-function ((force-function string)
                             &key (cost-function "domain-size")
	                              (terminate? "(declare (ignore x))")
	                              (order ">" )
	                              (reorder-force-function "linear-force"))

  :initvals '("static-ordering linear-force" "domain-size" "(declare (ignore x))" ">" "linear-force")
  :indoc '("ordering-force-functions" "domain-size, range-size or score-position" "terminate-function" "> or <" "linear-force, divide-and-conquer-force or random-force")
  :doc "Screamer ordering and force-functions"
  :menuins '((0 (("static-ordering linear-force" "static-ordering linear-force")
                 ("static-ordering divide-and-conquer-force" "static-ordering divide-and-conquer-force")
                 ("static-ordering random-force" "static-ordering random-force")
                 ("reorder" "reorder" )))

            (1 (("domain-size" "domain-size")
                ("range-size" "range-size")
                ("score-position" "score-position")
               ))

            (2 (("(declare (ignore x))"  "(declare (ignore x))" )
                ("(< x 1e-6)"  "(< x 1e-6)" ) ))

            (3 ((">" ">")
                ("<" "<"))
            )

            (4 (("linear-force" "linear-force")
                ("divide-and-conquer-force" "divide-and-conquer-force")
                ("random-force" "random-force")))
             )
  :icon 486
(if (or (equal force-function "static-ordering linear-force")
        (equal force-function "static-ordering divide-and-conquer-force")
        (equal force-function "static-ordering random-force"))
    force-function
    (list force-function
          cost-function
          terminate?
          order
          reorder-force-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCREAMER-DOCUMENTATION

(defmethod! screamer-doc ((function t))
  :initvals '("solution")
  :indoc '("package::function-name")
  :doc "Shows Screamer's documentation of a function."
  :menuins '((0 (("solution" "solution")
                        ("static-ordering" "static-ordering")
                        ("reorder" "reorder" )
                        ("linear-force" "linear-force" )
                        ("divide-and-conquer-force" "divide-and-conquer-force" )
                        ("domain-size" "domain-size")
                        ("range-size" "range-size" )))
                   )
  :icon 486
(om-show-reference-doc
 (cond
  ((equal function "solution") 's::solution)
  ((equal function "static-ordering") 's::static-ordering)
  ((equal function "reorder") 's::reorder )
  ((equal function "linear-force") 's::linear-force )
  ((equal function "divide-and-conquer-force") 's::divide-and-conquer-force )
  ((equal function "domain-size") 's::domain-size)
  ((equal function "range-size") 's::range-size )
  ((equal function "assert!") 's::assert!)
  ((equal function "best-value") 's::best-value)
(t function))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VARIABLES GENERATORS

(defmethod! screamer-variable ((var string) &optional args)
  :initvals '("an-integer-betweenv" (0 10))
  :indoc '("screamer-variable" "list")
  :doc "Screamer variable"
  :menuins '((0 (("an-integer-betweenv" "an-integer-betweenv")
                 ("a-member-ofv" "a-member-ofv")
                 ("a-random-member-ofv" "a-random-member-ofv")
                 ("an-integerv" "an-integerv")
                 ("an-integer-abovev" "an-integer-abovev")
                 ("an-integer-belowv" "an-integer-belowv")
                 ("a-realv" "a-realv")
                 ("a-real-abovev" "a-real-abovev")
                 ("a-real-belowv" "a-real-belowv")
                 ("a-real-betweenv" "a-real-betweenv")
                 ("a-numberv" "a-numberv")
                 ("a-booleanv" "a-booleanv")
                  ))
                   )
  :icon 486
  (cond
    ((equal var "an-integer-betweenv") (s::an-integer-betweenv (first args) (second args)))
    ((equal var "a-member-ofv") (s::a-member-ofv args))
    ((equal var "a-random-member-ofv") (om?::a-random-member-ofv args))
    ((equal var "a-booleanv") (s::a-booleanv))
    ((equal var "an-integerv") (s::an-integerv))
    ((equal var "an-integer-abovev") (s::an-integer-abovev (first args)))
    ((equal var "an-integer-belowv") (s::an-integer-belowv (first args)))
    ((equal var "a-realv") (s::a-realv))
    ((equal var "a-real-abovev")  (s::a-real-abovev (first args)))
    ((equal var "a-real-belowv") (s::a-real-belowv (first args)))
    ((equal var "a-real-betweenv") (s::a-real-betweenv (first args) (second args)))
    ((equal var "a-numberv") (s::a-numberv))))

(defmethod! list-ofvs ((n-vars integer) (variables string) &optional args)
  :initvals '(3 "an-integer-betweenv" (0 10))
  :indoc '("integer" "screamer-variable" "list")
  :doc "List of screamer variables"
  :menuins '((1 (("an-integer-betweenv" "an-integer-betweenv")
                 ("a-member-ofv" "a-member-ofv")
                 ("a-random-member-ofv" "a-random-member-ofv")
                 ("an-integerv" "an-integerv")
                 ("an-integer-abovev" "an-integer-abovev")
                 ("an-integer-belowv" "an-integer-belowv")
                 ("a-realv" "a-realv")
                 ("a-real-abovev" "a-real-abovev")
                 ("a-real-belowv" "a-real-belowv")
                 ("a-real-betweenv" "a-real-betweenv")
                 ("a-numberv" "a-numberv")
                 ("a-booleanv" "a-booleanv")
                  ))
                   )
  :icon 486
(make-lists-of-variables n-vars variables args))

(defmethod! list-of-lists-ofvs ((n-vars list) (variables string) &optional args)
  :initvals '((3 4 3) "an-integer-betweenv" (0 10))
  :indoc '("list" "screamer-variable" "list")
  :doc "List of lists of screamer variables"
  :menuins '((1 (("an-integer-betweenv" "an-integer-betweenv")
                 ("a-member-ofv" "a-member-ofv")
                 ("a-random-member-ofv" "a-random-member-ofv")
                 ("an-integerv" "an-integerv")
                 ("an-integer-abovev" "an-integer-abovev")
                 ("an-integer-belowv" "an-integer-belowv")
                 ("a-realv" "a-realv")
                 ("a-real-abovev" "a-real-abovev")
                 ("a-real-belowv" "a-real-belowv")
                 ("a-real-betweenv" "a-real-betweenv")
                 ("a-numberv" "a-numberv")
                 ("a-booleanv" "a-booleanv")
                  ))
                   )
  :icon 486
(mapcar #'(lambda (x)
(make-lists-of-variables x variables args)) n-vars))

(defun make-lists-of-variables (n var &optional args)
 (cond
   ((equal var "an-integer-betweenv") (om?::list-of-integers-betweenv n (first args) (second args)))
   ((equal var "a-member-ofv") (om?::list-of-members-ofv n args))
   ((equal var "a-random-member-ofv") (om?::list-of-random-members-ofv n args))
   ((equal var "a-booleanv") (om?::list-of-booleansv n))
   ((equal var "an-integerv") (om?::list-of-integersv n))
   ((equal var "an-integer-abovev") (om?::list-of-integers-abovev n (first args)))
   ((equal var "an-integer-belowv") (om?::list-of-integers-belowv n (first args)))
   ((equal var "a-realv") (om?::list-of-realsv n))
   ((equal var "a-real-abovev")  (om?::list-of-reals-abovev n (first args)))
   ((equal var "a-real-belowv") (om?::list-of-reals-belowv n (first args)))
   ((equal var "a-real-betweenv") (om?::list-of-reals-betweenv n (first args) (second args)))
   ((equal var "a-numberv") (om?::list-of-numbersv n))))

(defmethod! list-of-chords-inv ((n-chords list) (domain list) &optional random?)
  :initvals '((3 4 3) (6000 6200 6400 6500 6700 6900 7100 7200) nil)
  :indoc '("list" "list" "t or nil")
  :menuins '((2 (("nil" 'nil)  ("t" 't))))
  :doc "Generates a list of screamer variables interpeted as CHORDS, that is a list with members of domain with no duplications and in ascending order."
  :icon 486
 (om?::list-of-chords-inv n-chords domain random?))

