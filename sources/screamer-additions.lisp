(in-package :screamer)

(defvar-compile-time *all-screamer-score-variables* nil
 "A global variable storing the order which variables (and non-variables) will appear in the musical score representation 
of screamer-score (Openmusic). This is used for score-position (new cost-function).")

;(defvar-compile-time *variables-percent* nil
 ;"PERCENTAGE") ;<== FOR PROGRESS-BAR
 
  (defun random-force (x)
  "Returns X if it is not a variable. If X is a bound variable then returns
  its value.

  If X is an unbound variable then it must be known to have a countable set of
  potential values. In this case X is nondeterministically restricted to be
  equal to a random value in this countable set, thus forcing X to be bound.
  The dereferenced value of X is then returned.

  An unbound variable is known to have a countable set of potential values
  either if it is known to have a finite domain or if it is known to be integer
  valued.

  An error is signalled if X is not known to have a finite domain and is not
  known to be integer valued.

  Upon backtracking X will be bound to each potential value in turn, failing
  when there remain no untried alternatives.

  Since the set of potential values is required only to be countable, not
  finite, the set of untried alternatives may never be exhausted and
  backtracking need not terminate. This can happen, for instance, when X is
  known to be an integer but lacks either an upper of lower bound.

  The order in which the nondeterministic alternatives are tried is left
  unspecified to give future implementations leeway in incorporating heuristics
  in the process of determining a good search order."
   (let ((variable (value-of x)))
     (if (variable? variable)
         (restrict-value!
          variable
          (cond ((not (eq (variable-enumerated-domain variable) t))
                 (a-random-member-of (variable-enumerated-domain variable)))
                ((variable-integer? variable)
                 (if (variable-lower-bound variable)
                     (if (variable-upper-bound variable)
                         (an-integer-between
                          (variable-lower-bound variable)
                          (variable-upper-bound variable))
                         (an-integer-above (variable-lower-bound variable)))
                     (if (variable-upper-bound variable)
                         (an-integer-below (variable-upper-bound variable))
                         (an-integer))))
                (t (error "It is only possible to random force a variable that~%~
                         has a countable domain"))))))
   (value-of variable))
	   		  			   	
(defun score-position (x) 
  "This function can replace domain-size or random-size as a new cost-function for music constraints.
  Returns the position of X in the musical score representation of screamer-score (Openmusic)."
 (position x *all-screamer-score-variables*))
 	  
(defmacro-compile-time count-scs-failures (&body body)
"Failure count for screamer-solver (OpenMusic). Prints a message for each 1,000,000 failures."
 (let ((values (gensym "VALUES-")))
  `(let ((failure-count 0))
     (when-failing ((incf failure-count) 
      (when (integerp (/ failure-count 1000000)) (print (format nil "Number of failures: ~:d." failure-count)))) 
       (let ((,values (multiple-value-list (progn ,@body))))
          (print (format nil "
------------------------------------- 
Failures    =    ~:d
------------------------------------- " failure-count) )        
        (values-list ,values))))))
		
 