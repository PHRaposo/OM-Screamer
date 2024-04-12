(in-package :screamer)

(defvar *all-screamer-score-variables* nil
 "A global variable storing the order which variables (and non-variables) will appear in the musical score representation 
of screamer-score (Openmusic). This is used for score-order (new cost-function).") ;<== phraposo
			   	
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
		
 