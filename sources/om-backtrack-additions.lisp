(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCREAMER-ORDERING-FORCE-FUNCTIONS
;;; (SCREAMER::STATIC-ORDERING, REORDER, LINEAR-FORCE, DIVIDE-AND-CONQUER-FORCE, DOMAIN-SIZE, RANGE-SIZE) 

(defclass screamer-ordering-boxes (OMBoxCall) () 
   (:documentation "Screamer ORDERING box"))

(defmethod screamer-ordering-boxes-p ((self screamer-ordering-boxes)) t)
(defmethod screamer-ordering-boxes-p ((self t)) nil)
			 
(defmethod omNG-box-value ((self screamer-ordering-boxes) &optional (numout 0))
   "Eval the output indexed by 'numout' for the box 'self'. In this method we call the generic function reference of 'self'."
   (handler-bind ((error #'(lambda (c)
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " " 
                                                                               (om-report-condition c)
                                                                               )
                                               :size (om-make-point 300 200))
                               (om-abort)))))
     (cond
      ((equal (allow-lock self) "l") 
       (setf (value self) (list (special-lambda-value self (intern (string (reference self)) :s)))) ;;;test with :s package
       (car (value self)))
       ((equal (allow-lock self) "#")
        (setf (value self) (list (eval `(function ,(intern (string (reference self)) :s))))))
      ((or ;(equal (allow-lock self) "l") 
           (equal (allow-lock self) "o")  
           (and (equal (allow-lock self) "x") (value self)) 
           (and (equal (allow-lock self) "&") (ev-once-p self))) (call-next-method))
      (t (let* ((args  (loop for input in (inputs self)
                             when (not (keyword-input-p input)) collect (omNG-box-value input)))
                (qargs (loop for val in args collect (if (or (symbolp val) (omlistp val)) `',val val))) 
                (themethod (compute-applicable-methods (fdefinition (reference self)) args)) rep)
           (if (null themethod)
             (progn (dialog-message (string+ "no method is defined for inputs in box " (name self)))
                    (abort))
             (progn
               (when (and (EditorFrame (car themethod)) (not (compiled? (car themethod))))
                 (modify-genfun (EditorFrame (car themethod))))
               
			   (setf rep (multiple-value-list (eval `(,(intern (string (reference self)) :s) ,.qargs)))))	
            )
           (when (equal (allow-lock self) "&")
             (setf (ev-once-p self) t)
             (setf (value self) rep))
           (when (equal (allow-lock self) "x")
             (setf (value self) rep))			 
          (progn (setf (value self) rep) ;;; new for om-backtrack in OM 7.2
               (nth numout rep))))))
             )
			 
(defmethod gen-code-call ((self screamer-ordering-boxes) &optional args)
 (let ((screamerfun `,(intern (string (reference self)) :s)))     
  `(,screamerfun ,.(decode self))))

  (defmethod gen-code ((self screamer-ordering-boxes) numout)
     "Generate Lisp code for the box 'self'."
     (let ((screamerfun `,(intern (string (reference self)) :s)))
       (cond
        ((equal (allow-lock self) "&") 
         (gen-code-for-ev-once self numout))
        ((equal (allow-lock self) "x")
         `(nth ,numout ,(gen-code (value self) 0)))
        ((equal (allow-lock self) "o") 
         `',(reference self))
        ((equal (allow-lock self) "l")
         (curry-lambda-code self screamerfun))
         ((equal (allow-lock self) "#")
           (setf (value self) `(function ,screamerfun)))			 		 
        (t  `(,screamerfun ,.(decode self))))))

(defclass screamer-force-function-boxes (OMBoxCall) () 
   (:documentation "Screamer FORCE FUNCTION box"))

(defmethod screamer-force-function-boxes-p ((self screamer-force-function-boxes)) t)
(defmethod screamer-force-function-boxes-p ((self t)) nil)
	 
(defmethod omNG-box-value ((self screamer-force-function-boxes) &optional (numout 0))
   "Eval the output indexed by 'numout' for the box 'self'. In this method we call the generic function reference of 'self'."
   (handler-bind ((error #'(lambda (c)
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " " 
                                                                               (om-report-condition c)
                                                                               )
                                               :size (om-make-point 300 200))
                               (om-abort)))))
     (cond
      ((equal (allow-lock self) "l") 
       (setf (value self) (list (special-lambda-value self (intern (string (reference self)) :s)))) ;;;test with :s package
       (car (value self)))
       ((equal (allow-lock self) "#")
        (setf (value self) (list (eval `(function ,(intern (string (reference self)) :s))))))
      ((or ;(equal (allow-lock self) "l")  
           (equal (allow-lock self) "o")  
           (and (equal (allow-lock self) "x") (value self)) 
           (and (equal (allow-lock self) "&") (ev-once-p self))) (call-next-method))
      (t (let* ((args  (loop for input in (inputs self)
                             when (not (keyword-input-p input)) collect (omNG-box-value input)))
                (qargs (loop for val in args collect (if (or (symbolp val) (omlistp val)) `',val val))) 
                (themethod (compute-applicable-methods (fdefinition (reference self)) args))
				 rep)
				 
           (if (null themethod)
             (progn (dialog-message (string+ "no method is defined for inputs in box " (name self)))
                    (abort))
             (progn
               (when (and (EditorFrame (car themethod)) (not (compiled? (car themethod))))
                 (modify-genfun (EditorFrame (car themethod))))
       
			   (setf rep (multiple-value-list (eval `(,(intern (string (reference self)) :s) ,.qargs)))))
						  
            )
           (when (equal (allow-lock self) "&")
             (setf (ev-once-p self) t)
             (setf (value self) rep))
           (when (equal (allow-lock self) "x")
             (setf (value self) rep))	 
          (progn (setf (value self) rep) ;;; new for om-backtrack in OM 7.2
               (nth numout rep))))))
             )
	 
(defmethod gen-code-call ((self screamer-force-function-boxes) &optional args)
 (let ((screamerfun `,(intern (string (reference self)) :s)))  
        `(,screamerfun ,.(decode self))   
	 ))

  (defmethod gen-code ((self screamer-force-function-boxes) numout)
     "Generate Lisp code for the box 'self'."
     (let ((screamerfun `,(intern (string (reference self)) :s)))
       (cond
        ((equal (allow-lock self) "&") 
         (gen-code-for-ev-once self numout))
        ((equal (allow-lock self) "x")
         `(nth ,numout ,(gen-code (value self) 0)))
        ((equal (allow-lock self) "o") 
         `',(reference self))
        ((equal (allow-lock self) "l")
         (curry-lambda-code self screamerfun))
         ((equal (allow-lock self) "#")
           (setf (value self) `(function ,screamerfun)))		 
        (t `(,screamerfun ,.(decode self))  
	    ))))

;; ========================================================== ;;
;; CONDV BOX
     
(defclass condv-box (OMBoxCall) () 
   (:documentation "CONDV box"))

(defmethod condv-box-p ((self condv-box)) t)
(defmethod condv-box-p ((self t)) nil)
                                
(defmethod omNG-box-value ((self condv-box) &optional (numout 0))
   "Eval the output indexed by 'numout' for the box 'self'. In this method we call the generic function reference of 'self'."
   (handler-bind ((error #'(lambda (c)
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " " 
                                                                               (om-report-condition c)
                                                                               )
                                               :size (om-make-point 300 200))
                               (om-abort)))))
(cond
      ((equal (allow-lock self) "l")
  	   (progn (om-message-dialog (format nil "~S DOES NOT WORK IN LAMBDA MODE." (string (reference self))))
  	          (om-abort))) 
      ((or (equal (allow-lock self) "o")  
           (and (equal (allow-lock self) "x") (value self)) 
           (and (equal (allow-lock self) "&") (ev-once-p self))) (call-next-method))
      (t (let* ((theinputs (loop for i in (inputs self) ;<=== FROM OMOut (gen-code method -> in-out-boxes.lisp)
	                                 collect (connected? i)))
			    themethod code qargs form rep)
				
				(setf code (loop for box in theinputs
				            collect (if box (gen-code (first box) (second box)) 'nil)))				   					 
	            (setf qargs (loop for val in code collect (if (or (symbolp val) (omlistp val)) `',val val)))

				(setf themethod (compute-applicable-methods (fdefinition (reference self)) qargs))
	            (if (null themethod)
	                (progn (dialog-message (string+ "no method is defined for inputs in box " (name self)))
	                       (om-abort))
                    (progn
                     (when (and (EditorFrame (car themethod)) (not (compiled? (car themethod))))
                      (modify-genfun (EditorFrame (car themethod))))
					(setf form `(,(intern (string (reference self)) :screamer+) ,.(mapcar #'cdr qargs)))		   
			   ;(print form); <=== check code
			   (setf rep (multiple-value-list (eval form))))
			)
           (when (equal (allow-lock self) "&")
             (setf (ev-once-p self) t)
             (setf (value self) rep))
           (when (equal (allow-lock self) "x")
             (setf (value self) rep))
          (progn (setf (value self) rep)
               (nth numout rep))))))
             )
	 
(defmethod gen-code-call ((self condv-box) &optional args)
 (let ((screamerfun `,(intern (string (reference self)) :screamer+)))
        `(,screamerfun ,.(mapcar #'cdr (decode self)))
	 ))

  (defmethod gen-code ((self condv-box) numout)
     "Generate Lisp code for the box 'self'."
     (let ((screamerfun `,(intern (string (reference self)) :screamer+)))
       (cond
        ((equal (allow-lock self) "&") 
         (gen-code-for-ev-once self numout))
        ((equal (allow-lock self) "x")
         `(nth ,numout ,(gen-code (value self) 0)))
        ((equal (allow-lock self) "o") 
         `',(reference self))
        ((equal (allow-lock self) "l")
         (progn (dialog-message (string+ "no method is defined for inputs in box " (name self)))
	             (om-abort)))
         ((equal (allow-lock self) "#")
           (setf (value self) `(function ,screamerfun)))		 
        (t `(,screamerfun ,.(mapcar #'cdr (decode self)))
	    ))))
     
;; ========================================================== ;;
;; TODO: (IF NEEDED)
#|
(defmethod special-lambda-value ((self screamerboxes) symbol)
 "Eval a screamerbox in lambda mode."
 (multiple-value-bind (nesymbs args) (get-args-eval-currry self)
  (eval `#'(lambda ,(reverse nesymbs)
            (,symbol ,.args)))))
										 
(defmethod curry-lambda-code ((self screamerboxes) symbol)
    "Lisp code generetion for a box in lambda mode."

    (let ((nesymbs nil)
          (oldlambdacontext *lambda-context*))
      (setf *lambda-context* t)

      (unwind-protect 
          (let ((args (mapcan #'(lambda (input)
                                  (let ((a (if (connected? input)
                                               (gen-code input 0)
                                             (let ((newsymbol (gensym)))
                                               (push newsymbol nesymbs)
                                               newsymbol))))
                                    (if (keyword-input-p input) 
                                        (list (value input) a) 
                                      (list a))))
                              (inputs self))))
							  				 
            `#'(lambda ,(reverse nesymbs)
                (,symbol ,.args)))

        (setf *lambda-context* oldlambdacontext)
       
     )))
|#
;; ========================================================== ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SOLUTION - ORDERING-FORCE-FUNCTIONS

(defmethod get-boxcallclass-fun ((self (eql 'best-value))) 'screamer-valuation-boxes)
(defmethod get-real-funname ((self (eql 'best-value))) self)
(defmethod! best-value ((form1 t) (objective-form t) &optional (form2? nil))
:initvals '(nil nil nil)
:indoc '("integer" "forms" "fail")
:doc "OM equivalent of SCREAMER:BEST-VALUE macro." 
:icon 486 
(screamer::best-value form1 objective-form form2?))

(defmethod get-boxcallclass-fun ((self (eql 'solution))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'solution))) self)
(defmethod! solution ((x t) (force-function t))
:initvals '(nil nil)
:indoc '("X" "ordering force function")
:doc "OM version of SCREAMER:SOLUTION." 
:icon 486 
(screamer::solution x force-function))

(defmethod get-boxcallclass-fun ((self (eql 'static-ordering))) 'screamer-ordering-boxes)
(defmethod get-real-funname ((self (eql 'static-ordering))) self)
(defmethod! static-ordering ((force-function t))
:initvals '(nil)
:indoc '("force function")
:doc "OM version of SCREAMER:STATIC-ORDERING." 
:icon 486 
(screamer::static-ordering force-function))

(defmethod get-boxcallclass-fun ((self (eql 'linear-force))) 'screamer-force-function-boxes)
(defmethod get-real-funname ((self (eql 'linear-force))) self)
(defmethod! linear-force ((variable t))
 :initvals '(nil t)
 :indoc '("variable")
 :doc "OM version of SCREAMER:LINEAR-FORCE." 
 :icon 486 
 (screamer::linear-force variable))

(defmethod get-boxcallclass-fun ((self (eql 'divide-and-conquer-force))) 'screamer-force-function-boxes)
(defmethod get-real-funname ((self (eql 'divide-and-conquer-force))) self)
(defmethod! divide-and-conquer-force ((variable t))
 :initvals '(nil t)
 :indoc '("variable")
 :doc "OM version of SCREAMER:LINEAR-FORCE." 
 :icon 486
 (screamer::divide-and-conquer-force variable))
 
 (defmethod get-boxcallclass-fun ((self (eql 'random-force))) 'screamer-force-function-boxes)
 (defmethod get-real-funname ((self (eql 'random-force))) self)
 (defmethod! random-force ((variable t))
  :initvals '(nil t)
  :indoc '("variable")
  :doc "SCREAMER:RANDOM-FORCE: new experimental force function." 
  :icon 486
  (screamer::random-force))

(defmethod get-boxcallclass-fun ((self (eql 'reorder))) 'screamer-ordering-boxes)
(defmethod get-real-funname ((self (eql 'reorder))) self)
(defmethod! reorder ((cost-function t) (terminate? t ) (order t) (force-function t))
 :initvals '(nil nil nil nil)
 :indoc '("cost-function" "terminate?" "order" "force-function")
 :doc "OM version of SCREAMER:LINEAR-FORCE." 
 :icon 486
 (screamer::reorder cost-function terminate? order force-function))

(defmethod get-boxcallclass-fun ((self (eql 'domain-size))) 'screamer-force-function-boxes)
(defmethod get-real-funname ((self (eql 'domain-size))) self)
(defmethod! domain-size ((x t))
 :initvals '(nil)
 :indoc '("X")
 :doc "OM version of SCREAMER:DOMAIN-SIZE." 
 :icon 486
 (screamer::domain-size x))

(defmethod get-boxcallclass-fun ((self (eql 'range-size))) 'screamer-force-function-boxes)
(defmethod get-real-funname ((self (eql 'range-size))) self)
(defmethod! range-size ((x t))
 :initvals '(nil)
 :indoc '("X")
 :doc "OM version of SCREAMER:DOMAIN-SIZE." 
 :icon 486
 (screamer::range-size x))

(defmethod! order ((symb-fn t))
 :initvals '('>)
 :indoc '("symbol or string")
 :doc "Order argument for SCREAMER::REORDER = > (descending) or < (ascending)." 
 ;:menuins '((0 (('> '>) ('< '<))))
 :icon 486
 (eval `(function ,(cond ((stringp symb-fn) (read-from-string symb-fn))
			 ((symbolp symb-fn) symb-fn)
			  (t (progn (om-message-dialog "The ORDER argument should be a symbol or string.")
				    (om-abort)))))))

(defmethod get-boxcallclass-fun ((self (eql 'condv))) 'condv-box)
(defmethod get-real-funname ((self (eql 'condv))) self)
(defmethod! condv ((form t) &rest forms)
:initvals '(nil t)
:indoc '("form" "forms")
:doc "OM equivalent of CONDV macro." 
:icon 486 
(declare (ignore form forms)))

