
(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *screamer-score-midi-approx* nil)
(setf *screamer-score-midi-approx* nil)

(defvar *screamer-score-debug* nil)
(setf  *screamer-score-debug* nil)

(defvar *print-screamer-score-failures?* nil)
 (setf *print-screamer-score-failures?* nil)

(defun set-print-score-fails (bool)
 (setf *print-screamer-score-failures?* bool))

(defvar *print-screamer-score-time?* nil)
 (setf *print-screamer-score-time?* t)

(defvar *screamer-score-random?* nil)
(setf *screamer-score-random?* nil)

(defun set-screamer-score-print-time (bool)
 (setf *print-screamer-score-time?* bool))

(defun set-screamer-score-debug (bool)
 (setf *screamer-score-debug* bool)
 (if *screamer-score-debug*
     (progn 
         (setf *print-screamer-score-failures?* t)
         (setf *print-screamer-score-time?* t)
         "SCREAMER-SCORE DEBUG: ON.")
	 (progn 
		 (setf *print-screamer-score-failures?* nil)
                (setf *print-screamer-score-time?* nil)
                "SCREAMER-SCORE DEBUG: OFF.")))

;; IN PROGRESS

(defvar *screamer-score-notes* nil)
(setf  *screamer-score-notes* nil)

(defvar *screamer-score-chords* nil)
(setf *screamer-score-chords* nil)

(defvar *screamer-score-chords-on-beat* nil)
(setf *screamer-score-chords-on-beat* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPILE-CONSTRAINT

;;; RETRIEVE THE NAME OF THE PATCH IN LAMBDA MODE

(defun get-current-patch ()
(handler-bind ((error #'(lambda (c)
                          (when *msg-error-label-on*
                            (om-message-dialog (string+ "Error while evaluating the function " "get-current-patch" " : "
                                                     (om-report-condition c))
                                               :size (om-make-point 300 200))
                            (om-abort)))))
(let* ((interfaces (capi::collect-interfaces 'capi:interface :screen :any :sort-by :visible))
       (persist (remove nil (mapcar #'(lambda (x)
	                         (if (equal (class-name (class-of x)) 'editorwindow)
							     (obj x)))
				              interfaces))))
(car
 (loop for el in persist
	   when (patch-p el)
	   collect el)))))

(defun get-patch-boxes (patch)
(handler-bind ((error #'(lambda (c)
                          (when *msg-error-label-on*
                            (om-message-dialog (string+ "Error while evaluating the function " "get-patch-boxes" " : "
                                                     (om-report-condition c))
                                               :size (om-make-point 300 200))
                            (om-abort)))))
 (let* ((patchboxes (find-class-boxes (boxes patch) 'omboxabspatch))
	      (patches (if patchboxes (mapcar #'reference patchboxes) nil)))
		 (if patches
			 (list patchboxes (mapcar #'get-patch-boxes patches))
		      nil))))

(defun find-patch-from-lambda-fn (fn-symb patch-boxes)
 (car
  (loop for patchbox in patch-boxes
    when (equal fn-symb (symbol-name (code (reference patchbox))))
    collect patchbox)))

(defun get-patch-function-name (lambda-patch)
 (symbol-name (second (cadr (third (function-lambda-expression lambda-patch))))))
 
(defun find-lambda-patchbox (lambda-fn)
 (handler-bind ((error #'(lambda (c)
                       (when *msg-error-label-on*
                         (om-message-dialog (string+ "Error while evaluating the function " "find-lambda-patchbox" " : "
                                                  (om-report-condition c))
                                            :size (om-make-point 300 200))
                         (om-abort)))))
 (let ((expr (function-lambda-expression lambda-fn)))
  (cond  ((stringp (third expr)) (third expr))
         ((not (listp (cadr (third expr)))) (string (gensym "anon-fun-")))
         (t (find-patch-from-lambda-fn
             (get-patch-function-name lambda-fn)
              (remove nil (flat (get-patch-boxes (get-current-patch))))))))))

(defun compile-screamer-constraint (fun)
(handler-bind ((error #'(lambda (c)
                       (when *msg-error-label-on*
                         (om-message-dialog (string+ "Error while evaluating the function " "compile-screamer-constraint" " : "
                                                  (om-report-condition c))
                                            :size (om-make-point 300 200))
                         (om-abort)))))
 (let* ((expr (function-lambda-expression fun))
        (patchbox (find-lambda-patchbox fun))
        (patch-name (if (stringp patchbox);<== lambda function documentation
		                     patchbox 
                        (if (null patchbox);<== function in lambda mode
                            (symbol-name (second (cadr (third expr))))
                            (name (reference patchbox))))));<== patch in lambda mode
    (if (compiled-function-p fun)
         expr
        (compile (eval `(defun ,(gensym (if (null patch-name) "anon-fun-" (concatenate 'string patch-name "-"))) ,(function-lambda-list fun)
		                     (apply ,fun (list ,.(function-lambda-list fun))))))))))

;; BACKTRACK CONSTRAINTS (OLD)
(defun compile-screamer-backtrack-constraint (fun)
  (handler-bind ((error #'(lambda (c)
                            (when *msg-error-label-on*
                              (om-message-dialog (string+ "Error while evaluating the function "
                                                          "compile-screamer-backtrack-constraint"
                                                          " : "
                                                          (om-report-condition c))
                                                 :size (om-make-point 300 200))
                              (om-abort)))))
    (let* ((expr (function-lambda-expression fun))
           (patchbox (find-lambda-patchbox fun))
           (patch-name (if (stringp patchbox) ;<== lambda function documentation
                           patchbox
                           (if (null patchbox) ;<== function in lambda mode
                               (symbol-name (second (cadr (third expr))))
                               (name (reference patchbox))))) ;<== patch in lambda mode
           (lambda-list (loop for x from 1 to (length (function-lambda-list fun))
                             collect (intern (string (gensym)) :om))))
      (compile
       (eval
        `(defun ,(gensym (if (null patch-name)
                             "backtrack-anon-fun-"
                             (concatenate 'string patch-name "-backtrack-")))
           ,lambda-list (om?::any-fn ,fun ,@lambda-list)))))))

;; LIST-OF-LISTP / LIST-OF-LISTS
(defun list-of-listp (thing) (and (listp thing) (every #'listp thing)))
(deftype list-of-lists () '(satisfies list-of-listp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;TIME

(defun om-timing-start ()
  "Capture timing and resource stats at the start."
  (sys::start-gc-timing)
  (list (get-internal-run-time)
        (get-internal-real-time)
        (sys::total-allocation)
        (sys::get-gc-timing)
        (sys::get-universal-time)))

(defun om-timing-stop (start-data)
  "Collect and print timing/resource stats since START-DATA."
  (let* ((end-run (get-internal-run-time))
         (end-real (get-internal-real-time))
         (end-alloc (sys::total-allocation))
         (end-gc (let ((gc-timing (sys::get-gc-timing))) (sys::stop-gc-timing) gc-timing))
         (end-time (get-universal-time))
         (start-run (first start-data))
         (start-real (second start-data))
         (start-alloc (third start-data))
         (start-gc (fourth start-data))
         (start-clock (fifth start-data))
         (user-time (/ (- end-run start-run) internal-time-units-per-second))
         (elapsed-time (/ (- end-real start-real) internal-time-units-per-second))
         (allocation (- end-alloc start-alloc))
         (gc-time (- (getf end-gc :total 0.0) (getf start-gc :total 0.0)))
         (wall-time (- end-time start-clock)))
    (format *om-stream* "~%-------------------------------------~%")
    (format *om-stream* "   User time      = ~,3f s~%" user-time)
    (format *om-stream* "   Elapsed time   = ~,3f s~%" elapsed-time)
    (format *om-stream* "   Real Time      = ~A s~%" wall-time)
    (format *om-stream* "   Allocation     = ~:d bytes~%" allocation)
    (format *om-stream* "   GC time        = ~,3f s~%" gc-time)
    (format *om-stream* "-------------------------------------~%")))

;;;;

(defun get-next-chord (chord)
(let ((chord-pos (position chord *screamer-score-chords* :test #'equal)))
 (nth (1+ chord-pos) *screamer-score-chords*)))
 
(defun get-previous-chord (chord)
(let ((chord-pos (position chord *screamer-score-chords* :test #'equal)))
(if (minusp (1- chord-pos)) 
     nil
    (nth (1- chord-pos) *screamer-score-chords*))))
 
(defun get-neighbor-chord (chord n direction)
 (let ((chord-pos (position chord *screamer-score-chords*)))
  (if (string-equal "->" direction)
       (nth (+ n chord-pos) *screamer-score-chords*)
       (if (minusp (- chord-pos n))
           nil
          (nth (- chord-pos n) *screamer-score-chords*)))))
 
 (defun get-next-note (note)
  (let* ((voice-notes (loop for voice in *screamer-score-notes*
                           when (member note voice :test #'eq)
                           collect voice))
         (note-pos (position note voice-notes :test #'eq)))
   (nth (1+ note-pos) voice-notes)))                         
 
 (defun get-previous-note (note)
  (let* ((voice-notes (loop for voice in *screamer-score-notes*
                           when (member note voice :test #'eq)
                           collect voice))
         (note-pos (position note voice-notes :test #'eq)))
  (if (minusp (1- note-pos))
       nil
      (nth (1- note-pos) voice-notes))))

(defun get-neighbor-note (note n direction)
 (let* ((voice-notes (loop for voice in *screamer-score-notes*
                           when (member note voice :test #'eq)
                           collect voice))
         (note-pos (position note voice-notes :test #'eq)))
  (cond ((string-equal "->" direction)
         (nth (+ n note-pos) voice-notes))
        (t (if (minusp (- note-pos n))
                nil
               (nth (- note-pos n) voice-notes))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OM METHODS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mk-growing (vars);==> GROWING LIST ((0) (0 1) (0 1 2) ...)
 (let* ((list-length (length vars))
         (posn (loop for x from 0 to (1- list-length)
                            for y = (arithm-ser 0 x 1)
                            collect y)))
 (posn-match vars posn)))

(defun mk-car-cdr (vars);==> CARD-CDR ((0 (1 2 3 4 5)) (1 (2 3 4 5)) (2 (3 4 5)) ...)
 (let* ((apply-length (1- (length vars)))
	    (posn (loop for x from 0 to (1- apply-length)
          for y = (list x (arithm-ser (1+ x) apply-length 1))
 collect y)))
 (posn-match vars (reverse posn))))

(defun split-domain-list1 (list-length n-inputs voice-domain);==> N-INPUTS ((0 1) (1 2) (2 3) ...)
  (let* ((posn (loop for x from 0 to (- list-length n-inputs)
           for y = (arithm-ser x (+ x (1- n-inputs)) 1)
  collect y)))
 (posn-match voice-domain posn)))

; APPLY-CONTV

(defmethod! apply-contv ((cs function) (mode string) (recursive? string) (vars t))
  :initvals '(nil "atom" "off" nil)
  :indoc '("patch in lambda mode" "string" "string" "list of variables" )
  :menuins '((1 (("atom" "atom") ("list" "list")))
             (2 (("off" "off") ("n-inputs" "n-inputs") ("car-cdr" "car-cdr") ("growing" "growing"))))
  :doc "Applies constraint recursively to list of variables."
  :icon 486
  (handler-bind
      ((error #'(lambda (c)
                  (when *msg-error-label-on*
                    (om-message-dialog
                     (string+
                      "Error while evaluating the function "
                      (cond
                        ((stringp (third (function-lambda-expression cs)))
                         (third (function-lambda-expression cs)))
                        ((stringp (function-name cs))
                         (let ((patch? (find-lambda-patchbox cs)))
                           (if patch?
                               (name (reference patch?))
                               (get-patch-function-name cs))))
                        (t (symbol-name (function-name cs))))
                      " : "
                      (om-report-condition c))
                     :size (om-make-point 300 200))
                    (om-abort)))))
    (cond
      ((equal mode "atom")
       (om?::assert!-deep-mapcar cs cs vars))
      ((equal mode "list")
       (cond
         ((equal recursive? "n-inputs")
          (mapcar #'(lambda (x)
                      (screamer::assert!
                       (apply cs x)))
                  (split-domain-list1 (length vars) (length (function-lambda-list cs)) vars)))
         ((equal recursive? "car-cdr")
          (mapcar #'(lambda (x)
                      (screamer::assert!
                       (funcall cs (first x) (second x))))
                  (mk-car-cdr vars)))
         ((equal recursive? "growing")
          (mapcar #'(lambda (x)
                      (screamer::assert!
                       (funcall cs x)))
                  (mk-growing vars)))
         (t
          (screamer::assert!
           (apply cs (list vars))))))
      (t
       (progn
         (om-message-dialog "Error while evaluating the function APPLY-CONTV: Unknown mode.")
         (om-abort))))))

(defmethod! om-assert! ((x screamer::variable))
:initvals '( t ) 
:indoc '("boolean variable or list")
:doc "OM equivalent to SCREAMER::ASSERT!. Accepts one boolean variable or a list of boolean variables."
:icon 486
(screamer::assert! x))

(defmethod! om-assert! ((x list))
:initvals '( t ) 
:indoc '("boolean variable or list")
:doc "OM equivalent to SCREAMER::ASSERT!. Accepts one boolean variable or a list of boolean variables."
:icon 486
(mapcar #'om-assert! x))

 (defmethod! x->dxv ((list list))
 :initvals '((1 2 3 4 5)) :indoc '("variable or list of variables")
 :doc ""
 :icon 485
 (om?::x->dxv list))

 (defmethod! x->dxv ((listv screamer::variable))
 :initvals '((1 2 3 4 5)) :indoc '("variable or list of variables")
 :doc ""
 :icon 485
(?::mapcarv #'s::-v (?::cdrv listv) listv))

 (defmethod! rx->dxv ((listv screamer::variable))
 :initvals '((1 2 3 4 5)) :indoc '("variable or list of variables")
 :doc "Intervals in reverse order (from last to first)."
 :icon 485
(?::mapcarv #'s::-v (?::cdrv (s::funcallv #'reverse listv)) (s::funcallv #'reverse listv)))

 (defmethod! rx->dxv ((list list))
 :initvals '((1 2 3 4 5)) :indoc '("variable or list of variables")
 :doc  "Intervals in reverse order (from last to first)."
 :icon 485
 (om?::x->dxv (reverse list)))

 (defmethod! x->dx-absv ((list list))
 :initvals '((1 2 3 4 5)) :indoc '("variable or list of variables")
 :doc ""
 :icon 478
 (om?::x->dx-absv list))

 (defmethod! x->dx-absv ((listv screamer::variable))
 :initvals '((1 2 3 4 5)) :indoc '("variable or list of variables")
 :doc ""
 :icon 478
 (?::mapcarv #'(lambda (x y) (om?::absv (s::-v y x))) listv (?::cdrv listv)))

 (defmethod! dx->xv ((start number) (list list))
 :initvals '(0 (1 2 3 4 5)) :indoc '("variable or number" "variable, list of variables or list")
 :doc ""
 :icon 485
 (om?::dx->xv start list))

 (defmethod! dx->xv ((start number) (listv screamer::variable))
 :initvals '(0 (1 2 3 4 5)) :indoc '("variable or number" "variable, list of variables or list")
 :doc ""
 :icon 485
 (om?::dx->xv-listv start listv))

 (defmethod! not-intersectionv ((list1 list) (list2 list))
 :initvals '((0 2 4) (1 3 5)) :indoc '("list" "list")
 :doc ""
 :icon 477
 (s::=v (?::lengthv (?::intersectionv list1 list2)) 0))

 (defmethod! all-intervalsv ((var-list list))
 :initvals '(nil)
 :indoc '("list")
 :doc "Returns a list of screamer variables constrained to be the intervals between each variable with each other variable in list."
 :icon 485
 (if (= 1 (length (remove nil (flat var-list))))
      nil
  (let* ((flat-list (remove nil (flat var-list)))
  	     (positions (arithm-ser 0 (1- (length flat-list)) 1))
		 (all-comb-posn (om?::asc-permutations positions 2))
		 (all-perms (mapcar #'(lambda (x) (posn-match flat-list x)) all-comb-posn)))
 (mapcar #'(lambda (vars)
  (screamer::-v (first vars) (second vars)))
  all-perms))))
 
; -----------------------------------------
; OM+V / OM-V / OM*V / OM/V / MOD12V / MC->PCV / OM-ABSV

(defmethod! om-absv ((n number))
:initvals '(-8) :indoc '("variable, number or list")
:icon 480
(om?::absv n))

(defmethod! om-absv ((lst list))
(mapcar #'om?::absv lst))

(defmethod! om-absv ((var screamer::variable))
:icon 480
(if (s::variable-list? var)
    (?::mapcarv #'(lambda (x) (om?::absv x)) var)
    (om?::absv var)))
    

(defmethod! modv ((n integer) (d integer))
:initvals '(-8 12) :indoc '("variable, number or list" "integer")
:icon 480
(s::funcallv #'mod n d))

(defmethod! modv ((lst list) (d integer))
(mapcar #'(lambda (x) (modv x d)) lst))

(defmethod! modv ((var screamer::variable)(d integer))
:icon 480
(if (s::variable-list? var)
    (?::mapcarv #'(lambda (x) (s::funcallv #'mod x d)) var)
    (s::funcallv #'mod var d)))
   

(defmethod! mod12v ((n integer))
:initvals '(-8) :indoc '("variable, number or list")
:icon 480
(s::funcallv #'mod n 12))

(defmethod! mod12v ((lst list))
(mapcar #'mod12v lst))

(defmethod! mod12v ((var screamer::variable))
:icon 480
(if (s::variable-list? var)
    (let ((z (?::mapcarv #'(lambda (x) (s::funcallv #'mod x 12)) var)))
     z)
    (let ((z (s::funcallv #'mod var 12)))
      z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MIDI

(defmethod! m->pcv ((n integer))
:initvals '(60) :indoc '("variable, number or list")
:icon 479
(om?::a-midi->pcv n))

(defmethod! m->pcv ((n list))
:initvals '((60 64 67)) :indoc '("variable, number or list")
:icon 479
(mapcar #'m->pcv n))

(defmethod! m->pcv ((var screamer::variable))
:initvals '(60) :indoc '("variable, number or list")
:icon 479
 (if (s::variable-list? var)
     (?::mapcarv #'om?::a-midi->pcv var)
     (om?::a-midi->pcv var)))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
(defmethod! mc->pcv ((n integer))
:initvals '(6000) :indoc '("variable, number or list")
:icon 479
(om?::a-mc->pcv n))

(defmethod! mc->pcv ((n list))
:initvals '((6000 6400 6700)) :indoc '("variable, number or list")
:icon 479
(mapcar #'mc->pcv n))

(defmethod! mc->pcv ((var screamer::variable))
:initvals '(6000) :indoc '("variable, number or list")
:icon 479
 (if (s::variable-list? var)
     (?::mapcarv #'om?::a-mc->pcv var)
     (om?::a-mc->pcv var)))


(defmethod! all-membersv ((list list) (sequence list))
:initvals '((11 5 2) (0 2 4 5 7 9 11)) :indoc '("variable or list" "variable or list")
:icon 477
;(apply #'s::andv (mapcar #'(lambda (x) (s::memberv x sequence)) list)))
(om?::all-membersv list sequence))

(defmethod! all-membersv ((listv screamer::variable) (sequence list))
:initvals '((11 5 2) (0 2 4 5 7 9 11)) :indoc '("variable or list" "variable or list")
:icon 477
 (?::everyv #'(lambda (r)(s::equalv r 't))
  (?::mapcarv #'(lambda (x)
                (s::memberv x sequence))
   listv)))

(defmethod! all-membersv ((list list) (sequence screamer::variable))
:initvals '((11 5 2) (0 2 4 5 7 9 11)) :indoc '("variable or list" "variable or list")
:icon 477
 (?::everyv #'(lambda (r)(s::equalv r 't))
  (?::mapcarv #'(lambda (x)
                (s::memberv x sequence))
   list)))

(defmethod! om-memberv ((variable screamer::variable) (lst list))
:initvals '(nil nil) :indoc '("variable, number or list" "list")
:icon 477
 (screamer::memberv variable lst))

(defmethod! om-memberv ((variable number) (lst list))
:initvals '(nil nil) :indoc '("variable, number or list" "list")
:icon 477
 (not (null (member variable lst))))
 
 (defmethod! om-memberv ((variable screamer::variable) (lst screamer::variable))
 :initvals '(nil nil) :indoc '("variable, number or list" "list")
 :icon 477
  (screamer::memberv variable lst))
  
(defmethod! om-memberv ((variable number) (lst screamer::variable))
:initvals '(nil nil) :indoc '("variable, number or list" "list")
:icon 477
(screamer::memberv variable lst))
  
(defmethod! om-memberv ((variable list) (lst list))
 (mapcar #'(lambda (x)
  (screamer::memberv x lst))
 variable))

(defmethod! all-diffv ((list t))
:initvals '(nil) :indoc '("list")
:icon 477
(if (listp list)
    (s::apply #'s::/=v list)
    (s::funcallv #'(lambda (x) (apply #'/= x))))) 

(defmethod! sumv ((list t))
:initvals '(nil) :indoc '("list")
:icon 480
(?::sumv list))

(defmethod! list-equalv? ((l1 list) (l2 list))
:initvals '(nil nil) :indoc '("list" "list")
:icon 476
(om?::equalv-lists l1 l2))

(defmethod! om-make-equal ((x t) (y t))
:initvals '(nil nil) :indoc '("any" "any")
:icon 486
(?::make-equal x y)
 t)

(defmethod! omifv ((test t) (action t) &optional else)
   :numouts 1 
   :initvals '(nil nil nil) 
   :indoc '("IFV" "THENV" "ELSEV")
   :doc "IFV <test> THENV <action> ELSEV <else>." 
   :icon 486
 (screamer+::ifv test action else))
 
; -----------------------------------------

(defmethod! om*v ((arg1 t) (arg2 t))
:initvals '(0 0) :indoc '("variable, number or list" "variable, number or list")
:doc ""
:icon 483
(s::*v arg1 arg2))

(defmethod! om*v ((arg1 t) (arg2 list))
(mapcar #'(lambda (input)
            (om*v arg1 input)) arg2))

(defmethod! om*v ((arg1 list) (arg2 t))
(mapcar #'(lambda (input)
            (om*v  input arg2)) arg1))

; -----------------------------------------

(defmethod! om+v ((arg1 t) (arg2 t))
:initvals '(0 0) :indoc '("variable, number or list" "variable, number or list")
:doc ""
:icon 481
(s::+v arg1 arg2))

(defmethod! om+v ((arg1 t) (arg2 list))
(mapcar #'(lambda (input)
          (om+v arg1 input)) arg2))

(defmethod! om+v ((arg1 list) (arg2 t))
(mapcar #'(lambda (input)
          (om+v  input arg2)) arg1))

; -----------------------------------------

(defmethod! om-v ((arg1 t) (arg2 t))
:initvals '(0 0) :indoc '("variable, number or list" "variable, number or list")
:doc ""
:icon 484
(s::-v arg1 arg2))

(defmethod! om-v ((arg1 t) (arg2 list))
(mapcar #'(lambda (input)
          (om-v arg1 input)) arg2))

(defmethod! om-v ((arg1 list) (arg2 t))
(mapcar #'(lambda (input)
          (om-v  input arg2)) arg1))

; -----------------------------------------

(defmethod! om/v ((arg1 t) (arg2 t))
:initvals '(1 1) :indoc '("variable, number or list" "variable, number or list")
:doc ""
:icon 482
(s::/v arg1 arg2))

(defmethod! om/v ((arg1 t) (arg2 list))
(mapcar #'(lambda (input)
          (om/v arg1 input)) arg2))

(defmethod! om/v ((arg1 list) (arg2 t))
(mapcar #'(lambda (input)
          (om/v  input arg2)) arg1))

; -----------------------------------------

(defmethod! m->midic ((n t))
:initvals '(60) :indoc '("variable, number or list")
:icon 479
(om*v n 100))

(defmethod! midic->m ((n t))
:initvals '(6000) :indoc '("variable, number or list")
:icon 479
(om/v n 100))

; -----------------------------------------

; UPDATE FUNCTIONS-WITHOUT-NAME

(setf *function-without-name*
 (let ((defaults *function-without-name*))
  (x-append (list 'om*v 'om-v 'om+v 'om/v) defaults)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; BEZIER CURVES

(defmethod! quadratic-bezier ((p0 number) (p1 number) (p2 number) (steps integer))
 :initvals '(6000 4800 7400 20)
 :indoc '("number" "number" "number" "integer")
 :doc "Solve a quadratic Bezier curve with three points in a given number of steps."
 :icon 473
 (let* ((t-var (interpolation 0 1 steps 0.0))
        (q0 (mapcar #'(lambda (z)
                      (+ (* (expt (- 1 z) 2) p0)
                          (* (- 1 z) (* 2  z) p1)
                          (* (expt z 2) p2)))
                t-var)))
(simple-bpf-from-list (om* t-var 1000)
                                  q0)))

(defmethod! cubic-bezier ((p0 number) (p1 number) (p2 number) (p3 number) (steps integer))
 :initvals '(3600 2100 8400 6000 20)
 :indoc '("number" "number" "number" "number" "integer")
 :doc "Solve a cubic Bezier curve with four points in a given number of steps."
 :icon 473
 (let* ((t-var (interpolation 0 1 steps 0.0))
        (c0 (mapcar #'(lambda (z)
                      (+ (* (expt (- 1 z) 3) p0)
                          (* 3 (expt (- 1 z) 2) z p1)
                          (* 3 (- 1 z) (expt z 2) p2)
                          (* (expt z 3) p3)))
                t-var)))
(simple-bpf-from-list (om* t-var 1000)
                                  c0)))
								  
; VOICE-MERGER

(defun voice-merger-internal (list accumul)
(cond  ((null list) accumul)
           ((null accumul) (voice-merger-internal (cdr list) (car list)))
           (t  (voice-merger-internal (cdr list) (merger (car list) accumul)))))

(defmethod! voice-merger ((objs list))
   :initvals '(nil)
   :indoc '("list of voices")
   :icon 253
   :doc "Merges a list of voices into a new voice object."
(voice-merger-internal objs nil))

(defmethod! voice-merger ((self om::poly))
   :initvals '(nil)
   :indoc '("poly object")
   :icon 253
   :doc "Merges a list of voices into a new voice object."
(voice-merger-internal (voices self) nil))

; BPF-FROM-POLY

(defmethod! bpf-lib-from-poly ((poly-obj poly))
    :initvals '(nil)
    :indoc '( "poly object")
    :doc "Builds a bpf-lib from a poly object."
    :icon 475
(make-instance 'bpf-lib
  :bpf-list
  (mapcar #'(lambda (voice)
   (let ((tempo-ms (float (/ 1000 (/ 60 (second (car (tempo voice))))))))
   (simple-bpf-from-list (butlast (dx->x 0 (om* tempo-ms (om-abs (tree2ratio (tree voice))))))
                                    (flat (mapcar #'lmidic (chords voice))))))
   (voices poly-obj))))

(defun om-variables-in (x)
 (cond ((null x) nil)
           ((atom x) 
             (if (equal 'screamer::variable (type-of x))
                 x
                 nil))
           (t (remove nil (cons (om-variables-in (car x)) (om-variables-in (cdr x)))))))

(defmethod! variables-in ((domain-list list))
    :initvals '(nil)
    :indoc '( "list")
    :doc "
 Returns all screamer variables, removing rests."
    :icon 486
(om-variables-in domain-list))

(defmethod! contain-rests? ((domain-list list))
    :initvals '((60 nil nil (s::an-integerv)))
    :indoc '( "domain-list")
    :doc "Returns t if the list containts any rests (represented as null value <nil> in the screamer-score domains)."
    :icon 486
(some #'null (flat domain-list)))

(defmethod! pcset-equalv ((domain-list list) (pcset list))
    :initvals '((60 64 67) (0 4 7))
    :indoc '( "midic" "pcset list")
    :doc "Returns t if a list of midi values <input1> containts all the pitch-classes in pcset list <input2>."
    :icon 486
(om?::all-membersv (m->pcv (flat (remove nil (flat domain-list)))) pcset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PC-SET-THEORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod! member-of-scv? ((vars t) (sc-list list) (mode string) &optional (random? nil)) 
  :initvals '((nil) (om::3-11a om::3-11b) "midi" nil) 
:indoc '("list of screamer variables" "list of set-classes<fn>" "pc or midi" "t or nil") 
  :doc "Constraint a list of Screamer variables to be all members of a list of set-classes in Forte notation."
:menuins '((2 (("midi" "midi") ("pc" "pc"))))
    :icon 486 
(cond (random? 
      (if (equal mode "midi")
          (om?::random-member-of-setclassv vars (om?::om-symb->om? sc-list))
          (om?::random-member-of-setclassv-pcs vars (om?::om-symb->om? sc-list))))
      (t (if (equal mode "midi")
         (om?::member-of-setclassv vars (om?::om-symb->om? sc-list))
         (om?::member-of-setclassv-pcs vars (om?::om-symb->om? sc-list))))))

(defmethod! set-classpv? ((vars t) (pc-set list))  
  :initvals '((nil) (0 4 7)) 
  :indoc '("list of screamer variables => midi" "fn or integers") 
  :doc "Constraint a list of Screamer variables <integers-between 0 11> to be all members of the selected pc-set <list of integers>."
    :icon 486 
  (om?::pcv? vars (car (gethash (sort (remove-duplicates pc-set) #'<) om?::*all-possible-chroma-subsets-hash*))))

(defmethod! set-classpv? ((vars t) (pc-set symbol))  
  :initvals '((nil) 'om::|3-11B|) 
  :indoc '("list of screamer variables => midics" "fn or integers") 
  :doc "Constraint a list of Screamer variables <integers-between 0 11> to be all members of the selected pc-set <list of integers>."
    :icon 486 
  (om?::pcv? vars (om?::om-symb->om? pc-set)))

(defmethod! sub-setpv? ((vars t) (pc-set list) &optional (card-min nil) (card-max nil) (forbid nil))  
  :initvals '((nil) (0 1 3 4 6 9) nil nil nil) 
  :indoc '("list of screamer variables => (integers-betweenv 0 11)" "fn or integers" "integer" "integer" "list of fns") 
  :doc "Constraint a list of Screamer variables <integers-between 0 11> to be all subsets of the selected pc-set.
Optional arguments: 
 <CARD-MIN> minimun cardinality.
 <CARD-MAX> maximun cardinality.
 <FORBID> list of set classes in Forte notation [ex.: (3-11a 3-11b 3-4b)]."
    :icon 486 
(om?::subv? vars (om?::fn pc-set) card-min card-max (om?::om-symb->om? forbid)))

(defmethod! sub-setpv? ((vars t) (pc-set symbol) &optional (card-min nil) (card-max nil) (forbid nil))  
  :initvals '((nil) 'om::|6-27A| nil nil nil) 
  :indoc '("list of screamer variables => (integers-betweenv 0 11)" "fn or integers" "integer" "integer" "list of fns") 
  :doc "Constraint a list of Screamer variables <integers-between 0 11> to be all subsets of the selected pc-set.
Optional arguments: 
 <CARD-MIN> minimun cardinality.
 <CARD-MAX> maximun cardinality.
 <FORBID> list of set classes in Forte notation."
    :icon 486 
(om?::subv? vars pc-set card-min card-max (om?::om-symb->om? forbid)))   

;===============================================
;;;===> SCS

(defmethod! SC-subsets ((fn symbol) &optional (card-min nil) (card-max nil) (forbid nil))
  :initvals '('om::|6-27A| nil nil nil)
:indoc '("fn symbol" "integer" "integer") 
  :doc "Return all subsets."
    :icon 486
(om?::om?-symb->om  (om?::all-subsets (om?::om-symb->om? fn)
                                     (if card-min card-min 0)
                                     (if card-max card-max (om?::card fn))
                                     (if forbid (om?::om-symb->om? forbid) nil))))

(defmethod! SC-subsets ((fn list) &optional (card-min nil) (card-max nil) (forbid nil))
  :initvals '('(om::|6-27A| om::|6-27B|) nil nil nil)
  :indoc '("fn symbol" "integer" "integer") 
  :doc "Return all subsets."
  :icon 486
(remove-duplicates 
 (flat 
  (mapcar #'(lambda (x)
             (om?::om?-symb->om
              (om?::all-subsets (om?::om-symb->om? x)
                        (if card-min card-min 0)
                        (if card-max card-max (om?::card x))
                        (if forbid (om?::om-symb->om? forbid) nil))))
             fn)
  )
 :test #'equal))

(defmethod! SCs-card ((card integer))
  :initvals '(6)
  :indoc '("integer" ) 
  :menuins '((0 (("1" 1)  ("2" 2) ("3" 3) ("4" 4) ("5" 5) ("6" 6) ("7" 7) ("8" 8) ("9" 9) ("10" 10) ("11" 11) ("12" 12))))
  :doc "Return all fn symbols."
  :icon 486
(om?::om?-symb->om
 (case card 
 (1 om?::card1)
 (2 om?::card2)
 (3 om?::card3 )
 (4 om?::card4 )
 (5 om?::card5 )
 (6 om?::card6 )
 (7 om?::card7 )
 (8 om?::card8 )
 (9 om?::card9 )
 (10 om?::card10 )
 (11 om?::card11 )
 (12 om?::card12))))

(defmethod! SC+off ((midics list)) 
  :initvals '((6000 6100))
  :indoc '("midics")
  :icon 486
  :doc  "returns a list containing the SC-name and the offset 
(i.e. the transposition relative to the prime form of the SC) of 
 midis (a list of midic-values), midics can also be a list of lists 
 of midics in which case SC+off returns the SCs with offsets 
 for each midic-value sublist."
  (if (atom (car midics))
      (let ((res (gethash (om?::make-set midics) om?::*all-possible-chroma-subsets-hash*)))
         (x-append (om?::om?-symb->om (first res)) (second res)))
    (let (res)
      (dolist (midics-l midics)
        (push (gethash  (om?::make-set midics-l) om?::*all-possible-chroma-subsets-hash*) res))
      (mapcar #'(lambda (x)
       (x-append (om?::om?-symb->om (first x)) (second x))) (nreverse res)))))

(defmethod! SC-name ((midics list))
  :initvals '((6000 6100))
  :indoc '("midics")
  :icon 486
  :doc  "returns a list containing the SC-name and the offset 
(i.e. the transposition relative to the prime form of the SC) of 
 midis (a list of midic-values), midics can also be a list of lists 
 of midics in which case SC+off returns the SCs with offsets 
 for each midic-value sublist."
  (if (atom (car midics))
      (om?::om?-symb->om (car (gethash (om?::make-set midics) om?::*all-possible-chroma-subsets-hash*)))
    (let (res)
      (dolist (midics-l midics)
        (push (car (gethash (om?::make-set midics-l) om?::*all-possible-chroma-subsets-hash*)) res))
      (mapcar #'om?::om?-symb->om (nreverse res)))))

(defmethod! sub/supersets ((SC t) (card number))
  :initvals '('om::4-z15a 9)
  :indoc '("SC" "card")
  :icon 486
  :doc "returns all subset classes of SC (when card is less than the cardinality of SC)
or superset classes (when card is greater than the cardinality of SC) 
of cardinality card."
  (if (= (om?::card (om?::om-symb->om? SC)) card)
    SC
    (if (> (om?::card (om?::om-symb->om? SC)) card)
      (om?::om?-symb->om (om?::all-subsets (om?::om-symb->om? SC) card card nil))
      (om?::supersets (om?::om-symb->om? SC) card))))

(defmethod! SC-info ((mode symbol) (SC om::t))
  :initvals '(:prime 'om::4-z15a)
  :indoc '("mode" "SC" ) 
  :menuins '((0 (("prime" :prime) ("icv" :icv)  ("member-sets" :member-sets) ("complement-pcs" :complement-pcs))))
  :doc "Returns the selected info (prime-form, interval class vector, members-sets or complement) about an SC."
  :icon 486
(cond 
((equal mode :prime) (om?::prime SC))
((equal mode :icv) (om?::icv SC))
((equal mode :member-sets) (om?::pc-set-transpositions (om?::prime SC)))
((equal mode :complement-pcs) (om?::set-complement  (om?::prime SC)))
(t (progn (om-message-dialog "Please select a valid mode (:prime, :icv, :member-sets or :complement-pcs).") (om-abort)))))

(defmethod! normal-order ((input list) (mode string))  
  :initvals '((6000 6700 6400 7100) "midic") 
  :indoc '("list of midics or pitch classes" "pc or midic") 
  :doc "Returns the normal order [list of integers] of the given set [midicents or pitch classes]."
  :menuins '((1 (("midic" "midic") ("pc" "pc"))))
  :icon 486
(if (equal mode "midic")
    (list! (om?::get-n-ord (mc->pcv input)))
    (list! (om?::get-n-ord input))))
	
(defmethod! normal-orderv ((input t) (mode string))  
  :initvals '(t "midi") 
  :indoc '("list of midicents or pitch classes" "pcs or midics") 
  :doc "Returns an Screamer variable constrained to be the normal order of a given set [midi values or pitch classes]. The input can be variables or non variables."
  :menuins '((1 (("midi" "midi") ("pc" "pc"))))
  :icon 486
(if (equal mode "midi")
    (if (every #'screamer::bound? input)
        (list! (om?::get-n-ord (m->pcv input)))
        (om?::n-ordv input))
	  (if (every #'screamer::bound? input)
	      (list! (om?::get-n-ord input))
        (om?::n-ordv-pcs input))))


