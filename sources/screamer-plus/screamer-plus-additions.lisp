;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ADDITIONS TO SCREAMER PLUS

(in-package :screamer+)

; EXPERIMENTS:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;==> BASED ON MEMBERS-OFV

;;; MIDICENTS

(defun equal-mc-pc (n1 n2)
(equal (/ (mod n1 1200) 100) n2))

(defun ascending-setv (x)
 (funcallv #'sort x #'<v))

(defun list-of-mcs-to-pcsv (x)
 (mapcarv #'(lambda (z) (/v (funcallv #'mod z 1200) 100)) x))

(defun make-mcsetv (x)
  (let (
        (z (make-variable))
       )
     (?::attach-noticer!
       #'(lambda()
           (when (and (bound? x) (every #'bound? (value-of x)))
              (do* (
                    (dec (?::apply-substitution x) (cdr dec))
                    (curr (car dec) (car dec))
                    (vals nil)
                    )
                  ((endp dec) 
                   (assert! (equalv z vals))
                   )
		  (declare (type list dec vals))
		  (declare (type fixnum curr))
                 (om::push-end-new (value-of curr) vals :test #'equal-mc-pc :key #'(lambda (n) (/ (mod n 1200) 100)))
                 )
              )
           )
       x)
 (ascending-setv (list-of-mcs-to-pcsv z)))
)

;;; MIDI

(defun equal-m-pc (n1 n2)
(equal (mod n1 12) n2))

(defun list-of-m-to-pcv (x)
 (mapcarv #'(lambda (z) (funcallv #'mod z 12)) x))

(defun make-midisetv (x)
  (let (
        (z (make-variable))
       )
     (?::attach-noticer!
       #'(lambda()
           (when (and (bound? x) (every #'bound? (value-of x)))
              (do* (
                    (dec (?::apply-substitution x) (cdr dec))
                    (curr (car dec) (car dec))
                    (vals nil)
                    )
                  ((endp dec)
                   (assert! (equalv z vals))
                   )
		   (declare (type list dec vals))
		   (declare (type fixnum curr))
                 (om::push-end-new (value-of curr) vals :test #'equal-m-pc :key #'(lambda (n) (mod n 12)))
                 )
              )
           )
       x)
 (ascending-setv (list-of-m-to-pcv z)))
)

(defun make-setv (x)
  (let (
        (z (make-variable))
       )
     (?::attach-noticer!
       #'(lambda()
           (when (and (bound? x) (every #'bound? (value-of x)))
              (do* (
                    (dec (?::apply-substitution x) (cdr dec))
                    (curr (car dec) (car dec))
                    (vals nil)
                    )
                  ((endp dec)
                   (assert! (equalv z vals))
                   )
                 (declare (type list dec vals))				   
		 (declare (type fixnum curr))
                 (pushnew (value-of curr) vals :test #'equal)
                 )
              )
           )
       x)
  (funcallv #'sort z #'<v)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;===> BASED ON NOT-EQUALV FUNCTION

(defun interval-memberv (x y sequence)
  (if (and (bound? x) (bound? y))
      (member (- (value-of x) (value-of y)) sequence)

    (let (
	  (z (a-booleanv))
	  (i (a-numberv))
	  )
	  (assert! (=v i (-v x y)))
	  (assert! (eqv z (memberv i sequence)))

      (attach-noticer!
       #'(lambda()
	   (when (and (bound? y) (bound? x) (not (bound? i)))
	     (assert!-equalv z (memberv i sequence))
	     (when (and (known?-true z) (enumerated-domain-p i))
	       (assert!-memberv-internal
			   i
		(remove-if-not #'(lambda(e) (member e sequence))
			   (variable-enumerated-domain i))
		)
	       )
	     )
	   )
       i)

      z)
    )
  )

(defun interval-notv-memberv (x y sequence)
  (if (and (bound? x) (bound? y))
      (not (member (- (value-of x) (value-of y)) sequence))

    (let (
	  (z (a-booleanv))
  	  (i (a-numberv))
  	  )
  	  (assert! (=v i (-v x y)))
  	  (assert! (eqv z (notv (memberv i sequence))))

        (attach-noticer!
         #'(lambda()
  	   (when (and (bound? y) (bound? x) (not (bound? i)))
  	     (assert!-equalv z (notv (memberv i sequence)))
  	     (when (and (known?-true z) (enumerated-domain-p i))
  	       (assert!-memberv-internal
  			   i
  		(remove-if-not #'(lambda(e) (not (member e sequence)))
  			   (variable-enumerated-domain i))
  		)
  	       )
  	     )
  	   )
         i)

        z)
      )
    )

(defun abs-v (n)
 (maxv n (*v n -1)))

(defun abs-interval-memberv (x y sequence)
(if (and (bound? x) (bound? y))
    (member (abs (- (value-of x) (value-of y))) sequence)

  (let (
  (z (a-booleanv))
  (i (a-numberv))
  )
  (assert! (=v i (abs-v (-v x y))))
  (assert! (eqv z (memberv i sequence)))

    (attach-noticer!
     #'(lambda()
   (when (and (bound? y) (bound? x) (not (bound? i)))
     (assert!-equalv z (memberv i sequence))
     (when (and (known?-true z) (enumerated-domain-p i))
       (assert!-memberv-internal
		   i
	(remove-if-not #'(lambda(e) (member e sequence))
		   (variable-enumerated-domain i))
	)
       )
     )
   )
     i)

    z)
  )
)

(defun abs-interval-notv-memberv (x y sequence)
(if (and (bound? x) (bound? y))
    (not (member (abs (- (value-of x) (value-of y))) sequence))

  (let (
  (z (a-booleanv))
  (i (a-numberv))
  )
  (assert! (=v i (abs-v (-v x y))))
  (assert! (eqv z (notv (memberv i sequence))))

    (attach-noticer!
     #'(lambda()
   (when (and (bound? y) (bound? x) (not (bound? i)))
     (assert!-equalv z (notv (memberv i sequence)))
     (when (and (known?-true z) (enumerated-domain-p i))
       (assert!-memberv-internal
		   i
	(remove-if-not #'(lambda(e) (not (member e sequence)))
		   (variable-enumerated-domain i))
	)
       )
     )
   )
     i)

    z)
  )
)

(defun hard-memberv (x sequence)
  (if (bound? x)
      (member (value-of x) sequence)

    (let (
	  (z (a-booleanv))
	  (i (a-numberv))
	  )
	  (assert! (=v i (value-of x)))
	  (assert! (eqv z (memberv i sequence)))

      (attach-noticer!
       #'(lambda()
	   (when (and (bound? x) (not (bound? i)))
	     (assert!-equalv z (memberv i sequence))
	     (when (and (known?-true z) (enumerated-domain-p i))
	       (assert!-memberv-internal
			   i
		(remove-if-not #'(lambda(e) (member e sequence))
			   (variable-enumerated-domain i))
		)
	       )
	     )
	   )
       i)

      z)
    )
  )

  (defun mod-interval-memberv (x y sequence)
  (if (and (bound? x) (bound? y))
      (member (mod (- (value-of x) (value-of y)) 12) sequence)

    (let (
    (z (a-booleanv))
    (i (a-numberv))
    )
    (assert! (=v i (funcallv #'mod (-v x y) 12)))
    (assert! (eqv z (memberv i sequence)))

      (attach-noticer!
       #'(lambda()
     (when (and (bound? y) (bound? x) (not (bound? i)))
       (assert!-equalv z (memberv i sequence))
       (when (and (known?-true z) (enumerated-domain-p i))
         (assert!-memberv-internal
  		   i
  	(remove-if-not #'(lambda(e) (member e sequence))
  		   (variable-enumerated-domain i))
  	)
         )
       )
     )
       i)

      z)
    )
  )

  (defun mod-interval-notv-memberv (x y sequence)
  (if (and (bound? x) (bound? y))
      (not (member (mod (- (value-of x) (value-of y)) 12) sequence))

    (let (
    (z (a-booleanv))
    (i (a-numberv))
    )
    (assert! (=v i (funcallv #'mod (-v x y) 12)))
    (assert! (eqv z (notv (memberv i sequence))))

      (attach-noticer!
       #'(lambda()
     (when (and (bound? y) (bound? x) (not (bound? i)))
       (assert!-equalv z (notv (memberv i sequence)))
       (when (and (known?-true z) (enumerated-domain-p i))
         (assert!-memberv-internal
  		   i
  	(remove-if-not #'(lambda(e) (not (member e sequence)))
  		   (variable-enumerated-domain i))
  	)
         )
       )
     )
       i)

      z)
    )
  )

