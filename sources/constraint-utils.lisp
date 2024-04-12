;; MIT License

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

;;;;;;;;;;;;;;;;;;;;;;;
;; OM-SCREAMER FUNCTIONS

(in-package :om-screamer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTRAINTS FOR PITCHES - ONE MELODIC LINE (ASSERT! IS NOT NEEDED)
;;; (FOR SCREAMER SOLVER - DEPRECATED) - NEED TO CHECK IF STILL NEEDED

(defun all-ascendingv (var-list)
 (assert!-apply-rec #'(lambda (x y) (all-asc x y)) var-list)) 

(defun all-asc (var1 var2)
 (>v (-v var2 var1) 0))
	  
(defun all-descendingv (var-list)
 (assert!-apply-rec #'(lambda (x y) (all-desc x y)) var-list)) 

(defun all-desc (var1 var2)
 (<v (-v var2 var1) 0))

(defun allowed-melodic-intervals (var-list int-list)
 (assert!-apply-rec #'(lambda (x y) (al-mel-ints x y int-list)) var-list)) 

(defun al-mel-ints (var1 var2 list)
 (memberv (absv (-v var2 var1)) list))

(defun not-allowed-melodic-intervals (var-list int-list)
 (assert!-apply-rec #'(lambda (x y) (not-al-mel-ints x y int-list)) var-list)) 

(defun not-al-mel-ints (var1 var2 list)
 (notv (memberv (absv (-v var2 var1)) list)))

(defun no-repeated-melodic-intervals (var-list)
 (assert!-apply-rec #'(lambda (x y z) (no-repeat-mel-ints x y z)) var-list)) 

(defun no-repeat-mel-ints (var1 var2 var3)
 (/=v (-v var2 var1) (-v var3 var2)))

(defun ballistic? (var-list) ;;;
" < From PW-CONSTRAINTS by Mikael Laurson >
A ballistic movement allows two jumps in the same direction, 
but the larger jump has to be below the smaller one."
 (assert!-apply-rec #'(lambda (x y z) (blstc? x y z)) var-list)) 

(defun blstc? (midi1 midi2 midi3) 
" midi1, midi2 and midi3 should form a 'ballistic' melodic movement.
A ballistic movement allows two jumps in the same direction, 
but the larger jump has to be below the smaller one."
  (let* ((up (<v midi1 midi2 midi3))
         (down (>v midi1 midi2 midi3))
         (mel-diff1 (absv (-v midi1 midi2)))
         (mel-diff2 (absv (-v midi2 midi3)))
         (jump-case? (orv (>v mel-diff1 2) (>v mel-diff2 2)))
         (same-direction? (orv up down)))
    (ifv (and jump-case? same-direction?)
       (ifv up 
           (>=v  mel-diff1 mel-diff2)
           (<=v  mel-diff1 mel-diff2))
        t)))

(defun symm? (var-list)
 (let ((int-mod12v (om::mod12v (om::x->dxv var-list))))
(assert! (equalv int-mod12v (reverse int-mod12v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OM METHODS - BUILT-IN CONSTRAINTS - UTILS
(in-package :om)

(defmethod! contain-variables? ((domain-list list))		  
    :initvals '((6000 nil nil (s::an-integerv)))
    :indoc '( "domain-list") 
    :doc "
First output: Returns t if the list containts any screamer variable and nil if it is an empty list (nil) or if contains only numbers.
Second output: Returns all screamer variables if the first output is true and nil otherwise."                    
    :icon 487
	:numouts 2
 (let ((screamer-variables (s::variables-in domain-list)))
  (values (not (null screamer-variables))
          (if screamer-variables
		 	 (if (list-of-listp domain-list)
	             (group-list screamer-variables (mapcar #'length domain-list) 'linear)
		      screamer-variables)
		  nil))
  ))

(defmethod! contain-rests? ((domain-list list))		  
    :initvals '((6000 nil nil (s::an-integerv)))
    :indoc '( "domain-list") 
    :doc "Returns t if the list containts any rests (represented as null value <nil> in the screamer-score domains)."                    
    :icon 487
(not (null (position 'nil domain-list))))
 
(defmethod! pcset-equalv ((domain-list list) (pcset list))		  
    :initvals '((6000 6400 6700) (0 4 7))
    :indoc '( "midics" "pcset list") 
    :doc "Returns t if a list of midics <input1> containts all the pitch-classes in pcset list <input2>."  
    :icon 487
(all-membersv (mc->pcv (flat (remove nil domain-list))) pcset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HARMONIC AND MELODIC INTERVALS

(defmethod! harmonic-intervalsv ((domain-list list) (mode string))		  
    :initvals '(nil "+/-")
    :indoc '( "midics" "positive/negative or absolute") 
    :doc "Returns the harmonic intervals (positive/negative or absolute) of a given list on different outputs.
Use > and < to add/remove outputs." 
	:menuins '((1 (("+/-" "+/-") ("abs" "abs")))) 
    :icon 487
	:numouts 2
 (let ((intsv (if (equal mode "+/-") 
	(mapcar #'(lambda (x y) (if (or (null x) (null y)) nil (s::-v y x))) (flat domain-list) (cdr (flat domain-list))) 
	(mapcar #'(lambda (x y) (if (or (null x) (null y)) nil (om?::absv (s::-v y x)))) (flat domain-list) (cdr (flat domain-list)))))) 
  (values-list (first-n intsv (length intsv)))))

(defmethod get-boxcallclass-fun ((self (eql 'harmonic-intervalsv))) 'OMBoxSplit)

(defmethod! melodic-intervalsv ((domain-list1 list) (domain-list2 list) (mode string))		  
    :initvals '(nil nil "+/-")
    :indoc '( "midics" "positive/negative or absolute") 
    :doc "Returns the melodic intervals (positive/negative or absolute) between two lists on different outputs.
Use > and < to add/remove outputs." 
	:menuins '((1 (("+/-" "+/-") ("abs" "abs")))) 
    :icon 487
	:numouts 2
 (let ((intsv (if (equal mode "+/-") 
	(mapcar #'(lambda (x y) (if (or (null x) (null y)) nil (s::-v y x))) (flat domain-list1) (flat domain-list2)) 
	(mapcar #'(lambda (x y) (if (or (null x) (null y)) nil (om?::absv (s::-v y x)))) (flat domain-list1) (flat domain-list2))))) 
  (values-list (first-n intsv (length intsv)))))

(defmethod get-boxcallclass-fun ((self (eql 'melodic-intervalsv))) 'OMBoxSplit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;==> IN PROGRESS

(defun parallel? (list1 list2)
 (if (or (null (first list1)) (null (first list2)) 
	     (null (second list1)) (null (second list2))) 
	  nil
 (let ((m1 (s::-v (first list2) (first list1)))
       (m2 (s::-v (second list2) (second list1))))
  (s::orv (s::andv (s::>v m1 0) ;same direction - same intervals
                   (s::>v m2 0)
	      	       (s::=v m1 m2)) 
 	      (s::andv (s::<v m1 0)
 	   	           (s::<v m2 0)
				   (s::=v m1 m2))))))
				   
(defun direct? (list1 list2)
(if (or (null (first list1)) (null (first list2)) 
     (null (second list1)) (null (second list2))) 
  nil
(let ((m1 (s::-v (first list2) (first list1)))
      (m2 (s::-v (second list2) (second list1))))
   (s::orv (s::andv (s::>v m1 0) ;same direction - different intervals
		          (s::>v m2 0)
		          (s::/=v m1 m2)) 
	     (s::andv (s::<v m1 0)
			   (s::<v m2 0)
		          (s::/=v m1 m2))))))
				  
(defun contrary? (list1 list2)
(if (or (null (first list1)) (null (first list2)) 
     (null (second list1)) (null (second list2))) 
  nil
(let ((m1 (s::-v (first list2) (first list1)))
      (m2 (s::-v (second list2) (second list1))))			 
 (s::orv (s::andv (s::<v m1 0) ;opposite directions
			      (s::>v m2 0)) 
		 (s::andv (s::>v m1 0)
		          (s::<v m2 0))))))	
					  
(defun oblique? (list1 list2)
(if (or (null (first list1)) (null (first list2)) 
     (null (second list1)) (null (second list2))) 
  nil
 (let ((m1 (s::-v (first list2) (first list1)))
       (m2 (s::-v (second list2) (second list1))))					  
	(s::orv (s::andv (s::=v m1 0)  ;interval 1 = 0 - interval 2 /= 0
			         (s::/=v m2 0))
			(s::andv (s::/=v m1 0);interval 1 /= 0 - interval 2 = 0
				     (s::=v m2 0))))))

(defun stepwise? (n1 n2)
(if (or (null n1) (null n2)) 
    nil
    (s::memberv (om?::absv (s::-v n2 n1)) '(100 200))))

(defun any-step? (list1 list2)
 (s::orv (stepwise? (second list1) (second list2))
         (stepwise? (first list1) (first list2))))

(defun step-upper-voice? (list1 list2)
 (stepwise? (first list1) (first list2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
;;; CONSTRAINTS

(defmethod! constraint-scale-one-voice ((mode string) (scale list) (voices-list list))		  
    :initvals '("midics" (0 200 400 500 700 900 1100 1200) (0))
    :indoc '( "midics/pcs" "list" "list") 
    :doc "Returns a screamer-score-constraint object." 
	:menuins '((0 (("midics" "midics") ("pcs" "pcs")))) 
    :icon 487

(if (equal mode "pcs")
    (let ((constraint (eval `#'(lambda (x) (s::assert! (?::hard-memberv x ,(reclist-vars (mc->pcv scale))))))))
     (constraint-one-voice constraint  "n-inputs" voices-list "pitch"))

    (let ((constraint  (eval `#'(lambda (x) (s::assert! (?::hard-memberv (mc->pcv x) ,(reclist-vars (mc->pcv scale))))))))
     (constraint-one-voice constraint  "n-inputs" voices-list "pitch"))))

(defmethod! constraint-chords-alldiff-notes ((mode string) (input-mode string) &optional voices-list)		  
    :initvals '("midics" "all-voices" nil)
    :indoc '("midics/pcs" "all-voices/voices-list" "list") 
    :doc "Returns a screamer-score-constraint object." 
	:menuins '((0 (("midics" "midics") ("pcs" "pcs")))
                         (1 (("all-voices" "all-voices") ("voices-list" "voices-list")))) 
    :icon 487
    (let ((constraint (if (equal mode "midics")
                                 (eval ` #'(lambda (x) (s::assert! (apply 's::/=v (remove nil x)))))
                                 (eval `#'(lambda (x) (s::assert! (apply 's::/=v  (mc->pcv (remove nil x)))))))))
    (if (equal input-mode "all-voices")
       (constraint-harmony constraint  "n-inputs" "all-voices" "all")
       (constraint-harmony constraint  "n-inputs" "voices-list" "all" voices-list))))

(defmethod! no-crossing-voices ((input-mode string) (unison? string) &optional voices-list)	  
    :initvals '("all-voices" "no" nil)
    :indoc '( "all-voices/voices-list" "yes/no" "list") 
    :doc "Returns a screamer-score-constraint object." 
    :menuins '((0 (("all-voices" "all-voices") ("voices-list" "voices-list")))
                         (1 (("no" "no") ("yes" "yes")))) 
    :icon 487
    (let ((constraint (if (equal unison? "no")
                                 (eval `#'(lambda (x) (om?::assert!-apply-rec #'(lambda (y z) (s::>v y z)) x))) 
                                 (eval `#'(lambda (x) (om?::assert!-apply-rec #'(lambda (y z) (s::>=v y z)) x))))))
                                     
    (if (equal input-mode "all-voices")
       (constraint-harmony constraint  "n-inputs" "all-voices" "all")
       (constraint-harmony constraint  "n-inputs" "voices-list" "all" voices-list))))

(defmethod! not-parallel-fifths-octaves ((voices-list list))		  
    :initvals '( ((0 1) (0 2)) )
    :indoc '("list") 
    :doc "Returns a screamer-score-constraint object." 
    :icon 487
    (let ((constraint
             (eval `#'(lambda (x y)
             (if (or (some #'null x) (some #'null y)) 
                  nil
               (let ((interval1  (s::funcallv #'mod (om?::absv (s::-v (first x) (second x))) 1200))
                     (interval2 (s::funcallv #'mod (om?::absv (s::-v (first y) (second y))) 1200)))
                      
             (s::assert! 
              (?::ifv (parallel? x y)
                   (s::orv (s::notv (s::memberv interval1 '(0 700)))
                                        (s::notv (s::memberv interval2 '(0 700))))

               t)
              )
             )
            )
           )
          )
        )
      )
   (constraint-harmony constraint  "n-inputs" "voices-list" "all" voices-list)))

(defmethod! chord-at-measure ((measures list) (chords list) (voices list))		  
  :initvals '( nil nil nil)
  :indoc '("list" "list" "list") 
  :doc "Constraint all notes of a voice (or voices) to be members of chord at measure number. 

Returns a list of screamer-score-constraint objects." 
  :icon 487
   (let ((constraints (loop for chord in chords collect (eval `#'(lambda (x) (s::assert! (all-membersv x ,(reclist-vars chord))))))))
    (loop for mes in measures 
             for cs in constraints
   collect (constraint-measure (constraint-one-voice cs "list" voices "pitch") mes))))

(defmethod! chord-at-times ((chords list) (onsets list) (voices list) &optional (mode "midics"))		  
  :initvals '( nil nil nil "midics")
  :indoc '("list" "list" "list" "string") 
  :menuins '((3 (("midics" "midics") ("pcs" "pcs")))) 
  :doc "Constraint all notes of a voice (or voices) to be members of chord at the given onset.

The onsets list should be in chronologial order, e.g., a chord (6000 6400 6700) with onset 1/4 
will constraint all notes to be members of this chord when onset is greater or equal 0 and smaller
than 1/4. 

The number of chords must be the same as the number of onsets.

If the optional <mode> arguments is supplied, it can constraint notes to be members of the pitch class
content of the given chord <pcs> or exactly the same notes of the chord <midics> - default. 

Returns a list of screamer-score-constraint objects." 
  :icon 487
   (let* ((onset-pairs (mapcar #'list (butlast (x-append 0 onsets)) onsets))
           (constraints (loop for chord in chords
                              for onset-pair in onset-pairs
                              collect (eval `#'(lambda (x)
                                                      (let ((onset (second x)))
                                                        (when (first x)
                                                       (if (and (>= onset ,(first onset-pair))
                                                                   (< onset ,(second onset-pair)))
                                                            (if (equal ,mode "midics")
                                                                (s::assert! (s::memberv (first x) ,(reclist-vars chord)))
                                                                (s::assert! (s::memberv (mc->pcv (first x))  ,(reclist-vars (remove-duplicates (mc->pcv chord))))))))))))))
    (loop for cs in constraints
             collect (constraint-one-voice cs "n-inputs" voices "pitch-onset"))))