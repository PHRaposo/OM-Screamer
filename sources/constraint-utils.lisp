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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OM METHODS - BUILT-IN CONSTRAINTS - UTILS

(in-package :om)

(defmethod! variables-in ((domain-list list))
    :initvals '(nil)
    :indoc '( "list")
    :doc "
 Returns all screamer variables, removing rests."
    :icon 487
 (let ((screamer-variables (screamer::variables-in (flat domain-list))))
          (if screamer-variables
		 	 (if (list-of-listp domain-list)
	             (group-list screamer-variables (mapcar #'length domain-list) 'linear)
		      screamer-variables))))

(defmethod! contain-rests? ((domain-list list))
    :initvals '((6000 nil nil (s::an-integerv)))
    :indoc '( "domain-list")
    :doc "Returns t if the list containts any rests (represented as null value <nil> in the screamer-score domains)."
    :icon 487
(not (null (position 'nil (flat domain-list)))))

(defmethod! pcset-equalv ((domain-list list) (pcset list))
    :initvals '((6000 6400 6700) (0 4 7))
    :indoc '( "midics" "pcset list")
    :doc "Returns t if a list of midics <input1> containts all the pitch-classes in pcset list <input2>."
    :icon 487
(all-membersv (mc->pcv (flat (remove nil (flat domain-list)))) pcset))


(defun flat-chords (x)
 (flat (loop for el in x collect (if (listp el) (reverse el) el))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;==> COUNTERPOINT: IN PROGRESS

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

(defun tied? (x y)
(if (or (null x) (null y))
nil
    (s::equalv (s::variable-name x) (s::variable-name y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MELODIC-LINE-INTERVALS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-asc-desc-lines (notes)
 (let ((all-positions
  (loop for n from 0 to (- (length notes) 2)
        if (> (nth n notes) (nth (1+ n) notes))
        collect (om::x-append n (1+ n)) into descending
        else 
        collect (om::x-append n (1+ n)) into ascending
        finally (return (om::x-append (list (om::flat ascending)) 
                                      (list (om::flat descending)))))))
  (om::posn-match notes (mapcar #'remove-duplicates all-positions))))

(defun split-ascending (notes)
(om::group-list 
  notes 
  (om::x->dx 
  (om::x-append 
   0  
  (loop for n from 0 to (- (length notes) 2)
        if (> (nth n notes) (nth (1+ n) notes))
        collect (1+ n))
  (length notes)))
 'linear))

(defun split-descending (notes)
 (om::group-list 
  notes 
  (om::x->dx 
  (om::x-append 
   0  
  (loop for n from 0 to (- (length notes) 2)
        if (< (nth n notes) (nth (1+ n) notes))
        collect (1+ n))
  (length notes)))
'linear))

(defun get-melodic-intervals (asc-or-desc-line)
 (abs (- (first asc-or-desc-line) (om::last-elem asc-or-desc-line))))
  
(defun melodic-line-intervals (melody allowed-mel-ints)
"This functions returns true if all notes <first input>
in the melodic line contains only allowed intervals
<second input>.
The intervals is calculated from the lowest note to the highest in
a ascending or descending line. 
E.g.: a line '(6000 6400 6600 6200 5900) will return two intervals,
600 (from the ascending line 6000 6400 6600) and 700 (from the
descending line 6600 6200 5900). Then, if those intervals are allowed
the function will return t, otherwise returns nil." 
 (let* ((asc-or-desc-lines (get-asc-desc-lines melody))
       (ascending (split-ascending (first asc-or-desc-lines)))
       (descending (split-descending (second asc-or-desc-lines)))
       (all-intervals (mapcar #'get-melodic-intervals (om::x-append ascending descending))))
 (loop for interval in all-intervals
       always (member interval allowed-mel-ints))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTRAINTS

(defmethod! constraint-scale ((mode string) (scale list) (voices-list list))
    :initvals '("midics" (0 200 400 500 700 900 1100 1200) (0))
    :indoc '( "midics/pcs" "list" "list")
    :doc "Constraint one voice to contain only members of the input scale. Returns a screamer-score-constraint object."
	:menuins '((0 (("midics" "midics") ("pcs" "pcs"))))
    :icon 487

(if (equal mode "pcs")
    (let ((constraint (eval `#'(lambda (x) (?::hard-memberv (if (atom x) x (flat-chords x)) ,(reclist-vars (mc->pcv scale)))))))
     (constraint-one-voice constraint  "n-inputs" voices-list "pitch"))

    (let ((constraint  (eval `#'(lambda (x) (?::hard-memberv (mc->pcv (if (atom x) x (flat-chords x))) ,(reclist-vars (mc->pcv scale)))))))
     (constraint-one-voice constraint  "n-inputs" voices-list "pitch"))))

(defmethod! chords-alldiff ((mode string) (input-mode string) &optional voices-list)
    :initvals '("midics" "all-voices" nil)
    :indoc '("midics/pcs" "all-voices/voices-list" "list")
    :doc "Constraint all chords to contain or only differentc pcs or only different midics. Returns a screamer-score-constraint object."
	:menuins '((0 (("midics" "midics") ("pcs" "pcs")))
                         (1 (("all-voices" "all-voices") ("voices-list" "voices-list"))))
    :icon 487
    (let ((constraint (if (equal mode "midics")
                                 (eval ` #'(lambda (x) (apply 's::/=v (remove nil (flat x)))))
                                 (eval `#'(lambda (x) (apply 's::/=v  (mc->pcv (remove nil (flat x)))))))))
    (if (equal input-mode "all-voices")
       (constraint-harmony constraint  "n-inputs" "all-voices")
       (constraint-harmony constraint  "n-inputs" "voices-list" :voices voices-list))))

(defmethod! no-crossing ((input-mode string) (unison? string) &optional voices-list)
    :initvals '("all-voices" "no" nil)
    :indoc '( "all-voices/voices-list" "yes/no" "list")
    :doc "Returns a screamer-score-constraint object."
    :menuins '((0 (("all-voices" "all-voices") ("voices-list" "voices-list")))
                         (1 (("no" "no") ("yes" "yes"))))
    :icon 487
    (let ((constraint (if (equal unison? "no")
                                 (eval `#'(lambda (x) (apply #'s::>v (remove nil (flat-chords x)))))
                                 (eval `#'(lambda (x) (apply #'s::>=v (remove nil (flat-chords x))))))))

    (if (equal input-mode "all-voices")
       (constraint-harmony constraint  "n-inputs" "all-voices")
       (constraint-harmony constraint  "n-inputs" "voices-list" :voices voices-list))))

(defmethod! not-parallel-fifths-octaves ((voices-list list))
    :initvals '( ((0 1) (0 2)) )
    :indoc '("list")
    :doc "Returns a screamer-score-constraint object."
    :icon 487
    (let ((constraint
             (eval `#'(lambda (x y)
             (if (or (some #'null x) (some #'null y))
                  t
                  (if (or (some #'listp x) (some #'listp y))
                      (progn (om-message-dialog "The not-parallel-fifths-octaves constraint does not work with voices that contains chords.")
                                  (om-abort))
               (let ((interval1  (s::funcallv #'mod (om?::absv (s::-v (first x) (second x))) 1200))
                     (interval2 (s::funcallv #'mod (om?::absv (s::-v (first y) (second y))) 1200)))

              (?::ifv (parallel? x y)
                   (s::orv (s::notv (s::memberv interval1 '(0 700)))
                                        (s::notv (s::memberv interval2 '(0 700))))

               t))))))))

   (constraint-harmony constraint  "n-inputs" "voices-list" :voices voices-list)))

(defmethod! chord-at-measure ((measures list) (chords list) (voices list))
  :initvals '( nil nil nil)
  :indoc '("list" "list" "list")
  :doc "Constraint all notes of a voice (or voices) to be members of chord at measure number.

Returns a list of screamer-score-constraint objects."
  :icon 487
   (let ((constraints (loop for chord in chords collect (eval `#'(lambda (x) (all-membersv x ,(reclist-vars chord)))))))
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
                              collect  (eval `#'(lambda (x)
                                                        (let ((onset (second x)))
                                                        (if (null (first x))
                                                             t
                                                             (if (and (>= onset ,(first onset-pair))
                                                                         (< onset ,(second onset-pair)))
                                                                 (if (equal ,mode "midics")
                                                                     (s::memberv (first x) ,(reclist-vars chord))
                                                                     (s::memberv (mc->pcv (first x))  ,(reclist-vars (remove-duplicates (mc->pcv chord)))))
                                                             t))))))))
    (loop for cs in constraints
             collect (constraint-one-voice cs "n-inputs" voices "pitch-onset"))))

(defmethod! symmetrical-chords? ((input-mode string) &key (voices '(0 1 2)))
  :initvals '("all-voices" (0 1 2))
  :indoc '("string" "list")
  :doc "Constraint all chords to be symmetrical."
  :icon 487
  :menuins '((0 (("all-voices" "all-voices") ("voices-list" "voices-list"))))
 (let* ((cs (eval `#'(lambda (x)
                      (let* ((flat-list (flat-chords x))
							 (intervalsv (x->dx-absv (remove nil flat-list)))
							 (len (length intervalsv))
							 (positions (arithm-ser 0 (1- len) 1))
							 (symm-posn (cond ((oddp len)
								               (if (= 1 len)
											        0
												   (mat-trans (list (first-n positions (/ (1- len) 2))
												   	                (reverse (last-n positions (/ (1- len) 2)))))))
												  (t (if (= 2 len)
												         '(0 1)
														 (mat-trans (list (first-n positions (/ len 2))
														   	                 (reverse positions (/ len 2)))))))))
					   (if (listp symm-posn)
						   (apply #'s::andv (mapcar #'(lambda (x)
						  	            (s::=v (first x) (second x)))
					            (posn-match intervalsv symm-posn)))
                                                         t))))))
 (constraint-harmony cs "n-inputs" input-mode :voices voices)))
 
(defmethod! mel-line-intervals ((intervals list) (voices-list list))
 :initvals '((0 200 300 400 500 700 800 900) (0))
 :indoc '("list" "list")
 :doc "This functions returns true if all notes <first input>
in the melodic line contains only allowed intervals
<second input>.
This constraint will be individually applied to each voice listed
in the <third input>.
The intervals is calculated from the lowest note to the highest in
a ascending or descending line. 
E.g.: a line '(6000 6400 6600 6200 5900) will return two intervals,
600 (from the ascending line 6000 6400 6600) and 700 (from the
descending line 6600 6200 5900). Then, if those intervals are allowed
the function will return t, otherwise returns nil."
 :icon 487
 (let ((constraint (eval `(lambda (x)
            (om?::any-fn #'(lambda (vars)
			                (if (= 1 (length vars))
							     t
								 (melodic-line-intervals vars ,(reclist-vars intervals))))
					        x)))))
  (constraint-one-voice constraint "growing" voices-list "pitch")))

