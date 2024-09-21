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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCREAMER-SCORE-CONSTRAINTS
;;
;

;; BACKTRACK CONSTRAINTS

(defun compile-screamer-backtrack-constraint (fun)
 (let* ((expr (function-lambda-expression fun))
        (patchbox (find-lambda-patchbox fun))
        (patch-name (if (stringp patchbox);<== lambda function documentation
		                 patchbox 
						 (if (null patchbox);<== function in lambda mode
						     (symbol-name (second (cadr (third expr))))
							 (name (reference patchbox)))));<== patch in lambda mode
        (lambda-list (loop for x from 1 to (length (function-lambda-list fun))
                                    collect (intern (string (gensym)) :om))))
(compile 
 (eval
  `(defun ,(gensym (if (null patch-name) 
                                  "backtrack-anon-fun-"
                                  (concatenate 'string patch-name "-backtrack-")))
    ,lambda-list (om?::any-fn ,fun ,@lambda-list))))))

;;; MAIN-INTERFACE

(defmethod! constraint-one-voice ((constraint function) (input string)(voices list)(domain string)
	                               &key (percentage-mode "off") (percentage 0) (cs-mode "propagation"))
    :initvals '(nil "list" (0 1) "pitch" "off" 0 "propagation")
    :indoc '( "<lambda-patch>" "list" "list-of-voice-numbers" "string" "string" "number" "string")
    :doc "Constraint for one-voice"
    :menuins '((1 (("list" "list") ("n-inputs" "n-inputs") ("car-cdr" "car-cdr") ("growing" "growing")))
                     (3 (
                       ("pitch" "pitch")
                       ("pitch-dur" "pitch-dur")
                       ("pitch-onset" "pitch-onset")
                       ("pitch-dur-onset" "pitch-dur-onset")
                          )
                        )
                    (4 (("off" "off") ("exactly" "exactly") ("less-than" "less-than") ("greater-than" "greater-than")("between" "between")))
					(6 (("propagation" "propagation")
						("backtrack" "backtrack")
					   ;("heuristic" "heuristic")  ;==> NOT IMPLEMENTED
					   ))
                     )
    :icon 486
 (make-cs-one-voice constraint input voices domain percentage-mode percentage cs-mode))

(defmethod! constraint-harmony ((constraint function) (input string) (voice-select string)
								&key (voices '(0)) (domain "pitch") (beats "all") (percentage-mode "off") (percentage 0) (cs-mode "propagation"))
    :initvals '(nil "list" "all-voices" (0) "pitch" "all" "off" 0 "propagation")
    :indoc '( "<lambda-patch>"  "string" "string" "list" "string" "string" "string" "integer" "string")
    :doc "Constraint for one-voice"
    :menuins '((1 (("list" "list") ("n-inputs" "n-inputs")("car-cdr" "car-cdr") ("growing" "growing")))
               (2 (("all-voices" "all-voices") ("voices-list" "voices-list")))
               (4 (("pitch" "pitch")
			       ("pitch-dur" "chords-pitch-dur")
				   ("pitch-onset" "chords-pitch-onset")("pitch-dur-onset" "chords-pitch-dur-onset")
			       ))
               (5 (("all" "all") ("on-beat" "on-beat") ("off-beat" "off-beat") ("1st-beat" "1st-beat")))
               (6 (("off" "off") ("exactly" "exactly") ("less-than" "less-than") ("greater-than" "greater-than")("between" "between")))
			   (8 (("propagation" "propagation")
			       ("backtrack" "backtrack")
			      ;("heuristic" "heuristic") ;==> NOT IMPLEMENTED
			       ))
				)
    :icon 486
 (make-cs-harmony constraint input voice-select voices domain beats percentage-mode percentage cs-mode))

(defmethod! constraint-profile ((bpf-object bpf-lib) (voices list) (approx integer) (range string) &key (scale-time? t) (cs-mode "propagation"))
    :initvals '(nil (0) 4 "voice-range" "propagation")
    :indoc '("bpf or bpf-lib" "list of voice numbers" "integer" "string or list" "t or nil ""string")
    :doc "Constraint profile for voice or voices"
    :menuins '((3 (("voice-range" "voice-range") ("all" "all")))
               (4 (("t" t) ("nil" nil)))	
              ;(5 (("propagation" "propagation") ("backtrack" "backtrack" ) ("heuristic" "heuristic")))
                )
    :icon 486
 (make-cs-profile bpf-object voices approx range scale-time? cs-mode))

(defmethod! constraint-profile ((bpf-object bpf-lib)(voices list) (approx integer)(range list) &key (scale-time? t) (cs-mode "propagation"))
   :initvals '(nil (0) 4 (21 108) "propagation")
   :indoc '("bpf or bpf-lib" "list of voice numbers" "integer" "string or list" "t or nil ""string")
   :doc "Constraint profile for voice or voices"
   :menuins '((3 (("voice-range" "voice-range") ("all" "all")))
                          (4 (("t" t) ("nil" nil)))
              ;(5 (("propagation" "propagation") ("backtrack" "backtrack" ) ("heuristic" "heuristic")))
              )
   :icon 486
 (make-cs-profile bpf-object voices approx range scale-time? cs-mode))

(defmethod! constraint-profile ((bpf-object bpf) (voices list) (approx integer)(range string) &key (scale-time? t) (cs-mode "propagation"))
    :initvals '(nil (0) 4 "voice-range" "propagation")
    :indoc '("bpf or bpf-lib" "list of voice numbers" "integer" "string or list" "t or nil ""string")
    :doc "Constraint profile for voice or voices"
    :menuins '((3 (("voice-range" "voice-range") ("all" "all")))
                      (4 (("t" t) ("nil" nil)))
		       ;(5 (("propagation" "propagation") ("backtrack" "backtrack" ) ("heuristic" "heuristic")))
                       )
    :icon 486
 (make-cs-profile bpf-object voices approx range scale-time? cs-mode))

(defmethod! constraint-profile ((bpf-object bpf) (voices list) (approx integer) (range list) &key (scale-time? t) (cs-mode "propagation"))
   :initvals '(nil (0) 4 (21 108) "propagation")
   :indoc '("bpf or bpf-lib" "list of voice numbers" "integer" "string or list" "t or nil ""string")
   :doc "Constraint profile for voice or voices"
   :menuins '((3 (("voice-range" "voice-range") ("all" "all")))
                     (4 (("t" t) ("nil" nil)))
   	          ;(5 (("propagation" "propagation") ("backtrack" "backtrack" ) ("heuristic" "heuristic")))
                  )
   :icon 486
 (make-cs-profile bpf-object voices approx range scale-time? cs-mode))

 (defmethod! constraint-measure ((constraints t) (measure-number t))
  :initvals '(nil 0)
  :indoc '("screamer-score-constraints or list of constraints" "measure number or list of numbers")
  :doc "Formats the constraint to be applyied to selected measure."
  :icon 486
  (make-cs-measure constraints measure-number))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; AUXILIARY FUNCTIONS
;;
;

(defun make-profile-constraint (bpf-lib voices approx range domain scale-time?)
 (let* ((original-bpfs (if (equal (type-of bpf-lib) 'bpf-lib) (bpf-list bpf-lib) (repeat-n bpf-lib (length (pitch domain)))))
        (pitch-domain (mapcar #'(lambda (x) (mapcar #'first x)) (pitch-dur domain))) ;pitch-variables with rests
        (ratio-domain (om-abs (mapcar #'(lambda (x) (mapcar #'second x)) (pitch-dur domain)))) ;ratios - abs values
        (bpfs (if scale-time? (mapcar #'scale-bpf-time (posn-match ratio-domain voices) original-bpfs)
		                       original-bpfs))
        (midics (mapcar #'sort-list (midics-domain domain))))
 (if (listp range)
     (let ((new-midics (mapcar #'(lambda (midics-list)
		                          (remove-if-not #'(lambda (n)
									                (and (>= n (first range))
													     (<= n (second range))))
									midics-list))
						midics)))
		(midics-profiles-voice-range bpfs voices new-midics pitch-domain approx))

  (if (equal range "voice-range")
      (midics-profiles-voice-range bpfs voices midics pitch-domain approx)
      (midics-profiles-all bpfs voices midics pitch-domain approx)
   )
  )
 )
)

(defun ratios-to-ms (ratios tempo)
 (let* ((whole-note (/ 240000 tempo))
        (ratios-to-ms (mapcar #'(lambda (x)
                                 (coerce x 'double-float))
                              (om* whole-note ratios))))
	 (dx->x 0 ratios-to-ms)))

(defun scale-bpf-time (ratios bpf)	
 (let* ((bpf-y-points (y-points bpf))
        (length-lcm (lcm (length ratios) (length bpf-y-points)))
		(ratios-ms (ratios-to-ms ratios 60))
        (scale-ratios (om-round (om-scale ratios-ms ;(butlast (dx->x (first ratios) ratios))
		                                            0 (1- length-lcm))
					   0))
        (samples (multiple-value-bind (x y z) (om-sample bpf-y-points length-lcm) (third (list x y z)))))
(simple-bpf-from-list scale-ratios (posn-match samples scale-ratios))))

(defun midics-profiles-voice-range (bpfs voices midics pitch-domain approx)
  (let* ((samples (mapcar #'(lambda (bpf pitch-dom)
                            (multiple-value-bind (x y z) (om-sample (y-points bpf) (length pitch-dom))
								                           (third (list x y z))))
			      bpfs (posn-match pitch-domain voices)))
        (notes-positions (mapcar #'(lambda (sample midics-list) (om- (om-round (om-scale sample 1 (length midics-list)) 0) 1)) samples (posn-match midics voices)))
        (midic-profile (mapcar #'(lambda (midics-list positions) (posn-match midics-list positions)) (posn-match midics voices) notes-positions)))
(mapcar #'(lambda (vars midics)
 (profile-constraint vars midics approx)) (posn-match pitch-domain voices) midic-profile)))

(defun midics-profiles-all (bpfs voices midics pitch-domain approx)
 (let* ((sample-all (multiple-value-bind (x y z) (om-sample (make-instance 'bpf-lib :bpf-list bpfs) (length (first pitch-domain))) (third (list x y z))))
          (min-max-all-samples (list (list-min (flat sample-all)) (list-max (flat sample-all))))
          (sample-min-max (mapcar #'(lambda (sample) (list (first min-max-all-samples) (list-min sample) (list-max sample) (second min-max-all-samples))) sample-all))
          (all-midics (remove-duplicates (flat midics)))
          (min-max-positions (mapcar #'(lambda (sample) (om-round (butlast (cdr (om-scale sample 0 (1- (length all-midics))))))) sample-min-max))
          (resamples (mapcar #'(lambda (bpf pitch-dom)
                            (multiple-value-bind (x y z) (om-sample (y-points bpf) (length pitch-dom))
								                           (third (list x y z))))
			      bpfs (posn-match pitch-domain voices)))
          (notes-positions (mapcar #'(lambda (sample min-max-posn) (om-round (om-scale sample (first min-max-posn) (second min-max-posn)))) resamples min-max-positions))
          (midic-profile (mapcar #'(lambda (positions) (posn-match all-midics positions)) notes-positions)))
(mapcar #'(lambda (vars midics)
 (profile-constraint vars midics approx)) (posn-match pitch-domain voices) midic-profile)))

(defun profile-constraint (vars midics-profiles approx)
 (let ((chords? (list-of-listp vars)))
 (mapcar #'(lambda (var midic)
 (cond ((null var) nil)

            (t  (if chords?
                (s::assert! (s::andv (s::>=v (first var) (s::-v midic approx))
                                               (s::<=v (first var) (s::+v midic approx))))

                (s::assert! (s::andv (s::>=v var (s::-v midic approx))
                                               (s::<=v var (s::+v midic approx))))))))
 vars midics-profiles)))

 ;; PERCENTAGE-CONSTRAINT

 (defun split-domain-list1 (list-length n-inputs voice-domain);==> N-INPUTS ((0 1) (1 2) (2 3) ...)
  (let* ((posn (loop for x from 0 to (- list-length n-inputs)
           for y = (arithm-ser x (+ x (1- n-inputs)) 1)
  collect y)))
 (posn-match voice-domain posn)))
	
(defun fix-chords-domain-type (cs-domain-type domain)
(cond ((string-equal "pitch" cs-domain-type) domain)

          ((string-equal "chords-pitch-dur" cs-domain-type)
           (let (res)
            (loop for el in domain 
                     when (not (every #'null (flat (first el))))
                     do (if (equal (first el) (first (first res)))
                              (let ((new-chord-pitch-dur (list (first el) (+ (second el) (second (first res))))))
                               (setf res (x-append (list new-chord-pitch-dur) (cdr res))))
                              (push el res)))
            (reverse res)))

          ((string-equal "chords-pitch-onset" cs-domain-type) 
           (let (res)
            (loop for el in domain
                     when (not (every #'null (flat (first el))))
                     do (if (equal (first el) (first (first res)))
                              (let ((new-chord-pitch-onset (list (first el) (second (first res)))))
                               (setf res (x-append (list new-chord-pitch-onset) (cdr res))))
                              (push el res)))
            (reverse res)))

          ((string-equal "chords-pitch-dur-onset" cs-domain-type) 
                       (let (res)
            (loop for el in domain 
                     when (not (every #'null (flat (first el))))
                     do (if (equal (first el) (first (first res)))
                              (let ((new-chord-pitch-dur-onset (list (first el) (+ (second el) (second (first res))) (third (first res)))))
                               (setf res (x-append (list new-chord-pitch-dur-onset) (cdr res))))
                              (push el res)))
            (reverse res))) 
          (t (om-message-dialog "UNKNOWN CONSTRAINT-HARMONY DOMAIN TYPE!") (om-abort)))) 

;;; *********************************************************************************************  ;;;
;;; ***********************   APPLY-SCREAMER-SCORE-CONSTRAINT   *********************************  ;;;
;;; *********************************************************************************************  ;;;

(defmethod apply-screamer-score-constraint ((score-constraint cs-one-voice) (domain screamer-score-domain))
 (let ((voices-domain (posn-match (get-one-voice-domain score-constraint domain) (get-voices score-constraint))))
  (cond ((not (equal (get-perc-mode score-constraint) "off"));<== PERCENTAGE-CONSTRAINT
         (mapcar #'(lambda (voice)
                    (apply-percentage-constraint score-constraint voice))
          voices-domain))

	  (t (let ((cs-input (get-input score-constraint))
	        (cs-fn (constraint score-constraint)))
	   (mapcar #'(lambda (voice)
                             (apply-contv cs-fn "list" cs-input voice))
                   voices-domain))))))

(defmethod apply-screamer-score-constraint ((score-constraint cs-harmony) (domain screamer-score-domain))
 (let* ((cs-domain-type (get-domain score-constraint))
	   (chords-domain (cond ((string-equal "pitch" cs-domain-type)
 	                         (get-chord-beat score-constraint domain))
						    ((not (string-equal "all" (get-beats score-constraint)))
						          (om-message-dialog "THE CONSTRAINT-HARMONY DOMAIN OPTIONS PITCH-DUR, PITCH-ONSET OR PITCH-DUR-ONSET WORKS ONLY WITH ALL BEATS.")
							      (om-abort))
						    (t (get-chord-domain score-constraint domain)))))
  (cond  ((not (equal (get-perc-mode score-constraint) "off"));<== PERCENTAGE-CONSTRAINT
          (apply-percentage-constraint score-constraint chords-domain))
	  (t (let ((cs-input (get-input score-constraint))
		       (voice-select (get-v-sel score-constraint))
		       (cs-fn (constraint score-constraint)))
		   (cond ((equal voice-select "all-voices");<== ALL-VOICES
                              (apply-contv cs-fn "list" cs-input chords-domain))
			     (t (let* ((voice-numbers (get-voices score-constraint))
				 	       (voices (if (and (atom (car voice-numbers));<== ONLY ONE-VOICE (IN CASE OF DOMAIN-TYPE = "CHORDS")
						                    (= 1 (length voice-numbers)))
						               (car voice-numbers);<== TO AVOID A LIST WITH A SINGLE POSITION
									    voice-numbers)))
				     (if (list-of-listp voice-numbers);<== LIST OF LISTS OF VOICES
				         (let ((voices-chords
							    (fix-chords-domain-type cs-domain-type
                                 (loop for positions in voices
				                       collect (mapcar #'(lambda (chord-domain)
													      (if (string-equal "pitch" cs-domain-type) ;<== PITCH
						 								      (posn-match chord-domain positions)
															  (x-append (list (posn-match (car chord-domain) positions));<== PITCH-DUR/PITCH-ONSET/PITCH-DUR-ONSET
																		      (cdr chord-domain)))) 
						 						 chords-domain)))))
					        (mapcar #'(lambda (vars)
                                       (apply-contv cs-fn "list" cs-input vars))
                             voices-chords))
						(let ((voice-chords
							   (fix-chords-domain-type cs-domain-type
                                (mapcar #'(lambda (chord-domain);<== LIST OF VOICES
								           (if (string-equal "pitch" cs-domain-type) ;<== PITCH
											   (posn-match chord-domain voices)
											   (x-append (list (posn-match (car chord-domain) voices));<== PITCH-DUR/PITCH-ONSET/PITCH-DUR-ONSET
														       (cdr chord-domain)))) 
								 chords-domain))))		 
                        (apply-contv cs-fn "list" cs-input voice-chords)))))))))))

(defmethod apply-screamer-score-constraint ((score-constraint cs-profile) (domain screamer-score-domain))
(make-profile-constraint
  (get-bpf score-constraint)
  (get-voices score-constraint)
  (get-approx score-constraint)
  (get-range score-constraint)
  (if (screamer-all-domains-p domain)
      (var-domain domain)
      domain)
 (scale-time? score-constraint)))

(defmethod apply-screamer-score-constraint ((score-constraint cs-measure) (domain screamer-score-domain))
 (let ((mes-domain (mes-domain domain))
		      (mes-numbers (measure score-constraint))
			  (cs-objects (constraint score-constraint)))
		 (cond ((screamer-score-constraint-p cs-objects);<== ONE CONSTRAINT
		        (if (numberp mes-numbers);<== ONE MEASURE
		            (apply-screamer-score-constraint cs-objects (nth mes-numbers mes-domain))
		            (mapcar #'(lambda (mes-num);<== LIST OF MEASURES
		                      (apply-screamer-score-constraint cs-objects (nth mes-num mes-domain)))
		            mes-numbers)))
			    (t ;<== LIST OF CONSTRAINTS
				(mapcar #'(lambda (cs-obj)
					       (if (numberp mes-numbers);<== ONE MEASURE
							   (apply-screamer-score-constraint cs-obj (nth mes-numbers mes-domain))
		                       (mapcar #'(lambda (mes-num);<== LIST OF MEASURES
				                          (apply-screamer-score-constraint cs-obj (nth mes-num mes-domain)))
				                mes-numbers)))
			    cs-objects))))) 

(defmethod apply-percentage-constraint ((score-constraint cs-one-voice) (voices-domain list))
(let ((cs-input-type (get-input score-constraint)))
 (cond ((string-equal "n-inputs" cs-input-type)
        (percentage-constraint-n-inputs score-constraint voices-domain))
       
	   ((string-equal "growing" cs-input-type)
	    (percentage-constraint-growing score-constraint voices-domain))
		
     (t (om-message-dialog "The percentage constraint ONLY works with <N-INPUTS> and <GROWING> input modes.")
	    (om-abort)))))
	 

(defmethod apply-percentage-constraint ((score-constraint cs-harmony) (chords-domain list))
 (let ((cs-input-type (get-input score-constraint)))
 (if (or (string-equal "list" cs-input-type)
         (string-equal "car-cdr" cs-input-type))
     (progn (om-message-dialog "The percentage constraint ONLY works with <N-INPUTS> and <GROWING> input modes.") (om-abort))
	 (let ((voice-select (get-v-sel score-constraint))
		   (cs-domain-type (get-domain score-constraint)))
 	  (cond ((equal voice-select "all-voices")
	         (if (string-equal "n-inputs" cs-input-type)	
 	             (percentage-constraint-n-inputs score-constraint chords-domain)
				 (percentage-constraint-growing score-constraint chords-domain)))
 	        (t (let* ((voice-numbers (get-voices score-constraint))
				 	  (voices (if (and (atom (car voice-numbers));<== ONLY ONE-VOICE (IN CASE OF DOMAIN-TYPE = "CHORDS")
						               (= 1 (length voice-numbers)))
						          (car voice-numbers);<== TO AVOID A LIST WITH A SINGLE POSITION
								  voice-numbers)))
 			  (if (list-of-listp voices);<== LIST OF LISTS OF VOICE NUMBERS
 		          (let ((voices-chords
							    (fix-chords-domain-type cs-domain-type
                                 (loop for positions in voices
				                       collect (mapcar #'(lambda (chord-domain)
													      (if (string-equal "pitch" cs-domain-type) ;<== PITCH
						 								      (posn-match chord-domain positions)
															  (x-append (list (posn-match (car chord-domain) positions));<== PITCH-DUR/PITCH-ONSET/PITCH-DUR-ONSET
																		      (cdr chord-domain)))) 
						 						 chords-domain)))))
 		           (mapcar #'(lambda (vars)
				   (if (string-equal "n-inputs" cs-input-type)
 		               (percentage-constraint-n-inputs score-constraint vars)
					   (percentage-constraint-growing score-constraint vars)))
 		            voices-chords))
					(let ((voice-chords
						   (fix-chords-domain-type cs-domain-type
                            (mapcar #'(lambda (chord-domain);<== LIST OF VOICES
							           (if (string-equal "pitch" cs-domain-type) ;<== PITCH
										   (posn-match chord-domain voices)
										   (x-append (list (posn-match (car chord-domain) voices));<== PITCH-DUR/PITCH-ONSET/PITCH-DUR-ONSET
													       (cdr chord-domain)))) 
							 chords-domain))))
				   (if (string-equal "n-inputs" cs-input-type)		 
 		               (percentage-constraint-n-inputs score-constraint voice-chords)
					   (percentage-constraint-growing score-constraint voice-chords)))))))))))

(defmethod percentage-constraint-n-inputs ((score-constraint cs-harmony) (chords-domain list))
 (let* ((voice-length (length chords-domain))
        (cs-fn (constraint score-constraint))
        (fn-inputs-length (length (function-lambda-list cs-fn)))
        (apply-length (if (= 1 fn-inputs-length)
                           voice-length
                           (- voice-length (1- fn-inputs-length))))
        (percent (om-round (om* (get-perc score-constraint) (om/ apply-length 100)) 0))
        (percent-cs-mode (get-perc-mode score-constraint))
        (splitted-list (split-domain-list1 voice-length fn-inputs-length chords-domain)))
     (cond ((equal percent-cs-mode "exactly")
                (let* ((test (mapcar #'(lambda (x) (apply cs-fn x)) splitted-list))
                      (truesv (om?::sumv (mapcar #'?::reifyv test))))
                (?::make-equal truesv percent)  ;(s::assert! (s::=v truesv percent))
 				))
 		((equal percent-cs-mode "between")
 		                 (let* ((test (mapcar #'(lambda (x) (apply cs-fn x)) splitted-list))
 		 				        (truesv (om?::sumv (mapcar #'?::reifyv test)))
 		 						)
 		                 (s::assert! (s::andv (s::>=v truesv (first percent))
 						                      (s::<=v truesv (second percent))))
 		 					))
                ((equal percent-cs-mode "less-than")
                 (let* ((test (mapcar #'(lambda (x) (apply cs-fn x)) splitted-list))
 			        (truesv (mapcar #'?::reifyv test))
 					)
                (s::assert! (?::at-mostv (1- percent) #'(lambda (x) (s::=v 1 x)) truesv))
 			))
                ((equal percent-cs-mode "greater-than")
                 (let* ((test (mapcar #'(lambda (x) (apply cs-fn x)) splitted-list))
                         (truesv (mapcar #'?::reifyv test))
 				   )
                 (if (= percent apply-length)
                     (?::make-equal truesv percent)  ;(s::assert! (s::=v truesv percent))
                     (s::assert! (?::at-leastv (1+ percent) #'(lambda (x) (s::=v 1 x)) truesv)))
 				 ))
                (t (om-abort)))))

(defmethod percentage-constraint-growing ((score-constraint cs-harmony) (chords-domain list))
 (let* ((splitted-list (mk-growing chords-domain))
        (cs-fn (constraint score-constraint))
        (apply-length (length splitted-list))
        (percent (om-round (om* (get-perc score-constraint) (om/ apply-length 100)) 0))
        (percent-cs-mode (get-perc-mode score-constraint)))
     (cond ((equal percent-cs-mode "exactly")
                (let* ((test (mapcar #'(lambda (x) (funcall cs-fn x)) splitted-list))
                      (truesv (om?::sumv (mapcar #'?::reifyv test))))
                (?::make-equal truesv percent)  ;(s::assert! (s::=v truesv percent))
 				))
 		((equal percent-cs-mode "between")
 		                 (let* ((test (mapcar #'(lambda (x) (funcall cs-fn x)) splitted-list))
 		 				        (truesv (om?::sumv (mapcar #'?::reifyv test)))
 		 						)
 		                 (s::assert! (s::andv (s::>=v truesv (first percent))
 						                      (s::<=v truesv (second percent))))
 		 					))
                ((equal percent-cs-mode "less-than")
                 (let* ((test (mapcar #'(lambda (x) (funcall cs-fn x)) splitted-list))
 			        (truesv (mapcar #'?::reifyv test))
 					)
                (s::assert! (?::at-mostv (1- percent) #'(lambda (x) (s::=v 1 x)) truesv))
 			))
                ((equal percent-cs-mode "greater-than")
                 (let* ((test (mapcar #'(lambda (x) (funcall cs-fn x)) splitted-list))
                         (truesv (mapcar #'?::reifyv test))
 				   )
                 (if (= percent apply-length)
                     (?::make-equal truesv percent)  ;(s::assert! (s::=v truesv percent))
                     (s::assert! (?::at-leastv (1+ percent) #'(lambda (x) (s::=v 1 x)) truesv)))
 				 ))
                (t (om-abort)))))
								
(defmethod percentage-constraint-n-inputs ((score-constraint cs-one-voice) (voice-domain list))
 (let* ((chords? (list-of-listp voice-domain))
        (voice-length (length voice-domain))
 		(cs-fn (constraint score-constraint))
        (fn-inputs-length (length (function-lambda-list cs-fn)))
        (apply-length (if (= 1 fn-inputs-length)
                           voice-length
                           (- voice-length (1- fn-inputs-length))))
        (percent (om-round (om* (get-perc score-constraint) (om/ apply-length 100)) 0))
        (percent-cs-mode (get-perc-mode score-constraint))
        (splitted-list (if (and (= 1 fn-inputs-length) (not chords?))
                                (flat voice-domain)
                                (split-domain-list1 voice-length fn-inputs-length voice-domain))))
      (cond ((equal percent-cs-mode "exactly")
                 (let* ((test (if (and (= 1 fn-inputs-length) (not chords?))
                                  (mapcar #'(lambda (x) (apply cs-fn (list x))) splitted-list)
                                  (mapcar #'(lambda (x) (apply cs-fn x)) splitted-list)))
 				        (truesv (om?::sumv (mapcar #'?::reifyv test)))
 						)
                 (?::make-equal truesv percent)  ;(s::assert! (s::=v truesv percent))
 					))
 			((equal percent-cs-mode "between")
 			                 (let* ((test (if (and (= 1 fn-inputs-length) (not chords?))
 			                                  (mapcar #'(lambda (x) (apply cs-fn (list x))) splitted-list)
 			                                  (mapcar #'(lambda (x) (apply cs-fn x)) splitted-list)))
 			 				        (truesv (om?::sumv (mapcar #'?::reifyv test)))
 			 						)
 			                 (s::assert! (s::andv (s::>=v truesv (first percent))
 							                      (s::<=v truesv (second percent))))
 			 					))
                 ((equal percent-cs-mode "less-than")
                  (let* ((test (if (and (= 1 fn-inputs-length) (not chords?))
                                   (mapcar #'(lambda (x) (apply cs-fn (list x))) splitted-list)
                                   (mapcar #'(lambda (x) (apply cs-fn x)) splitted-list)))
   				  (truesv (mapcar #'?::reifyv test))
 						)
                 (s::assert! (?::at-mostv (1- percent) #'(lambda (x) (s::=v 1 x)) truesv))
 					))
                 ((equal percent-cs-mode "greater-than")
                  (let* ((test (if (and (= 1 fn-inputs-length) (not chords?))
                                   (mapcar #'(lambda (x) (apply cs-fn (list x))) splitted-list)
                                   (mapcar #'(lambda (x) (apply cs-fn x)) splitted-list)))
   				  (truesv (mapcar #'?::reifyv test))
 						)
                  (if (= percent apply-length)
                      (?::make-equal truesv percent)  ;(s::assert! (s::=v truesv percent))
                      (s::assert! (?::at-leastv (1+ percent) #'(lambda (x) (s::=v 1 x)) truesv)))
 					 ))
                 (t (om-abort)))))

 (defmethod percentage-constraint-growing ((score-constraint cs-one-voice) (voice-domain list))
  (let* ((splitted-list (mk-growing voice-domain))
  		 (cs-fn (constraint score-constraint))
         (apply-length (length splitted-list))
         (percent (om-round (om* (get-perc score-constraint) (om/ apply-length 100)) 0))
         (percent-cs-mode (get-perc-mode score-constraint)))
       (cond ((equal percent-cs-mode "exactly")
                  (let* ((test (mapcar #'(lambda (x) (apply cs-fn (list x))) splitted-list))
  				        (truesv (om?::sumv (mapcar #'?::reifyv test)))
  						)
                 (?::make-equal truesv percent)                  
               ;(s::assert! (s::=v truesv percent))
  					))
  			((equal percent-cs-mode "between")
  			                 (let* ((test (mapcar #'(lambda (x) (apply cs-fn (list x))) splitted-list))
  			 				        (truesv (om?::sumv (mapcar #'?::reifyv test)))
  			 						)
  			                 (s::assert! (s::andv (s::>=v truesv (first percent))
  							                      (s::<=v truesv (second percent))))
  			 					))
                  ((equal percent-cs-mode "less-than")
                   (let* ((test (mapcar #'(lambda (x) (apply cs-fn (list x))) splitted-list))
    				      (truesv (mapcar #'?::reifyv test))
  						 )
                  (s::assert! (?::at-mostv (1- percent) #'(lambda (x) (s::=v 1 x)) truesv))
  					))
                  ((equal percent-cs-mode "greater-than")
                   (let* ((test (mapcar #'(lambda (x) (apply cs-fn (list x))) splitted-list))
    				  (truesv (mapcar #'?::reifyv test))
  						)
                   (if (= percent apply-length)
                       (?::make-equal truesv percent) ; (s::assert! (s::=v truesv percent))
                       (s::assert! (?::at-leastv (1+ percent) #'(lambda (x) (s::=v 1 x)) truesv)))
  					 ))
                  (t (om-abort)))))
;;; *********************************************************************************************************************  ;;;

