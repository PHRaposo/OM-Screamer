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
(handler-bind ((error #'(lambda (c)
                       (when *msg-error-label-on*
                         (om-message-dialog (string+ "Error while evaluating the function " "compile-screamer-backtrack-constraint" " : "
                                                  (om-report-condition c))
                                            :size (om-make-point 300 200))
                         (om-abort)))))
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
    ,lambda-list (om?::any-fn ,fun ,@lambda-list)))))))

;;; MAIN-INTERFACE

(defmethod! constraint-one-voice ((constraint function) (input string)(voices list)(domain string)
	                               &key (rests "exclude") (percentage-mode "off") (percentage 0) (cs-mode "propagation"))
    :initvals '(nil "list" (0 1) "pitch" "exclude" "off" 0 "propagation")
    :indoc '( "<lambda-patch>" "list" "list-of-voice-numbers" "string" "string" "string" "number" "string")
    :doc "Constraint for one-voice"
    :menuins '((1 (("list" "list") ("n-inputs" "n-inputs") ("car-cdr" "car-cdr") ("growing" "growing")))
                     (3 (
                       ("pitch" "pitch")
                       ("pitch-dur" "pitch-dur")
                       ("pitch-onset" "pitch-onset")
                       ("pitch-dur-onset" "pitch-dur-onset")
                          )
                        )
					(4 (("include" "include") ("exclude" "exclude")))	
                    (5 (("off" "off") ("exactly" "exactly") ("less-than" "less-than") ("greater-than" "greater-than")("between" "between")))
					(7 (("propagation" "propagation")
						("backtrack" "backtrack")
					   ;("heuristic" "heuristic")  ;==> NOT IMPLEMENTED
					   ))
                     )
    :icon 486
 (make-cs-one-voice constraint input voices domain rests percentage-mode percentage cs-mode))

(defmethod! constraint-harmony ((constraint function) (input string) (voice-select string)
								&key (voices '(0)) (domain "pitch") (rests "exclude") (beats "all") (percentage-mode "off") (percentage 0) (cs-mode "propagation"))
    :initvals '(nil "list" "all-voices" (0) "pitch" "exclude" "all" "off" 0 "propagation")
    :indoc '( "<lambda-patch>"  "string" "string" "list" "string" "string" "string" "string" "integer" "string")
    :doc "Constraint for one-voice"
    :menuins '((1 (("list" "list") ("n-inputs" "n-inputs")("car-cdr" "car-cdr") ("growing" "growing")))
               (2 (("all-voices" "all-voices") ("voices-list" "voices-list")))
               (4 (("pitch" "pitch")
			       ("pitch-dur" "chords-pitch-dur")
				   ("pitch-onset" "chords-pitch-onset")("pitch-dur-onset" "chords-pitch-dur-onset")
			       ))
			   (5 (("include" "include") ("exclude" "exclude")))   
               (6 (("all" "all") ("on-beat" "on-beat") ("off-beat" "off-beat") ("1st-beat" "1st-beat")))
               (7 (("off" "off") ("exactly" "exactly") ("less-than" "less-than") ("greater-than" "greater-than")("between" "between")))
			   (9 (("propagation" "propagation")
			       ("backtrack" "backtrack")
			      ;("heuristic" "heuristic") ;==> NOT IMPLEMENTED
			       ))
				)
    :icon 486
 (make-cs-harmony constraint input voice-select voices domain rests beats percentage-mode percentage cs-mode))

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


 (defmethod! constraint-measures ((constraints t) (measure-number list) (type string) (voices list) (domain string)) ;(rests string))
  :initvals '(nil (0 1) "one-voice" (0 1) "pitch") ;"include")
  :indoc '("screamer-score-constraints" "measure number or list of numbers" "string" "list" "string") ;"string")
  :menuins '((2 (("one-voice" "one-voice") ("harmony" "harmony")))
             (4 (("pitch" "pitch")
                 ("pitch-dur" "pitch-dur")
                 ("pitch-onset" "pitch-onset")
                 ("pitch-dur-onset" "pitch-dur-onset")))
			 ;(5 (("include" "include") ("exclude" "exclude"))) ;; Needs work: exclude rests
			 )
  :doc "Formats the constraint to be applyied to selected measures at the same time. 
  The number of inputs must be the same as the number of voices and measures.
  Ex.: For measures '(0 1 2) and voices (0 1 2), the constraint must have three inputs and will be applied to measure 0 and voice 0, measure 1 and voice 1,
measure 2 and voice 2. Each input will return all events in those measures, including rests."
  :icon 486
  (make-cs-measures constraints measure-number type voices domain "include")) ; rests))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; AUXILIARY FUNCTIONS
;;
;

(defun make-profile-constraint (bpf-lib voices approx range domain scale-time?)
 (let* ((original-bpfs (if (equal (type-of bpf-lib) 'bpf-lib)
                           (bpf-list bpf-lib)
	                       (repeat-n bpf-lib (if (listp domain)
							                     (length (pitch (car domain)))
							                     (length (pitch domain))))))
        (pitch-domain  (if (listp domain) ;pitch-variables with rests <== LIST OF DOMAINS
						   (let ((measures (mat-trans (mapcar #'pitch-dur-include domain))))
						      (loop for voice in measures
							        collect (mapcar #'first (rec-merge-domain voice (length voice)))))
							(mapcar #'(lambda (x)(mapcar #'first x));<== ONE DOMAIN 
							                      (pitch-dur-include domain))))              
        (ratio-domain (if (listp domain) ;ratios - abs values <== LIST OF DOMAINS
						   (let ((measures (mat-trans (mapcar #'pitch-dur-include domain))))		 
							  (loop for voice in measures 
						            collect (om-abs (mapcar #'second (rec-merge-domain voice (length voice))))))
				      (om-abs (mapcar #'(lambda (x) (mapcar #'second x));<== ONE DOMAIN 
					                                 (pitch-dur-include domain))))) 
        (bpfs (if scale-time? (mapcar #'scale-bpf-time (posn-match ratio-domain voices) original-bpfs)
		                       original-bpfs))
        (midics (if (listp domain)
			         (mapcar #'sort-list (midics-domain (car domain)))
		             (mapcar #'sort-list (midics-domain domain)))))
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
        (scale-ratios (remove nil (om-round (om-scale ratios-ms ;(butlast (dx->x (first ratios) ratios))
		                                            0 (1- length-lcm))
					   0))) ;REMOVE NIL FOR NOW!
        (samples (multiple-value-bind (x y z) (om-sample bpf-y-points length-lcm) (third (list x y z)))))
;(print (list "SCALE-RATIOS" scale-ratios)) (print (list "SAMPLES" samples))
(simple-bpf-from-list scale-ratios (remove nil (posn-match samples scale-ratios)))))

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
                (s::assert! (s::andv (s::>=v (car var) (s::-v midic approx))
                                               (s::<=v (car var) (s::+v midic approx))))

                (s::assert! (s::andv (s::>=v var (s::-v midic approx))
                                               (s::<=v var (s::+v midic approx))))))))
 vars midics-profiles)))

 ;; PERCENTAGE-CONSTRAINT
	
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

(defmethod apply-screamer-score-constraint ((score-constraint cs-one-voice) (domain t)) ;<== OBJECT OR LIST ;(domain screamer-score-domain))
 (let ((voices-domain
		(posn-match (get-one-voice-domain score-constraint domain) (get-voices score-constraint))))
  (cond ((not (equal (get-perc-mode score-constraint) "off"));<== PERCENTAGE-CONSTRAINT
         (mapcar #'(lambda (voice)
                    (apply-percentage-constraint score-constraint voice))
          voices-domain))

	  (t (let ((cs-input (get-input score-constraint))
	        (cs-fn (constraint score-constraint)))
	   (mapcar #'(lambda (voice)
                             (apply-contv cs-fn "list" cs-input voice))
                   voices-domain))))))

(defmethod apply-screamer-score-constraint ((score-constraint cs-harmony) (domain t));<== OBJECT OR LIST (domain screamer-score-domain))
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

(defmethod apply-screamer-score-constraint ((score-constraint cs-profile) (domain t)) ;screamer-score-domain))
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
								(if (listp mes-num)
									(apply-screamer-score-constraint cs-objects (posn-match mes-domain mes-num))
		                            (apply-screamer-score-constraint cs-objects (nth mes-num mes-domain))))
		            mes-numbers)))
			    (t ;<== LIST OF CONSTRAINTS
				(mapcar #'(lambda (cs-obj)
					       (if (numberp mes-numbers);<== ONE MEASURE
							   (apply-screamer-score-constraint cs-obj (nth mes-numbers mes-domain))
		                       (mapcar #'(lambda (mes-num);<== LIST OF MEASURES
				                          (if (listp mes-num)
											  (apply-screamer-score-constraint cs-obj (posn-match mes-domain mes-num))
											  (apply-screamer-score-constraint cs-obj (nth mes-num mes-domain))))
				                mes-numbers)))
			    cs-objects))))) 

(defmethod apply-screamer-score-constraint ((score-constraint cs-measures) (domain t)) ;<== OBJECT OR LIST ;(domain screamer-score-domain))
 (let* ((mes-domain (mes-domain domain))
	    (mes-numbers (measures score-constraint))
		(cs-type (cs-type score-constraint))
		(voices (get-voices score-constraint))
		(new-obj (if (string-equal "one-voice" cs-type)
		             (make-cs-one-voice (constraint score-constraint) 
												"list"
											   (get-voices score-constraint)
											   (get-domain score-constraint)
											   (get-rests score-constraint)
											   "off"
											   nil 
											   "propagation")
		             (make-cs-harmony (constraint score-constraint) 
									   "list"
									   "voices-list"
									   voices
									  (let ((d (get-domain score-constraint)))
									   (if (string-equal "pitch" d)
										    d 
										   (concatenate 'string "chords-" d)))
									  (get-rests score-constraint)
									   "all"
									   "off"
										nil 
									   "propagation")))								   
        (domains (loop for voice in voices 
					   for mes-num in mes-numbers
					   collect (if (string-equal "one-voice" cs-type)
					               (nth voice (get-one-voice-domain new-obj (if (listp mes-num)
									                                             (posn-match mes-domain mes-num)
																				 (nth mes-num mes-domain))))
							   (let ((mes (if (listp mes-num)
									          (posn-match mes-domain mes-num)
											  (nth mes-num mes-domain))))
								(cond ((string-equal "pitch" cs-type)
								       (get-chord-beat new-obj mes))
						              (t (get-chord-domain new-obj mes)))))))
		(fn-inputs (length (function-lambda-list (constraint score-constraint))))							  
		(list-inputs (arithm-ser 0 (1- fn-inputs) 1)))
 (screamer::assert! (apply (constraint new-obj)
	                  (mapcar #'(lambda (n)
			  	                 (nth n domains))
				        list-inputs)))))
				   
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
                       (truesv (apply #'screamer::count-truesv test))
                        ;(om?::sumv (mapcar #'?::reifyv test)))
 						)
                (screamer::attach-noticer! nil splitted-list :dependencies (list truesv))
                (?::make-equal truesv percent)  ;(s::assert! (s::=v truesv percent))
 				))
 		((equal percent-cs-mode "between")
 		                 (let* ((test (mapcar #'(lambda (x) (apply cs-fn x)) splitted-list))
 		 				         (truesv (apply #'screamer::count-truesv test))
                        ;(om?::sumv (mapcar #'?::reifyv test)))
 						)
                (screamer::attach-noticer! nil splitted-list :dependencies (list truesv))
 		                 (s::assert! (s::andv (s::>=v truesv (first percent))
 						                      (s::<=v truesv (second percent))))
 		 					))
                 ((equal percent-cs-mode "less-than")
                 (let* ((test (mapcar #'(lambda (x) (apply cs-fn x)) splitted-list))
 			         (truesv (apply #'screamer::count-truesv test))
                        ;(om?::sumv (mapcar #'?::reifyv test)))
 						)
                (screamer::attach-noticer! nil splitted-list :dependencies (list truesv))
                (s::assert! (s::<=v truesv (1- percent))) ;(?::at-mostv (1- percent) #'(lambda (x) (s::=v 1 x)) truesv))
 			))
                ((equal percent-cs-mode "greater-than")
                 (let* ((test (mapcar #'(lambda (x) (apply cs-fn x)) splitted-list))
                         (truesv (apply #'screamer::count-truesv test))
                        ;(om?::sumv (mapcar #'?::reifyv test)))
 						)
                (screamer::attach-noticer! nil splitted-list :dependencies (list truesv))
                 (if (= percent apply-length)
                     (?::make-equal truesv percent)  ;(s::assert! (s::=v truesv percent))
                     (s::assert! (s::>=v truesv (1+ percent)))) ;(?::at-leastv (1+ percent) #'(lambda (x) (s::=v 1 x)) truesv)))
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
                     (truesv (apply #'screamer::count-truesv test))
                        ;(om?::sumv (mapcar #'?::reifyv test)))
 						)
                (screamer::attach-noticer! nil splitted-list :dependencies (list truesv))
                (?::make-equal truesv percent)  ;(s::assert! (s::=v truesv percent))
 				))
 		((equal percent-cs-mode "between")
 		                 (let* ((test (mapcar #'(lambda (x) (funcall cs-fn x)) splitted-list))
 		 				        (truesv (apply #'screamer::count-truesv test))
                        ;(om?::sumv (mapcar #'?::reifyv test)))
 						)
               (screamer::attach-noticer! nil splitted-list :dependencies (list truesv))
 		                 (s::assert! (s::andv (s::>=v truesv (first percent))
 						                      (s::<=v truesv (second percent))))
 		 					))
                 ((equal percent-cs-mode "less-than")
                 (let* ((test (mapcar #'(lambda (x) (apply cs-fn x)) splitted-list))
 			         (truesv (apply #'screamer::count-truesv test))
                        ;(om?::sumv (mapcar #'?::reifyv test)))
 						)
                (screamer::attach-noticer! nil splitted-list :dependencies (list truesv))
                (s::assert! (s::<=v truesv (1- percent))) ;(?::at-mostv (1- percent) #'(lambda (x) (s::=v 1 x)) truesv))
 			))
                ((equal percent-cs-mode "greater-than")
                 (let* ((test (mapcar #'(lambda (x) (apply cs-fn x)) splitted-list))
                         (truesv (apply #'screamer::count-truesv test))
                        ;(om?::sumv (mapcar #'?::reifyv test)))
 						)
                (screamer::attach-noticer! nil splitted-list :dependencies (list truesv))
                 (if (= percent apply-length)
                     (?::make-equal truesv percent)  ;(s::assert! (s::=v truesv percent))
                     (s::assert! (s::>=v truesv (1+ percent)))) ;(?::at-leastv (1+ percent) #'(lambda (x) (s::=v 1 x)) truesv)))
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
 				        (truesv (apply #'screamer::count-truesv test))
                        ;(om?::sumv (mapcar #'?::reifyv test)))
 						)
                (screamer::attach-noticer! nil splitted-list :dependencies (list truesv))
                 (?::make-equal truesv percent)  ;(s::assert! (s::=v truesv percent))
 					))
 			((equal percent-cs-mode "between")
 			                 (let* ((test (if (and (= 1 fn-inputs-length) (not chords?))
 			                                  (mapcar #'(lambda (x) (apply cs-fn (list x))) splitted-list)
 			                                  (mapcar #'(lambda (x) (apply cs-fn x)) splitted-list)))
 			 				        (truesv (apply #'screamer::count-truesv test))
                        ;(om?::sumv (mapcar #'?::reifyv test)))
 						)
                (screamer::attach-noticer! nil splitted-list :dependencies (list truesv))
 			                 (s::assert! (s::andv (s::>=v truesv (first percent))
 							                      (s::<=v truesv (second percent))))
 			 					))
                  ((equal percent-cs-mode "less-than")
                 (let* ((test (mapcar #'(lambda (x) (apply cs-fn x)) splitted-list))
 			         (truesv (apply #'screamer::count-truesv test))
                        ;(om?::sumv (mapcar #'?::reifyv test)))
 						)
                (screamer::attach-noticer! nil splitted-list :dependencies (list truesv))
                (s::assert! (s::<=v truesv (1- percent))) ;(?::at-mostv (1- percent) #'(lambda (x) (s::=v 1 x)) truesv))
 			))
                ((equal percent-cs-mode "greater-than")
                 (let* ((test (mapcar #'(lambda (x) (apply cs-fn x)) splitted-list))
                         (truesv (apply #'screamer::count-truesv test))
                        ;(om?::sumv (mapcar #'?::reifyv test)))
 						)
                (screamer::attach-noticer! nil splitted-list :dependencies (list truesv))
                 (if (= percent apply-length)
                     (?::make-equal truesv percent)  ;(s::assert! (s::=v truesv percent))
                     (s::assert! (s::>=v truesv (1+ percent)))) ;(?::at-leastv (1+ percent) #'(lambda (x) (s::=v 1 x)) truesv)))
 				 ))
                 (t (om-abort)))))


#|
;;; NEW
(defmethod percentage-constraint-n-inputs ((score-constraint cs-harmony) (chords-domain list))
 (let* ((voice-length (length chords-domain))
        (cs-fn (constraint score-constraint))
        (fn-inputs-length (length (function-lambda-list cs-fn)))
        (apply-length (if (= 1 fn-inputs-length)
                           voice-length
                           (- voice-length (1- fn-inputs-length))))
        (percent (om-round (om* (get-perc score-constraint) (om/ apply-length 100)) 0))
        (percent-cs-mode (get-perc-mode score-constraint))
        (splitted-list (split-domain-list1 voice-length fn-inputs-length chords-domain))
        (test (mapcar #'(lambda (x) (list (apply cs-fn x) x)) splitted-list))
        ;; (shuffled-test (permut-random test))
        (truesv (apply #'screamer::count-truesv (mapcar #'first test))))
   (let* ((user-perc (get-perc score-constraint))
          (window-size (if (>= user-perc 50)
                           2
                           (max 2 (floor (/ 100 (max 1 user-perc))))))
          (window-threshold 1)
          (windows (if (>= (length test) window-size)
                       (group-list test window-size 'linear)
                       nil))
          ;; (windows (if (>= (length test) window-size)
          ;;              (loop for i from 0 to (- (length test) window-size)
          ;;                    collect (subseq test i (+ i window-size)))
          ;;              nil))
          (window-tests
            (if windows
                (mapcar
                  (lambda (window)
                    (let* ((bools (mapcar #'first window))
                           (vars (remove-duplicates (flat (mapcar #'second window)) :test #'equal))
                           (window-truev (apply #'screamer::count-truesv bools))
                           (window-test (s::>=v window-truev window-threshold)))
                      (assert! window-test)
                      ;(screamer::attach-noticer! #'(lambda () (when (s::bound? window-test) (print (s::value-of window-test)))) window-test)
                      (screamer::attach-noticer! #'(lambda () nil) vars :dependencies (list window-test))
                      window-test))
                  windows)
                nil)))
     (s::assert! (s::equalv truesv percent))
     )))

(defmethod percentage-constraint-growing ((score-constraint cs-harmony) (chords-domain list))
 (let* ((splitted-list (mk-growing chords-domain))
        (cs-fn (constraint score-constraint))
        (apply-length (length splitted-list))
        (percent (om-round (om* (get-perc score-constraint) (om/ apply-length 100)) 0))
        (percent-cs-mode (get-perc-mode score-constraint))
        (test (mapcar #'(lambda (x) (list (funcall cs-fn x) x)) splitted-list))
        ;; (shuffled-test (permut-random test))
        (truesv (apply #'screamer::count-truesv (mapcar #'first test))))
   (let* ((user-perc (get-perc score-constraint))
          (window-size (if (>= user-perc 50)
                           2
                           (max 2 (floor (/ 100 (max 1 user-perc))))))
          (window-threshold 1)
          (windows (if (>= (length test) window-size)
                       (group-list test window-size 'linear)
                       nil))
          ;; (windows (if (>= (length test) window-size)
          ;;              (loop for i from 0 to (- (length test) window-size)
          ;;                    collect (subseq test i (+ i window-size)))
          ;;              nil))
          (window-tests
            (if windows
                (mapcar
                  (lambda (window)
                    (let* ((bools (mapcar #'first window))
                           (vars (remove-duplicates (flat (mapcar #'second window)) :test #'equal))
                           (window-truev (apply #'screamer::count-truesv bools))
                           (window-test (s::>=v window-truev window-threshold)))
                      (assert! window-test)
                      ;(screamer::attach-noticer! #'(lambda () (when (s::bound? window-test) (print (s::value-of window-test)))) window-test)
                      (screamer::attach-noticer! #'(lambda () nil) vars :dependencies (list window-test))
                      window-test))
                  windows)
                nil)))
     (s::assert! (s::equalv truesv percent)))))

(defmethod percentage-constraint-growing ((score-constraint cs-one-voice) (voice-domain list))
  (let* ((splitted-list (mk-growing voice-domain))
         (cs-fn (constraint score-constraint))
         (apply-length (length splitted-list))
         (percent (om-round (om* (get-perc score-constraint) (om/ apply-length 100)) 0))
         (percent-cs-mode (get-perc-mode score-constraint))
         (test (mapcar #'(lambda (x) (list (apply cs-fn (list x)) (list x))) splitted-list))
         ;; (shuffled-test (permut-random test))
         (truesv (apply #'screamer::count-truesv (mapcar #'first test))))
    (let* ((user-perc (get-perc score-constraint))
           (window-size (if (>= user-perc 50)
                            2
                            (max 2 (floor (/ 100 (max 1 user-perc))))))
           (window-threshold 1)
           (windows (if (>= (length test) window-size)
                        (group-list test window-size 'linear)
                        nil))
           ;; (windows (if (>= (length test) window-size)
           ;;              (loop for i from 0 to (- (length test) window-size)
           ;;                    collect (subseq test i (+ i window-size)))
           ;;              nil))
           (window-tests
             (if windows
                 (mapcar
                   (lambda (window)
                     (let* ((bools (mapcar #'first window))
                            (vars (remove-duplicates (flat (mapcar #'second window)) :test #'equal))
                            (window-truev (apply #'screamer::count-truesv bools))
                            (window-test (s::>=v window-truev window-threshold)))
                       (assert! window-test)
                       ;(screamer::attach-noticer! #'(lambda () (when (s::bound? window-test) (print (s::value-of window-test)))) window-test)
                       (screamer::attach-noticer! #'(lambda () nil) vars :dependencies (list window-test))
                       window-test))
                   windows)
                 nil)))
      (s::assert! (s::equalv truesv percent)))))

(defmethod percentage-constraint-n-inputs ((score-constraint cs-one-voice) (voice-domain list))
 (let* ((chords? (list-of-listp voice-domain))
        (voice-length (length voice-domain))
        (cs-fn (constraint score-constraint))
        (fn-inputs-length (length (function-lambda-list cs-fn)))
        (apply-length (if (= 1 fn-inputs-length)
                           voice-length
                           (- voice-length (1- fn-inputs-length))))
        (percent (ceiling (om* (get-perc score-constraint) (om/ apply-length 100))));
        (percent-cs-mode (get-perc-mode score-constraint))
        (splitted-list (if (and (= 1 fn-inputs-length) (not chords?))
                               (flat voice-domain)
                               (split-domain-list1 voice-length fn-inputs-length voice-domain)))
        (test (if (and (= 1 fn-inputs-length) (not chords?))
                  (mapcar #'(lambda (x) (list (apply cs-fn (list x)) (list x))) splitted-list)
                  (mapcar #'(lambda (x) (list (apply cs-fn x) x)) splitted-list)))
        ;; (shuffled-test (permut-random test))
        (truesv (apply #'screamer::count-truesv (mapcar #'first test))))
   (let* ((user-perc (get-perc score-constraint))
           (window-size (if (>= user-perc 50)
                            2
                            (max 2 (floor (/ 100 (max 1 user-perc))))))
          (window-threshold 1)
          (windows (if (>= (length test) window-size)
                       (group-list test window-size 'linear)
                       nil))
          ;; (windows (if (>= (length test) window-size)
          ;;              (loop for i from 0 to (- (length test) window-size)
          ;;                    collect (subseq test i (+ i window-size)))
          ;;              nil))
          (window-tests
            (if windows
                (mapcar
                  (lambda (window)
                    (let* ((bools (mapcar #'first window))
                           (vars (remove-duplicates (flat (mapcar #'second window)) :test #'equal))
                           (window-truev (apply #'screamer::count-truesv bools))
                           (window-test (s::>=v window-truev window-threshold)))
                      (s::assert! window-test)
                      ;(screamer::attach-noticer! #'(lambda () (when (s::bound? window-test) (print (s::value-of window-test)))) window-test)
                      (screamer::attach-noticer! #'(lambda () nil) vars :dependencies (list window-test))
                      window-test))
                  windows)
                nil)))
     (s::assert! (s::equalv truesv percent)))))
|#
;;;*********************************************************************************************************************  ;;;

