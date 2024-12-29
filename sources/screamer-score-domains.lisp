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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOMAIN

; AUXILIARY FUNCTIONS

(defun positions (input-list input-elem) ;FROM OM-TRISTAN
  (let ((index 0) res)
    (dolist (n input-list)
      (if (equal input-elem n)  (push index res)) ;modified to equal
      (setq index (1+ index)))
    (nreverse res)))

(defmethod locked-voice? ((self voice))
 (let ((voice-chords (remove-duplicates (flat (mapcar #'lmidic (chords self))))))
  (not (and (or (null (first voice-chords)) (= 6000 (first voice-chords)))
            (= 1 (length voice-chords))))))
			
(defun new-domain-pitch-dur (voices domains mcs-approx random?) ;<== NEW 12.09.2024
 (loop for voice in voices
       for x from 0
          if (locked-voice? voice)
          collect  (let* ((ratios (tree2ratio (tree voice)))
                               (all-rests? (null (chords voice)))
                               (chords (if all-rests?   
                                                (repeat-n nil (length (remove-if #'(lambda (x) (minusp x)) ratios)))
                                                (chords voice))))
                      (loop for ratio in ratios
                                collect (if (minusp ratio)
                                               (list nil ratio)
                                                (let ((notes (if all-rests?
                                                                     (pop chords)
                                                                     (om/ (lmidic (pop chords)) 100))))
                                                (cond ((null notes) (list (s::variablize notes) ratio))
                                                          ((= (length notes) 1) (x-append (s::variablize (car notes)) ratio))
                                                          (t (list (mapcar #'s::variablize notes) ratio)))))))
         else 
         collect (let* ((ratios (tree2ratio (tree voice)))
                           (domain (let ((dom (pop domains))) 
                                          (if (null dom)
                                              (progn (om-message-dialog (format nil "A DOMAIN IS MISSING FOR VOICE NUMBER ~A." x))
                                                (om-abort))
                                               dom)))
                           (voice-domain (if (= 2 mcs-approx) (mapcar #'round (domain domain)) (mapcar #'float (domain domain)))))
                     (if (string-equal "notes" (domain-type domain))
                         (loop for ratio in ratios
                                   collect (if (minusp ratio)
                                                  (list nil ratio)
                                                  (if random?
                                                     (list (om?::a-random-member-ofv voice-domain) ratio)
                                                     (list (s::a-member-ofv  voice-domain) ratio))))
                     (let* ((n-notes (let ((nums (n-notes domain)))
					                  (if (null nums)
                                           (progn (om-message-dialog "THE THIRD (OPTIONAL) ARGUMENT FOR SCREAMER-SCORE-DOMAIN SHOULD BE A NUMBER OR A LIST OF NUMBERS.")
                                             (om-abort))
									   nums)))
                             (n-chords (length (remove-if #'(lambda (x) (< x 0)) ratios)))
                             (chords-correct-length (if (listp n-notes) 
                                                                     (flat (group-list (n-notes domain) (list n-chords) 'circular))
                                                                      (repeat-n n-notes n-chords)))
                             (voice-domain (if (= 2 mcs-approx) (domain domain) (mapcar #'float (domain domain))))
                            (chords (om?::list-of-midi-chords-inv chords-correct-length mcs-approx voice-domain random?)))
                                         (loop for ratio in ratios
                                                  collect (if (minusp ratio)
                                                                (list nil ratio)
                                                                (list (pop chords) ratio))))))))

(defun get-measure-rest-places (measure)
 (let ((tree (list '? (list (tree measure)))))
  (get-rest-places tree)))

(defun ratios2onsets (ratios)
(butlast (dx->x 0 (om-abs ratios))))

(defun ratios2notes-posn (ratios)
(arithm-ser 0 (1- (length ratios)) 1))

(defun posn-in-onsets-list (voice-onsets-posn all-onsets)
(posn-in-onsets-list-internal voice-onsets-posn all-onsets nil))

(defun posn-in-onsets-list-internal (voice-onsets-posn all-onsets accumul)
;(print (format nil "ONSET: ~a" (car (first voice-onsets-posn))))
;(print (format nil "POSITION: ~a" (car (second voice-onsets-posn))))
;(print (format nil "OFFSET: ~a" (car (third voice-onsets-posn))))
;(print (format nil "CURR-ONSET: ~a" (car all-onsets)))
;(print (format nil "RESULTS: ~a" (reverse accumul)))
;(print (format nil "ONSET - CURR-ONSET = ~a" (- (car (first voice-onsets-posn)) (car all-onsets))))
;;NOTE: voice-onset-posn
;;first = ONSETS (not needed anymore ?)
;;second = POSITIONS
;;third = OFFSETS (NEW)
(if all-onsets
   (cond
	   ((null (first (third voice-onsets-posn)))
	   (posn-in-onsets-list-internal (x-append (list nil)
                                               (list nil) 
											   (list nil))
                                                 (cdr all-onsets)
                                                 (x-append (list nil) accumul)))
	;((= 1 (length (second voice-onsets-posn)) )
    ;(posn-in-onsets-list-internal voice-onsets-posn
    ;                                             (cdr all-onsets)
    ;                                             (x-append (first (second voice-onsets-posn)) accumul)))

   ((< (first all-onsets) (first (third voice-onsets-posn)));(second (first voice-onsets-posn)) ) ;
    (posn-in-onsets-list-internal voice-onsets-posn
                                                 (cdr all-onsets)
                                                 (x-append (first (second voice-onsets-posn)) accumul)))

  ((= (first all-onsets) (first (third voice-onsets-posn)));(second (first voice-onsets-posn)))
     (posn-in-onsets-list-internal (x-append (list (cdr (first voice-onsets-posn)))
                                             (list (cdr (second voice-onsets-posn)))  
											 (list (cdr (third voice-onsets-posn))))
                                                 (cdr all-onsets)
                                                 (x-append (second (second voice-onsets-posn)) accumul))))
												

	(reverse accumul))

	)

(defun remove-rest-posn (rest-places positions)
 (remove-if #'(lambda (x) (member x rest-places)) positions))

(defun get-beats-from-time-sig (time-sig)
   (repeat-n (/ 1 (second time-sig))
                  (first time-sig)))

(defun get-max-length-time-sig (voices)  ;<== NEW (05/09/2024
 (let* ((time-sigs (mapcar #'get-time-sig voices))
        (time-sig-lengths (mapcar #'length time-sigs))
		(max-posn (position (list-max time-sig-lengths) time-sig-lengths)))
  (nth max-posn time-sigs)))
  
(defun get-beats-offbeats (voices all-onsets pitch-variables-all-onsets)
 (let* ((time-sig (get-max-length-time-sig voices))  ;<== NEW (05/09/2024) ;(get-time-sig (first voices)))
          (beats (mapcar #'get-beats-from-time-sig time-sig))
          (beats-length (mapcar #'length beats))
	  (first-beats-posn-posn (dx->x 0 (butlast beats-length)))
          (beats-onsets (butlast (dx->x 0 (flat beats))))
          (beats-posn (mapcar #'(lambda (x) (position x all-onsets)) beats-onsets))
	  (chords (mat-trans pitch-variables-all-onsets))
          (offbeats (remove-nth chords beats-posn))
	 (first-beats-posn (posn-match beats-posn first-beats-posn-posn)))
(list (posn-match chords beats-posn)
       offbeats
	  (posn-match chords first-beats-posn))))

(defun build-ratios-domain (pitch-durs-domain)
   (handler-bind ((error #'(lambda (c)
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the function " "build-ratios-domain" " : " 
                                                        (om-report-condition c))
                                                  :size (om-make-point 300 200))
                               (om-abort)))))

 (let* ((ratios (mapcar #'(lambda (x) (mapcar #'second x)) pitch-durs-domain))
        (sums (mapcar #'(lambda (x) (apply #'+ (om-abs (flat x)))) ratios))
		(max-sum (list-max (flat sums))))
  (loop for voice-ratios in ratios
	    for voice-sum in sums
	    collect (if (< voice-sum max-sum)
			        (x-append voice-ratios (- voice-sum max-sum))
					 voice-ratios)))))		
 
(defun build-midics-domain (voices pitch-durs-domain domains mcs-approx)
 (let ((voice-domains (mapcar #'domain domains)))
  (loop for voice in voices 
		for pitch-dur-dom in pitch-durs-domain
		if (locked-voice? voice)
		collect (mapcar #'first pitch-dur-dom)
		else 
		collect (if (= 2 mcs-approx)
					(mapcar #'round (pop voice-domains))
					(mapcar #'float (pop voice-domains))))))

(defun chords-length-by-measure (voices) ;<== 10/10/2024
 (let* ((v-mes (loop for voice in voices collect (get-measures voice)))
       (mes-ratios  (mat-trans (loop for vm in v-mes
                         collect (loop for mes in vm
                                             collect (tree2ratio (list '? (list (tree mes))))))))
       (mes-all-onsets (loop for measure in mes-ratios 
                             collect (let* ((onsets (loop for vr in measure
                                                          when vr collect (butlast (dx->x 0 (om-abs vr))))))
                                       (when onsets 
                                         (remove-duplicates 
                                        (sort-list 
                                         (flat onsets))))))))                    
(mapcar #'length mes-all-onsets)))
 
(defun correct-pitch-dur-onset-domains (voices-list)
 "FOR PITCH-DUR, PITCH-ONSET AND PITCH-DUR-ONSET WITHOUT RESTS."
 (loop for voice in voices-list
	   collect (loop for el in voice ;do (print (car el))	
					 when (not (or (null (first el))
						      (and (listp (car el))
								    (null (car (car el))))))
					 collect el)))
									   
(defun build-variables-domain (voices domains mcs-approx random?)
   (handler-bind ((error #'(lambda (c)
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the function " "build-variables-domain" " : " 
                                                        (om-report-condition c))
                                                  :size (om-make-point 300 200))
                               (om-abort)))))

 (let* ((pitch-durs-domain (new-domain-pitch-dur voices (list! domains) mcs-approx random?));<== NEW 12/09/2024 ;==> list-of-lists of pitch-variables/durations(ratio)
        (midics-domain (build-midics-domain voices pitch-durs-domain (list! domains) mcs-approx));<== NEW 12/09/2024		 
		(ratios-domain (build-ratios-domain pitch-durs-domain));(mapcar #'(lambda (x) (mapcar #'second x)) pitch-durs-domain));==> list-of-lists of ratios  
        (pitch-variables (mapcar #'(lambda (x) (mapcar #'first x)) pitch-durs-domain)) ;==> list-of-lists of pitches [midics for locked-voices, nil for rests and screamer (a-member-ofv domain) for open-voices]
        (pitch-durs-domain (loop for ratios in ratios-domain  
                                 for pitches in pitch-variables
                                 collect (mat-trans (list pitches ratios))))
        (pitch-variables (mapcar #'(lambda (x) (mapcar #'first x)) pitch-durs-domain))
        (onsets-domain (mapcar #'ratios2onsets ratios-domain)) ;==> list-of-lists of onsets
        (notes-positions (mapcar #'ratios2notes-posn ratios-domain)) ;==> list-of-lists of notes positions
        (all-onsets (sort-list (remove-duplicates (flat onsets-domain)))) ;==> all-onsets from all-voices
        (notes-positions-in-onsets-list  (mapcar #'(lambda (input)
                                                    (posn-in-onsets-list input all-onsets))
										  (mapcar #'list onsets-domain notes-positions (mapcar #'om+ (om-abs ratios-domain) onsets-domain)))) ;;;=> list-of-lists of positions for each pitch in onsets lists, repeating long notes.
        (pitch-variables-all-onsets (mapcar #'posn-match pitch-variables notes-positions-in-onsets-list)) ;==> list of pitches repeating long notes.
        (beats-and-offbeats (get-beats-offbeats voices all-onsets pitch-variables-all-onsets)) ;==> FIRST (ON-BEATS) ==> SECOND (OFF-BEATS) ==> THIRD (FIRST BEATS)
        (chords-on-beats (mapcar #'flat ;==> list of chords on-beats
                            (remove-if #'(lambda (x) (or (not (some #'s::variable? (flat x)))
                                                              (some #'null (remove nil (flat x)))))
                           (first beats-and-offbeats))))
        (chords-off-beats (mapcar #'flat ;==> list of chords off-beats
                            (remove-if #'(lambda (x) (or (not (some #'s::variable? (flat x)))
                                                         (some #'null (remove nil (flat x)))))
                          (second beats-and-offbeats))))
		(chords-first-beats (mapcar #'flat ;==> list of chords first beats
                             (remove-if #'(lambda (x) (or (not (some #'s::variable? (flat x)))
                                                          (some #'null (remove nil (flat x)))))
                              (third beats-and-offbeats))))
        (rest-positions (loop for ratios in ratios-domain
				              collect (loop for ratio in ratios
								  	        for x from 0
											when (minusp ratio)
											collect x))) ;; <== NEW (07/09/2024
        ;; (mapcar #'get-rest-places voices)) ;==> rests positions
        (pitch-positions-without-rests (mapcar #'remove-rest-posn rest-positions notes-positions)) ;==> only notes positions
        (pitch-variables-without-rests (mapcar #'posn-match pitch-variables pitch-positions-without-rests)) ;==> only pitch/var without rests
        (all-chords (mat-trans pitch-variables-all-onsets)) ;<== INCLUDE RESTS
	    (all-chords-without-rests ;==> list-of-lists of chords for all voices (nil for rests and lists for domains in "chords" mode)
          (remove-if #'(lambda (x) (or (not (some #'s::variable? (flat x)))
                                       (some #'null (remove nil (flat x)))))
            all-chords)) ;<== NO RESTS
        (chords-ratios (x->dx (sort-list (remove-duplicates (flat (mapcar #'(lambda (x) (dx->x 0 (om-abs x))) ratios-domain))))))
        (pitch-dur-chords (mapcar #'list all-chords chords-ratios)) ;; <== NEW (04/09/2024) ==================
        (pitch-dur-chords-without-rests (mapcar #'list all-chords-without-rests chords-ratios))	
        (pitch-onset-chords (mapcar #'list (mat-trans pitch-variables-all-onsets) all-onsets))  
	    (pitch-onset-chords-without-rests (remove-if #'(lambda (x) (or (not (some #'s::variable? (flat (first x))))  
                                                          (some #'null (remove nil (flat (first x))))))
						               pitch-onset-chords))            
        (pitch-dur-onset-chords (loop for pitch-durs in pitch-dur-chords   
                                  for pitch-onset in pitch-onset-chords       
                                  collect (list (first pitch-onset) (second pitch-durs) (second pitch-onset)))) ;<===
        (pitch-dur-onset-chords-without-rests (loop for pitch-durs in pitch-dur-chords-without-rests   
                                  for pitch-onset in pitch-onset-chords-without-rests      
                                  collect (list (first pitch-onset) (second pitch-durs) (second pitch-onset)))) ;<===
        (pitch-onset-domain (mapcar #'(lambda (x y) (mapcar #'(lambda (input1 input2) (if (listp input1) (x-append (list input1) input2) (x-append input1 input2))) x y)) pitch-variables onsets-domain))
        (pitch-dur-onset-domain (mapcar #'(lambda (x y) (mapcar #' x-append x y)) pitch-durs-domain onsets-domain))
        (pitch-dur-without-rests (correct-pitch-dur-onset-domains pitch-durs-domain))
        (pitch-onset-without-rests (correct-pitch-dur-onset-domains pitch-onset-domain))
        (pitch-dur-onset-without-rests (correct-pitch-dur-onset-domains pitch-dur-onset-domain))
		)      
 (make-variables-domain
	   pitch-variables-without-rests
	   pitch-variables
	   all-chords-without-rests	   
	   all-chords
	   pitch-dur-without-rests
	   pitch-onset-without-rests
	   pitch-dur-onset-without-rests
	   pitch-variables-all-onsets
	   pitch-durs-domain
	   pitch-onset-domain
	   pitch-dur-onset-domain	    
	   chords-on-beats
	   chords-off-beats
	   chords-first-beats
	   pitch-dur-chords-without-rests
	   pitch-onset-chords-without-rests  
	   pitch-dur-onset-chords-without-rests
	   pitch-dur-chords
	   pitch-onset-chords   
	   pitch-dur-onset-chords	   
	   midics-domain))))

(defmethod build-measures-domain ((voices list) (vars-domain screamer-variables-domain))
   (handler-bind ((error #'(lambda (c)
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " "build-measures-domain" " : " 
                                                        (om-report-condition c))
                                                  :size (om-make-point 300 200))
                               (om-abort)))))

(let* ((measures (mapcar #'get-measures voices))
	     (measures-matrix (if (list-of-listp measures) (mat-trans measures) (mapcar #'list measures)))
	     (notes-length-by-measure (mat-trans
                                                         (mapcar #'(lambda (m-mat)
	                                                 ;(if (list-of-listp measures)
													      (mapcar #'(lambda (x)
														             (if (null x);==> included for different number of measures between voices
														                  0
													                     (length (flat (mapcar #'lmidic (chords x))))))
															 m-mat)
                                                                                                              )
														 ; (length (lmidic (chords m-mat)))))
	                                               measures-matrix)))
             (notes-rests-length-by-measure (mat-trans
                                                         (mapcar #'(lambda (m-mat)
													      (mapcar #'(lambda (x)
														             (if (null x);==> included for different number of measures between voices
														                  0
                                                                                                                                 (length (flat (remove-if #'cont-chord-p (get-all-chords x)))))  )       
															 m-mat)
                                                                                                              )
	                                               measures-matrix)))
	     (pitch-domains (mapcar #'(lambda (notes-domain notes-length) (group-list notes-domain notes-length 'linear))  (pitch vars-domain) notes-length-by-measure))
		 (pitch-include (mapcar #'(lambda (notes-domain notes-length) (group-list notes-domain notes-length 'linear))  (pitch-include vars-domain) notes-rests-length-by-measure))
		 (pitch-dur-domains (mapcar #'(lambda (notes-domain notes-length) (group-list notes-domain notes-length 'linear))  (pitch-dur vars-domain) notes-length-by-measure))
	     (pitch-onset-domains (mapcar #'(lambda (notes-domain notes-length) (group-list notes-domain notes-length 'linear))  (pitch-onset vars-domain) notes-length-by-measure))
	     (pitch-dur-onset-domains (mapcar #'(lambda (notes-domain notes-length) (group-list notes-domain notes-length 'linear))  (pitch-dur-onset vars-domain) notes-length-by-measure))
	     (pitch-dur-include (mapcar #'(lambda (notes-domain notes-length) (group-list notes-domain notes-length 'linear))  (pitch-dur-include vars-domain) notes-rests-length-by-measure))
	     (pitch-onset-include (mapcar #'(lambda (notes-domain notes-length) (group-list notes-domain notes-length 'linear))  (pitch-onset-include vars-domain) notes-rests-length-by-measure))
	     (pitch-dur-onset-include (mapcar #'(lambda (notes-domain notes-length) (group-list notes-domain notes-length 'linear))  (pitch-dur-onset-include vars-domain) notes-rests-length-by-measure))
	     (chords-length-by-measure (chords-length-by-measure voices));<== 10/10/2024
                                        ;(loop for mes in (mat-trans notes-length-by-measure)
                                        ;                          collect (list-max mes)))
                                        ;(mapcar #'(lambda (x) (length (remove-duplicates (flat x))))
	                                                  ;(mat-trans (loop for part in pitch-onset-domains
	                                                  ;                           for x = (loop for measure in part
	                                                  ;                                         collect (mapcar #'second measure))
	                                                  ;                           collect x))))
	   (chords-all-onsets (group-list (mat-trans (pitch-vars-all-onsets vars-domain)) chords-length-by-measure 'linear)) ;<==WITH RESTS
	   ;===============================================================================================================================
	   (chords-domains (loop for measure in chords-all-onsets
	                         collect (loop for chord in measure
                                                 when (not (null (remove nil (flat chord))))
                                                 collect chord))) ;<== 10/10/2024 ==> NO-RESTS
	   ;===============================================================================================================================
  	   (pitch-dur-onset-chords (group-list (chords-pitch-dur-onset vars-domain) (mapcar #'length chords-domains)  'linear)); <== FIX (24/11/2024) || chords-length-by-measure 'linear)) 
	   (pitch-dur-chords (group-list (chords-pitch-dur vars-domain) (mapcar #'length chords-domains) 'linear)) ; <== (24/11/2024) || chords-length-by-measure 'linear)) 
	   (pitch-onset-chords (group-list (chords-pitch-onset vars-domain) (mapcar #'length chords-domains)  'linear)) ; <== (24/11/2024) || chords-length-by-measure 'linear))
	   (pitch-dur-onset-chords-include (group-list (chords-pitch-dur-onset-include vars-domain) chords-length-by-measure 'linear)) 
	   (pitch-dur-chords-include (group-list (chords-pitch-dur-include vars-domain)  chords-length-by-measure 'linear)) 
	   (pitch-onset-chords-include (group-list (chords-pitch-onset-include vars-domain) chords-length-by-measure 'linear))
	   (chords-on-beat (let ((on-beat-chords (chords-on-beat vars-domain))) ; <== 10/10/2024
	                    (loop for mes in chords-domains
							  collect (loop for chord in mes
								            when (member chord on-beat-chords :test #'equal)
											collect chord))))
 	   (chords-off-beat (let ((off-beat-chords (chords-off-beat vars-domain))) ; <== 10/10/2024
 	                    (loop for mes in chords-domains
 							  collect (loop for chord in mes
 								            when (member chord off-beat-chords :test #'equal)
 											collect chord))))											
	   (chords-1st-beat (loop for mes in chords-domains ; <== 10/10/2024
		   	                  collect (list (car mes))))															   						   
	   (midics-domain (midics-domain vars-domain)))

(loop for pitch in (mat-trans pitch-domains)
      for pitch-i in (mat-trans pitch-include) 
      for chords in chords-domains
	  for chords-i in chords-all-onsets
	  for chords-pitch-dur in pitch-dur-chords
	  for chords-pitch-onset in pitch-onset-chords
	  for chords-pitch-dur-onset in pitch-dur-onset-chords
	  for chords-pitch-dur-i in pitch-dur-chords-include
	  for chords-pitch-onset-i in pitch-onset-chords-include
	  for chords-pitch-dur-onset-i in pitch-dur-onset-chords-include
	  for pitch-dur in (mat-trans pitch-dur-domains)
	  for pitch-onset in (mat-trans pitch-onset-domains)
	  for pitch-dur-onset in (mat-trans pitch-dur-onset-domains)
	  for pitch-dur-i in (mat-trans pitch-dur-include)
	  for pitch-onset-i in (mat-trans pitch-onset-include)
	  for pitch-dur-onset-i in (mat-trans pitch-dur-onset-include)
	  for on-beat-chords in chords-on-beat
	  for off-beat-chords in chords-off-beat
	  for 1st-beat-chords in chords-1st-beat
      collect
	 (make-measures-domain pitch
						   pitch-i
						   chords
						   chords-i
						   pitch-dur
						   pitch-onset
						   pitch-dur-onset
						   pitch-dur-i
						   pitch-onset-i
						   pitch-dur-onset-i
						   on-beat-chords
						   off-beat-chords
						   1st-beat-chords						   
						   chords-pitch-dur
						   chords-pitch-onset
						   chords-pitch-dur-onset
						   chords-pitch-dur-i
						   chords-pitch-onset-i
						   chords-pitch-dur-onset-i
						   midics-domain)))))

(defun build-all-domains (poly domains mcs-approx random?)
(let* ((voices (voices poly))
	   (variables-domain (build-variables-domain voices domains mcs-approx random?))
       (measures-domain (build-measures-domain voices variables-domain)))
 (make-screamer-all-domains variables-domain measures-domain)))

; INTERFACE

(defmethod! screamer-score-domain ((domain list)(domain-type string) &optional (n-notes nil))
    :initvals '((60 62 64 65 67 69 71 72) "notes" nil)
    :indoc '( "midi-list"  "domain-type" "number or list")
    :doc "Formats the domain (list of midis) according to the selected type (notes or chords)."
    :menuins '((1 (("notes" "notes") ("chords" "chords"))))
    :icon 486
 (make-score-domain domain domain-type n-notes))
