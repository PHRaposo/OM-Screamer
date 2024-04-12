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

(defun pitch-domains (voices domains)
(let* ((locked-voices-pitch (mapcar #'(lambda (x)
                                    (if (locked-voice? x)
                                        (chords x) t)) voices))
         (open-voices-posn (positions locked-voices-pitch t)))
(subs-posn locked-voices-pitch open-voices-posn domains)))

(defun pitch-dur-locked (ratio chord)
 (if (< ratio 0)
     (x-append (list nil) ratio)

 (let ((notes (lmidic chord)))
   (if (= (length notes) 1)
       (x-append (s::variablize (car notes)) ratio)
       (list (mapcar #'s::variablize notes) ratio)))))

(defun pitch-dur-vars (ratio domain mcs-approx random?)
 (if (< ratio 0)
 (x-append (list nil) ratio)
 (cond
  ((equal (first domain) "notes")
    (if random?
       (x-append (om?::a-random-mc-member-ofv mcs-approx (second domain)) ratio)
       (x-append (om?::a-mc-member-ofv mcs-approx (second domain)) ratio)))
  ((equal (first domain) "chords")
    (x-append (om?::list-of-mc-chords-inv (list (third domain)) mcs-approx (second domain) random?) ratio))
 (t (progn (om-message-dialog "The number of open voices does not corresponds to the number of domains.") (om-abort))))
))

(defun pitch-dur-chords (ratios domain chord-l mcs-approx random? accumul)
(if chord-l

 (let ((one-result
        (if (< (first ratios) 0)
            (x-append (list nil) (first ratios))
            (x-append (om?::list-of-mc-chords-inv (list (first chord-l)) mcs-approx domain random?) (first ratios)))))

   (if (null (first one-result))

       (pitch-dur-chords (cdr ratios) domain chord-l mcs-approx random? (x-append (list one-result) accumul))

       (pitch-dur-chords (cdr ratios) domain (cdr chord-l) mcs-approx random? (x-append (list one-result) accumul))))

 (reverse accumul)))

(defun domain-pitch-dur (voices pitch-domains mcs-approx random?)
(mapcar #'(lambda (voice pitch-dom)
 (if (locked-voice? voice)
     (mapcar #'pitch-dur-locked (tree2ratio (tree voice)) pitch-dom)
(if (equal (first pitch-dom) "chords")
     (if (listp (third pitch-dom)) ;==> corrected (third (first pitch-domais))) to (third pitch-dom))-21.01.2024
         (let* ((ratios (tree2ratio (tree voice)))
                  (n-chords (length (remove-if #'(lambda (x) (< x 0)) ratios)))
                  (chords-correct-length (flat (group-list (third pitch-dom) (list n-chords) 'circular)))) ;==> corrected (third (first pitch-domais))
          (pitch-dur-chords ratios (second (first pitch-domains)) chords-correct-length mcs-approx random? nil))
     (mapcar #'(lambda (input)
      (pitch-dur-vars input pitch-dom mcs-approx random?)) (tree2ratio (tree voice))))

     (mapcar #'(lambda (input)
      (pitch-dur-vars input pitch-dom mcs-approx random?)) (tree2ratio (tree voice))))
    ))
 voices pitch-domains))

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
(if all-onsets
   (cond
   ((= 1 (length (second voice-onsets-posn)) )
    (posn-in-onsets-list-internal voice-onsets-posn
                                                 (cdr all-onsets)
                                                 (x-append (first (second voice-onsets-posn)) accumul)))

   ((< (first all-onsets) (second (first voice-onsets-posn)) )
    (posn-in-onsets-list-internal voice-onsets-posn
                                                 (cdr all-onsets)
                                                 (x-append (first (second voice-onsets-posn)) accumul)))

  ((= (first all-onsets) (second (first voice-onsets-posn)) )
     (posn-in-onsets-list-internal (x-append (list (cdr (first voice-onsets-posn)))
                                                                  (list (cdr (second voice-onsets-posn))))
                                                 (cdr all-onsets)
                                                 (x-append (second (second voice-onsets-posn)) accumul))))

(reverse accumul)))

(defun remove-rest-posn (rest-places positions)
 (remove-if #'(lambda (x) (member x rest-places)) positions))

(defun get-beats-from-time-sig (time-sig)
   (repeat-n (/ 1 (second time-sig))
                  (first time-sig)))

(defun get-beats-offbeats (voices all-onsets pitch-variables-all-onsets)
 (let* ((time-sig (get-time-sig (first voices)))
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

(defun build-variables-domain (voices domains mcs-approx random?)
 (let* ((pitch-domains (pitch-domains voices (if (screamer-score-domain-p domains)
		                                                          (list (get-domain-parameters domains))
												   (mapcar #'(lambda (score-domain)
												     (get-domain-parameters score-domain)) domains))));==> chord-objects for locked-voices and list of midics for variables
         (midics-domain (mapcar #'(lambda (x) (if (chord-p (first x)) (flat (mapcar #'lmidic x)) (second x))) pitch-domains))
         (pitch-durs-domain (domain-pitch-dur voices pitch-domains mcs-approx random?))  ;==> list-of-list with pitch/dur (include rests - nil)
         (ratios-domain (mapcar #'(lambda (x) (mapcar #'second x)) pitch-durs-domain))  ;==> list-of-lists of ratios
         (pitch-variables (mapcar #'(lambda (x) (mapcar #'first x)) pitch-durs-domain))  ;==> list-of-lists of pitches [midics for locked-voices, nil for rests and screamer (a-random-member-ofv domain) for open-voices]
         (onsets-domain (mapcar #'ratios2onsets ratios-domain)) ;==> list-of-lists of onsets
         (notes-positions (mapcar #'ratios2notes-posn ratios-domain)) ;==> list-of-lists of notes positions
         (all-onsets (sort-list (remove-duplicates (flat onsets-domain)))) ;==> all-onsets from all-voices
         (notes-positions-in-onsets-list  (mapcar #'(lambda (input)
                                                          (posn-in-onsets-list input all-onsets)) (mapcar #'list onsets-domain notes-positions))) ;;;=> list-of-lists of positions for each pitch in onsets lists, repeating long notes.
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
        (rest-positions (mapcar #'get-rest-places voices)) ;==> rests positions
        (pitch-positions-without-rests (mapcar #'remove-rest-posn rest-positions notes-positions)) ;==> only notes positions
        (pitch-variables-without-rests (mapcar #'posn-match pitch-variables pitch-positions-without-rests)) ;==> only pitch/var without rests
		(all-chords ;==> list-of-lists of chords for all voices (nil for rests and lists for domains in "chords" mode)
                            (remove-if #'(lambda (x) (or (not (some #'s::variable? (flat x)))
                                                          (some #'null (remove nil (flat x)))))
                           (mat-trans pitch-variables-all-onsets)))
		;(chords-ratios (x->dx (sort-list (remove-duplicates (flat (mapcar #'(lambda (x) (dx->x 0 (om-abs x))) ratios-domain)))))) ===> NOT IMPLEMENTED YET
		;(pitch-dur-chords (mapcar #'list all-chords chords-ratios)) ;==> lists-of-lists of chords (pitches) and durations (ratios) ===> NOT IMPLEMENTED YET
		;(pitch-dur-onset-chords (mapcar #'list all-chords chords-ratios all-onsets)) ;==> lists-of-lists of chords (pitches), durations (ratios) and onsets (ratios) ===> NOT IMPLEMENTED YET
        (pitch-onset-domain (mapcar #'(lambda (x y) (mapcar #'(lambda (input1 input2) (if (listp input1) (x-append (list input1) input2) (x-append input1 input2))) x y)) pitch-variables onsets-domain))
        (pitch-dur-onset-domain (mapcar #'(lambda (x y) (mapcar #' x-append x y)) pitch-durs-domain onsets-domain)))

 (make-variables-domain
	   pitch-variables-without-rests
	   all-chords
	   pitch-durs-domain
	   pitch-onset-domain
	   pitch-dur-onset-domain
	   pitch-variables-all-onsets
	   chords-on-beats
	   chords-off-beats
	   chords-first-beats
	   nil ;==> PITCH-DUR-CHORDS
	   nil ;==> PITCH-DUR-ONSET-CHORDS
	   midics-domain)))

(defmethod build-measures-domain ((voices list) (vars-domain screamer-variables-domain))
(let* ((measures (mapcar #'get-measures voices))
	     (measures-matrix (if (list-of-listp measures) (mat-trans measures) measures))
	     (notes-length-by-measure (mat-trans (mapcar #'(lambda (m-mat)
	                                                 (if (list-of-listp measures) (mapcar #'(lambda (x) (length (flat (mapcar #'lmidic (chords x))))) m-mat)
	                                                     (length (lmidic (chords m-mat)))))
	                                               measures-matrix)))
	     (pitch-domains (mapcar #'(lambda (notes-domain notes-length) (group-list notes-domain notes-length 'linear))  (pitch vars-domain) notes-length-by-measure))
	     (pitch-dur-domains (mapcar #'(lambda (notes-domain notes-length) (group-list notes-domain notes-length 'linear))  (pitch-dur vars-domain) notes-length-by-measure))
	     (pitch-onset-domains (mapcar #'(lambda (notes-domain notes-length) (group-list notes-domain notes-length 'linear))  (pitch-onset vars-domain) notes-length-by-measure))
	     (pitch-dur-onset-domains (mapcar #'(lambda (notes-domain notes-length) (group-list notes-domain notes-length 'linear))  (pitch-dur-onset vars-domain) notes-length-by-measure))
	     (chords-length-by-measure (mapcar #'(lambda (x) (length (remove-duplicates (flat x))))
	                                                  (mat-trans (loop for part in pitch-onset-domains
	                                                                             for x = (loop for measure in part
	                                                                                           collect (mapcar #'second measure))
	                                                                             collect x))))
	   (chords-all-onsets (group-list (mat-trans (pitch-vars-all-onsets vars-domain)) chords-length-by-measure 'linear))
	   (chords-domains (loop for measure in chords-all-onsets
	                         for chords = (remove-if #'(lambda (x)(or (not (some #'s::variable? (flat x)))
	                                                                  (some #'null (remove nil (flat x)))))
	                                        measure)
	                         collect chords))
	;==> CHORD-ON-BEAT ;==> CHORD-OFF-BEAT	;==> PITCH-DUR-CHORDS ;==> PITCH-DUR-ONSET-CHORDS
	   (midics-domain (midics-domain vars-domain)))
(loop for pitch in (mat-trans pitch-domains)
      for chords in chords-domains
	  for pitch-dur in (mat-trans pitch-dur-domains)
	  for pitch-onset in (mat-trans pitch-onset-domains)
	  for pitch-dur-onset in (mat-trans pitch-dur-onset-domains)
      collect
	 (make-measures-domain pitch
						   chords
						   pitch-dur
						   pitch-onset
						   pitch-dur-onset
						   nil ;chords-on-beat
						   nil ;chords-off-beat
						   nil ;chords-pitch-dur
						   nil ;chords-pitch-dur-onset
						   midics-domain))))

(defun build-all-domains (poly domains mcs-approx random?)
(let* ((voices (voices poly))
	   (variables-domain (build-variables-domain voices domains mcs-approx random?))
       (measures-domain (build-measures-domain voices variables-domain)))
 (make-screamer-all-domains variables-domain measures-domain)))

; INTERFACE

(defmethod! screamer-score-domain ((domain list)(domain-type string) &optional (n-notes nil))
    :initvals '((6000 6200 6400 6500 6700 6900 7100 7200) "notes" nil)
    :indoc '( "midics-list"  "domain-type" "number or list")
    :doc "Formats the domain (list of midics) according to the selected type (notes or chords)."
    :menuins '((1 (("notes" "notes") ("chords" "chords"))))
    :icon 487
 (make-score-domain domain domain-type n-notes))
