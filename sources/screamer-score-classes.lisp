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

(defclass screamer-score-constraint ()
 ()
 (:documentation "A superclass for all constraints."))

(defmethod screamer-score-constraint-p ((self screamer-score-constraint)) t)
(defmethod screamer-score-constraint-p ((self t)) nil)

(defclass cs-one-voice (screamer-score-constraint)
 ((constraint :initform nil :initarg nil :reader constraint :writer set-constraint :documentation "lambda function")
  (input :initform nil :initarg nil :reader get-input :writer set-input :documentation "string")
  (voices :initform nil :initarg nil :reader get-voices :writer set-voices :documentation "list of voice numbers")
  (domain :initform nil :initarg nil :reader get-domain :writer set-domain :documentation "string")
  (percentage-mode :initform nil :initarg nil :reader get-perc-mode :writer set-perc-mode :documentation "string")
  (percentage :initform nil :initarg nil :reader get-perc :writer set-perc :documentation "number or list of numbers")
  (cs-mode :initform nil :initarg nil :reader get-cs-mode :writer set-cs-mode :documentation "string"))
 (:documentation "A simple container for constraint-one-voice. A description of each slot is presented below:
:CONSTRAINT = lambda function.
:INPUT = list, n-inputs, car-cdr or growing [string].
:VOICES = list with voice numbers.
:DOMAIN = pitch, pitch-dur, pitch-onset or pitch-dur-onset.
:PERCENTAGE-MODE = off, exactly, less-than, greather-than or between.
:PERCENTAGE = number of list of numbers (used for between).
:CS-MODE = propagation or backtrack (not implemented yet). This should be extended in the future to include heuristics or other search optimizations."))

(defmethod cs-one-voice-p ((self cs-one-voice)) t)
(defmethod cs-one-voice-p ((self t)) nil)
(defmethod screamer-score-constraint-p ((self cs-one-voice)) t)

(defmethod set-cs ((constraint function) (self cs-one-voice))
 (let ((compiled-constraint (fdefinition (compile-screamer-constraint constraint))))
  (set-constraint compiled-constraint self)))

(defmethod make-cs-one-voice ((cs function)(input string)(voices list)(domain string)(percentage-mode string)(percentage t) (cs-mode string))
  (let ((instance (make-instance 'cs-one-voice)))
   (set-cs cs instance)
   (set-input input instance)
   (set-voices voices instance)
   (set-domain domain instance)
   (set-perc-mode percentage-mode instance)
   (set-perc percentage instance)
   (set-cs-mode cs-mode instance)
  instance))

(defclass cs-harmony (screamer-score-constraint)
 ((constraint :initform nil :initarg nil :reader constraint :writer set-constraint :documentation "lambda function")
 (input :initform nil :initarg nil :reader get-input :writer set-input :documentation "string")
 (voice-select :initform nil :initarg nil :reader get-v-sel :writer set-v-sel :documentation "string")
 (voices :initform nil :initarg nil :reader get-voices :writer set-voices :documentation "list of voice numbers")
 (domain :initform nil :initarg nil :reader get-domain :writer set-domain :documentation "string")
 (beats :initform nil :initarg nil :reader get-beats :writer set-beats :documentation "string")
 (percentage-mode :initform nil :initarg nil :reader get-perc-mode :writer set-perc-mode :documentation "string")
 (percentage :initform nil :initarg nil :reader get-perc :writer set-perc :documentation "number or list of numbers")
 (cs-mode :initform nil :initarg nil :reader get-cs-mode :writer set-cs-mode :documentation "string"))
 (:documentation "A simple container for constraint-harmony. A description of each slot is presented below:
 :CONSTRAINT = lambda function.
 :INPUT = list, n-inputs, car-cdr or growing [string].
 :VOICE-SELECT = all-voices or voices-list [string].
 :VOICES = list with voice numbers.
 :DOMAIN = pitch, pitch+dur, pitch+onset or pitch+dur+onset (not implemented yet).
 :BEATS = all, on-beat, off-beat or 1st-beat.
 :PERCENTAGE-MODE = off, exactly, less-than, greather-than or between.
 :PERCENTAGE = number of list of numbers (used for between).
 :CS-MODE = propagation or backtrack [string] (not implemented yet). This should be extended in the future to include heuristics or other search optimizations."))

(defmethod cs-harmony-p ((self cs-harmony)) t)
(defmethod cs-harmony-p ((self t)) nil)
(defmethod screamer-score-constraint-p ((self cs-harmony)) t)

(defmethod set-cs ((constraint function) (self cs-harmony))
 (let ((compiled-constraint (fdefinition (compile-screamer-constraint constraint))))
  (set-constraint compiled-constraint self)))

(defmethod make-cs-harmony ((cs function)(input string)(voice-select string)(voices list)(domain string)(beats string)(percentage-mode string)(percentage t) (cs-mode string))
  (let ((instance (make-instance 'cs-harmony)))
   (set-cs cs instance)
   (set-input input instance)
   (set-v-sel voice-select instance)
   (set-voices voices instance)
   (set-domain domain instance)
   (set-beats beats instance)
   (set-perc-mode percentage-mode instance)
   (set-perc percentage instance)
   (set-cs-mode cs-mode instance)
  instance))

 (defclass cs-profile (screamer-score-constraint)
   ((bpf :initform nil :initarg nil :reader get-bpf :writer set-bpf :documentation "bpf or bpf-lib object")
   (voices :initform nil :initarg nil :reader get-voices :writer set-voices :documentation "list of voice numbers")
   (approx :initform nil :initarg nil :reader get-approx :writer set-approx :documentation "number")
   (range :initform nil :initarg nil :reader get-range :writer set-range :documentation "number or list")
   (cs-mode :initform nil :initarg nil :reader get-cs-mode :writer set-cs-mode :documentation "string"))
   (:documentation "A simple container for constraint-profile. A description of each slot is presented below:
   :BPF = bpf or bpf-lib.
   :VOICES = list with voice numbers.
   :APPROX = number in midi cents.
   :RANGE = number or list of numbers (min max).
   :CS-MODE = propagation or backtrack [string] (not implemented yet). This should be extended in the future to include heuristics or other search optimizations."))

(defmethod cs-profile-p ((self cs-profile)) t)
(defmethod cs-profile-p ((self t)) nil)
(defmethod screamer-score-constraint-p ((self cs-profile)) t)

(defmethod make-cs-profile ((bpf t)(voices list)(approx integer)(range t)(cs-mode string))
  (let ((instance (make-instance 'cs-profile)))
   (set-bpf bpf instance)
   (set-voices voices instance)
   (set-approx approx instance)
   (set-range range instance)
   (set-cs-mode cs-mode instance)
  instance))

(defclass cs-measure (screamer-score-constraint)
 ((constraint :initform nil :initarg nil :reader constraint :writer set-constraint :documentation "constraint object")
  (measure :initform nil :initarg nil :reader measure :writer set-measure :documentation "number or list")
  (cs-mode :initform nil :initarg nil :reader get-cs-mode :writer set-cs-mode :documentation "string"))
 (:documentation "A simple container for constraints. A description of each slot is presented below:
 :CONSTRAINT = constraint-one-voice, constraint-harmony or constraint-profile object.
 :MEASURES = number or list with measure numbers.
 :CS-MODE = propagation or backtrack [string] (not implemented yet).
  This should be extended in the future to include heuristics or other search optimizations."))

(defmethod cs-measure-p ((self cs-measure)) t)
(defmethod cs-measure-p ((self t)) nil)
(defmethod screamer-score-constraint-p ((self cs-measure)) t)

(defmethod make-cs-measure ((cs t)(measure t))
   (let ((instance (make-instance 'cs-measure)))
    (set-constraint cs instance)
    (set-measure measure instance)
   instance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOMAINS

(defclass screamer-score-domain ()
 ()
 (:documentation "A superclass for all domains."))

(defmethod screamer-score-domain-p ((self screamer-score-domain)) t)
(defmethod screamer-score-domain-p ((self t)) nil)

(defclass score-domain (screamer-score-domain) ;should change get-domain-parameters for domain, type,
 ((domain :initform nil :initarg nil :reader domain :writer set-domain :documentation "list of numbers")
  (domain-type :initform nil :initarg nil :reader domain-type :writer set-domain-type :documentation "string")
  (n-notes :initform nil :initarg nil :reader n-notes :writer set-n-notes :documentation "number or list"))
 (:documentation "A simple container for open voice domains (the enumerated domain of screamer variables).
A description of each slot is presented below:
  :DOMAIN = a list in midi cents.
  :TYPE = notes or chords [string].
  :N-NOTES = for chords only. The number of notes of each chord. If this argument is a list, e.g. [3 4 5],
  will generate chords with 3, 4 and 5 notes respectively, applied to the entire list."))

(defmethod make-score-domain ((domain list) (domain-type string) (n-notes t))
 (let ((instance (make-instance 'score-domain)))
  (set-domain domain instance)
  (set-domain-type domain-type instance)
  (set-n-notes n-notes instance)
 instance))

(defmethod get-domain-parameters ((self score-domain))
 (remove nil (list (domain-type self) (domain self) (n-notes self))))

(defmethod score-domain-p ((self score-domain)) t)
(defmethod score-domain-p ((self t )) nil )
(defmethod screamer-score-domain-p ((self score-domain)) t)

(defclass screamer-variables-domain (screamer-score-domain)
   ((pitch :initform nil :initarg nil :reader pitch :writer set-pitch :documentation "list of lists")
    (chords :initform nil :initarg nil :reader chords :writer set-chords :documentation "list of lists")
	(pitch-dur :initform nil :initarg nil :reader pitch-dur :writer set-pitch-dur :documentation "list of lists")
	(pitch-onset :initform nil :initarg nil :reader pitch-onset :writer set-pitch-onset :documentation "list of lists")
	(pitch-dur-onset :initform nil :initarg nil :reader pitch-dur-onset :writer set-pitch-dur-onset :documentation "list of lists")
	(pitch-vars-all-onsets :initform nil :initarg nil :reader pitch-vars-all-onsets :writer set-pitch-vars-all-onsets :documentation "list of lists")
    (chords-on-beat :initform nil :initarg nil :reader chords-on-beat :writer set-chords-on-beat :documentation "list of lists")
    (chords-off-beat :initform nil :initarg nil :reader chords-off-beat :writer set-chords-off-beat :documentation "list of lists")
    (chords-1st-beat :initform nil :initarg nil :reader chords-1st-beat :writer set-chords-1st-beat :documentation "list of lists")
    (chords-pitch-dur :initform nil :initarg nil :reader chords-pitch-dur :writer set-chords-pitch-dur :documentation "list of lists")
    (chords-pitch-dur-onset :initform nil :initarg nil :reader chords-pitch-dur-onset :writer set-chords-pitch-dur-onset :documentation "list of lists")
    (midics-domain :initform nil :initarg nil :reader midics-domain :writer set-midics-domain :documentation "list of lists"))
   (:documentation "A simple container for screamer-variables-domain.
 A description of each slot is presented below:
   :PITCH = list of lists of screamer variables (a-member-ofv or a-random-member-ofv) without rests (nil).
   :CHORDS = list of lists of screamer variables organized as chords, with rests.
   :PITCH-DUR  = list of lists of screamer variables. Each sublist contains two arguments (pitch-variable dur). Include rests.
   :PITCH-ONSETS   = list of lists of screamer variables and ratios. Each sublist contains two arguments (pitch-variable onset). Include rests.
   :PITCH-DUR-ONSETS   = list of lists of screamer variables and ratios. Each sublist contains three arguments (pitch-variable onset). Include rests.
   :PITCH-VARS-ALL-ONSETS = list of lists of screamer variables in all onsets. The variable is repeated in case of long notes.
   :CHORD-ON-BEAT = list of lists of screamer variables organized as chords. Contains only the first chord of each beat.
   :CHORDS-OFF-BEAT = list of lists of screamer variables organized as chords, removing the first chord of each beat.
   :CHORDS-1ST-BEAT = list of lists of screamer variables organized as chords. Conatins only the first chord of each measure.
   :CHORDS-PITCH/DUR ===> NOT IMPLEMENTED YET
   :CHORDS-PITCH/DUR/ONSET ===> NOT IMPLEMENTED YET
   :MIDICS-DOMAIN = list of lists of midicents, one list for each voice."))

(defmethod screamer-variables-domain-p ((self screamer-variables-domain)) t)
(defmethod screamer-variables-domain-p ((self t )) nil )
(defmethod screamer-score-domain-p ((self screamer-variables-domain)) t)

(defmethod make-variables-domain ((pitch list) (chords list) (pitch-durs list) (pitch-onset list) (pitch-dur-onset list) (pitch-vars-all-onsets list)
	   	                          (chords-on-beat list) (chords-off-beat list) (chords-1st-beat list) (pitch-dur-chords list)
								  (pitch-dur-onset-chords list) (midics-domain list))
 (let ((instance (make-instance 'screamer-variables-domain)))
  (set-pitch pitch instance)
  (set-chords chords instance)
  (set-pitch-dur pitch-durs instance)
  (set-pitch-onset pitch-onset instance)
  (set-pitch-dur-onset pitch-dur-onset instance)
  (set-pitch-vars-all-onsets pitch-vars-all-onsets instance)
  (set-chords-on-beat chords-on-beat instance)
  (set-chords-off-beat chords-off-beat instance)
  (set-chords-1st-beat chords-1st-beat instance)
  (set-chords-pitch-dur pitch-dur-chords instance)
  (set-chords-pitch-dur-onset pitch-dur-onset-chords instance)
  (set-midics-domain midics-domain instance)
 instance))

 (defmethod list-all-slots ((domain screamer-variables-domain))
  (list
   (slot-value domain 'pitch)
   (slot-value domain 'chords)
   (slot-value domain 'pitch-dur)
   (slot-value domain 'pitch-dur-onset)
   (slot-value domain 'pitch-vars-all-onsets)
   (slot-value domain 'chords-on-beat)
   (slot-value domain 'chords-off-beat)
   (slot-value domain 'chords-1st-beat)
   ;(slot-value domain 'chords-pitch-dur)
   ;(slot-value domain 'chords-pitch-dur-onset)
   (slot-value domain 'midics-domain)))

;; THESE METHODS IS FOR USING WITH BACKTRACK-CONSTRAINTS, SINCE SCREAMER RETURNS A LIST OF VALUES AND NOT CLASSES.
;; TODO: FIND A WAY TO REBUILD THE MEASURES-DOMAIN OR UNIFY THEM INTO A SINGLE LIST (ALL-DOMAINS = VARIABLES-DOMAIN + MEASURES-DOMAIN)

 (defclass screamer-measures-domain (screamer-score-domain)
   ((pitch :initform nil :initarg nil :reader pitch :writer set-pitch :documentation "list of lists")
    (chords :initform nil :initarg nil :reader chords :writer set-chords :documentation "list of lists")
	(pitch-dur :initform nil :initarg nil :reader pitch-dur :writer set-pitch-dur :documentation "list of lists")
	(pitch-onset :initform nil :initarg nil :reader pitch-onset :writer set-pitch-onset :documentation "list of lists")
	(pitch-dur-onset :initform nil :initarg nil :reader pitch-dur-onset :writer set-pitch-dur-onset :documentation "list of lists")
    (chords-on-beat :initform nil :initarg nil :reader chords-on-beat :writer set-chords-on-beat :documentation "list of lists")
    (chords-off-beat :initform nil :initarg nil :reader chords-off-beat :writer set-chords-off-beat :documentation "list of lists")
    (chords-pitch-dur :initform nil :initarg nil :reader chords-pitch-dur :writer set-chords-pitch-dur :documentation "list of lists")
    (chords-pitch-dur-onset :initform nil :initarg nil :reader chords-pitch-dur-onset :writer set-chords-pitch-dur-onset :documentation "list of lists")
    (midics-domain :initform nil :initarg nil :reader midics-domain :writer set-midics-domain :documentation "list of lists"))
   (:documentation "A simple container for screamer-measures-domain.
 A description of each slot is presented below:
   :PITCH = list of lists of screamer variables (a-member-ofv or a-random-member-ofv) without rests (nil).
   :CHORDS = list of lists of screamer variables organized as chords, with rests.
   :PITCH/DUR  = list of lists of screamer variables. Each sublist contains two arguments (pitch-variable dur). Include rests.
   :PITCH/ONSETS   = list of lists of screamer variables and ratios. Each sublist contains two arguments (pitch-variable onset). Include rests.
   :PITCH/DUR/ONSETS   = list of lists of screamer variables and ratios. Each sublist contains three arguments (pitch-variable onset). Include rests.
   :PITCH-VARS-WITH-RESTS = list of lists of screamer variables with rests (nil). Used to build measures-domain.
   :CHORD-ON-BEAT = list of lists of screamer variables organized as chords. Contains only the first chord of each beat.  ===> NOT IMPLEMENTED YET
   :CHORDS-OFF-BEAT = list of lists of screamer variables organized as chords, removing the first chord of each beat.  ===> NOT IMPLEMENTED YET
   :CHORDS-PITCH/DUR ===> NOT IMPLEMENTED YET
   :CHORDS-PITCH/DUR/ONSET ===> NOT IMPLEMENTED YET
   :MIDICS-DOMAIN = list of lists of midicents, one list for each voice."))

(defmethod screamer-measures-domain-p ((self screamer-measures-domain)) t)
(defmethod screamer-measures-domain-p ((self t )) nil )
(defmethod screamer-score-domain-p ((self screamer-measures-domain)) t)

(defmethod make-measures-domain ((pitch list) (chords list) (pitch-durs list) (pitch-onset list)
								 (pitch-dur-onset list) (chords-on-beat list) (chords-off-beat list)
								 (pitch-dur-chords list) (pitch-dur-onset-chords list) (midics-domain list))
  (let ((instance (make-instance 'screamer-measures-domain)))
   (set-pitch pitch instance)
   (set-chords chords instance)
   (set-pitch-dur pitch-durs instance)
   (set-pitch-onset pitch-onset instance)
   (set-pitch-dur-onset pitch-dur-onset instance)
   (set-chords-on-beat chords-on-beat instance)
   (set-chords-off-beat chords-off-beat instance)
   (set-chords-pitch-dur pitch-dur-chords instance)
   (set-chords-pitch-dur-onset pitch-dur-onset-chords instance)
   (set-midics-domain midics-domain instance)
  instance))

(defmethod list-all-slots ((domain screamer-measures-domain))
 (list
  (slot-value domain 'pitch)
  (slot-value domain 'chords)
  (slot-value domain 'pitch-dur)
  (slot-value domain 'pitch-dur-onset)
  ;(slot-value domain 'chords-on-beat)
  ;(slot-value domain 'chords-off-beat)
  ;(slot-value domain 'chords-pitch-dur)
  ;(slot-value domain 'chords-pitch-dur-onset)
  (slot-value domain 'midics-domain)))

(defclass screamer-all-domains (screamer-score-domain)
  ((variables-domain :initform nil :initarg nil :reader var-domain :writer set-var-domain :documentation "screamer-variables-domain object")
   (measures-domain :initform nil :initarg nil :reader mes-domain :writer set-mes-domain :documentation "screamer-measures-domain object"))
  (:documentation "A simple container for screamer-variable-domains and screamer-measures-domains.
A description of each slot is presented below:
  :VARIABLES-DOMAIN = (see class difinition of screamer-variables-domain).
  :MEASURES-DOMAIN = (see class difinition of screamer-measures-domain)"))

(defmethod screamer-all-domains-p ((self screamer-all-domains)) t)
(defmethod screamer-all-domains-p ((self t )) nil)
(defmethod screamer-score-domain-p ((self screamer-all-domains)) t)

(defmethod make-screamer-all-domains ((vars-domain screamer-variables-domain) (mes-domain list))
 (let ((instance (make-instance 'screamer-all-domains)))
	   (set-var-domain vars-domain instance)
	   (set-mes-domain mes-domain instance)
  instance))

(defmethod get-one-voice-domain ((self cs-one-voice) (domain screamer-variables-domain))
  (funcall (read-from-string (get-domain self)) domain))

(defmethod get-one-voice-domain ((self cs-one-voice) (domain screamer-measures-domain))
 (funcall (read-from-string (get-domain self)) domain))

(defmethod get-one-voice-domain ((self cs-one-voice) (domain screamer-all-domains))
 (get-one-voice-domain self (var-domain domain)))

(defmethod get-chord-beat ((self cs-harmony) (domain screamer-variables-domain))
 (let ((beats (read-from-string (concatenate 'string "chords-" (get-beats self)))))
  (if (equal 'chords-all beats)
      (chords domain)
      (funcall beats domain))))

(defmethod get-chord-beat ((self cs-harmony) (domain screamer-measures-domain))
 (let ((beats (read-from-string (concatenate 'string "chords-" (get-beats self)))))
  (if (equal 'chords-all beats)
      (chords domain)
      (funcall beats domain))))

(defmethod get-chord-beat ((self cs-harmony) (domain screamer-all-domains))
 (get-chord-beat self (var-domain domain)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCREAMER-POLY, SCREAMER-VOICE, SCREAMER-MEASURE AND SCREAMER-CHORD
;;; TODO: SCREAMER BPF, BPF-LIB (DEPENDS-ON OM-SCALE - OM-ROUND, BPFS, X-POINTS, Y-POINTS)

(defclass screamer-music-object ()
 ()
 (:documentation
  "A superclass for all music objects (screamer-poly, screamer-voice, screamer-chord and screamer-measure)."))

(defmethod screamer-music-object-p ((self screamer-music-object)) t)
(defmethod screamer-music-object-p ((self t)) nil)

(defclass screamer-chord (screamer-music-object)
 ((lmidic :initform nil :initarg nil :reader lmidic :writer set-lmidic :documentation "list of list in midicents"))
 (:documentation "A simple container for screamer-chord.
  A description of each slot is presented below:
  :LMIDIC = list of lists of numbers (midi cents)."))

(defmethod screamer-chord-p ((self screamer-chord)) t)
(defmethod screamer-chord-p ((self t)) nil)
(defmethod screamer-music-object-p ((self screamer-chord)) t)

(defmethod make-screamer-chord ((lmidic list))
 (let ((instance (make-instance 'screamer-chord)))
  (set-lmidic lmidic instance)
 instance))

(defmethod OMchord-to-Schord ((OMchord chord))
 (make-screamer-chord (lmidic omchord)))

(defclass screamer-voice (screamer-music-object)
  ((chords :initform nil :initarg nil :reader chords :writer set-chords :documentation "screamer-chord objects")
   (tree :initform nil :initarg nil :reader tree :writer set-tree :documentation "list")
   (ratios :initform nil :initarg nil :reader ratios :writer set-ratios :documentation "list")
   (sv-time-sig :initform nil :initarg nil :reader get-sv-time-sig :writer set-sv-time-sig :documentation "list")
   (tempo :initform nil :initarg nil :reader tempo :writer set-tempo :documentation "list")
   (locked? :initform nil :initarg nil :reader locked? :writer set-lock :documentation "list"))
  (:documentation "A simple container for screamer-voice.
A description of each slot is presented below:
  :CHORDS = list of screamer-chord objects.
  :TREE = list in RTM notation.
  :RATIOS = list of ratios (for compatibility).
  :TIME-SIG = list of two numbers (4 4).
  :TEMPO = number.
  :LOCKED? = t or nil. Locked voices contains predefine pitches."))

(defmethod screamer-voice-p ((self screamer-voice)) t)
(defmethod screamer-voice-p ((self t )) nil)
(defmethod screamer-music-object-p ((self screamer-voice)) t)

(defmethod make-screamer-voice ((chords list) (tree list) (ratios list) (time-sig list) (tempo number) (locked? t))
  (let ((instance (make-instance 'screamer-voice)))
   (set-chords chords instance)
   (set-tree tree instance)
   (set-ratios ratios instance)
   (set-sv-time-sig time-sig instance)
   (set-tempo tempo instance)
   (set-lock locked? instance)
  instance))

(defmethod get-time-sig ((self screamer-voice) &optional mode)
 (declare (ignore mode))
 (get-sv-time-sig self))

(defmethod locked-voice? ((self voice))
 (let ((voice-chords (remove-duplicates (flat (mapcar #'lmidic (chords self))))))
  (not (and (= 6000 (first voice-chords))
            (= 1 (length voice-chords))))))

(defmethod locked-voice? ((self screamer-voice))
 (locked? self))

(defmethod OMvoice-to-Svoice ((self voice))
 (make-screamer-voice (mapcar #'OMchord-to-Schord (chords self))
  	                  (tree self)
					  (tree2ratio (tree self))
					  (get-time-sig self)
					  (tempo self)
					  (locked-voice? self)))

(defclass screamer-poly (screamer-music-object)
((voices :initform nil :initarg nil :reader voices :writer set-voices :documentation "list of screamer-voice objects"))
 (:documentation "A simple container for screamer-voices."))

(defmethod screamer-poly-p ((self screamer-poly)) t)
(defmethod screamer-poly-p ((self t)) nil)
(defmethod screamer-music-object-p ((self screamer-poly)) t)

(defmethod make-screamer-poly ((voices list))
(let ((instance (make-instance 'screamer-poly)))
 (set-voices voices instance)
 instance))

(defmethod OMpoly-to-Spoly ((OMpoly poly))
 (make-screamer-poly (mapcar #'OMvoice-to-Svoice (voices OMpoly))))

(defclass screamer-measure (screamer-music-object)
 ((chords :initform nil :initarg nil :reader chords :writer set-chords :documentation "screamer-chord objects")
  (tree :initform nil :initarg nil :reader tree :writer set-tree :documentation "list in RTM notation"))
  (:documentation "A simple container for screamer-chords and trees.
	A description of each slot is presented below:
	  :CHORDS = list of screamer-chord objects.
      :TREE = list in RTM notation (time signature and divisions)."))

(defmethod screamer-measure-p ((self screamer-measure)) t)
(defmethod screamer-measure-p ((self t)) nil)
(defmethod screamer-music-object-p ((self screamer-measure)) t)

(defmethod make-screamer-measure ((chords list) (tree list))
  (let ((instance (make-instance 'screamer-measure)))
   (set-chords chords instance)
   (set-tree tree instance)
   instance))

(defun get-number-of-chords-per-measure (tree)
 (loop for mes in (cadr tree)
       collect (let ((ratios (correct-measurefloats (list 1 (list mes)))))
                (length (remove-if #'(lambda (x) (or (floatp x) (minusp x))) ratios)))))

(defmethod get-measures ((self screamer-voice))
 (let* ((tree (tree self))
        (n-chords-by-measure (get-number-of-chords-per-measure tree))
		(group-chords (group-list (chords self) n-chords-by-measure 'linear)))
  (loop for mes-tree in (cadr tree)
        for chord-group in group-chords
        collect (make-screamer-measure chord-group mes-tree))))
