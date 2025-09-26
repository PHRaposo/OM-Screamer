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
 ((constraint :initform nil :initarg :constraint :reader constraint :writer set-constraint :documentation "lambda function")
  (input :initform nil :initarg :input :reader get-input :writer set-input :documentation "string")
  (voices :initform nil :initarg :voices :reader get-voices :writer set-voices :documentation "list of voice numbers")
  (domain :initform nil :initarg :domain :reader get-domain :writer set-domain :documentation "string")
  (rests :initform nil :initarg :rests :reader get-rests :writer set-rests :documentation "string")
  (percentage-mode :initform nil :initarg :percentage-mode :reader get-perc-mode :writer set-perc-mode :documentation "string")
  (percentage :initform nil :initarg :percentage :reader get-perc :writer set-perc :documentation "number or list of numbers")
  (cs-mode :initform nil :initarg :cs-mode :reader get-cs-mode :writer set-cs-mode :documentation "string"))
 (:documentation "A simple container for constraint-one-voice. A description of each slot is presented below:
:CONSTRAINT = lambda function.
:INPUT = list, n-inputs, car-cdr or growing [string].
:VOICES = list with voice numbers.
:DOMAIN = pitch, pitch-dur, pitch-onset or pitch-dur-onset.
:RESTS = include or exclude.
:PERCENTAGE-MODE = off, exactly, less-than, greather-than or between.
:PERCENTAGE = number of list of numbers (used for between).
:CS-MODE = propagation or backtrack (not implemented yet). This should be extended in the future to include heuristics or other search optimizations."))

(defmethod cs-one-voice-p ((self cs-one-voice)) t)
(defmethod cs-one-voice-p ((self t)) nil)
(defmethod screamer-score-constraint-p ((self cs-one-voice)) t)

(defmethod set-cs ((constraint function) (self cs-one-voice) &optional cs-mode)
 (let ((compiled-constraint (fdefinition (if (string-equal "backtrack" cs-mode)
 	                                           (compile-screamer-backtrack-constraint constraint)										 
                                             (compile-screamer-constraint constraint)))))
  (set-constraint compiled-constraint self)))

(defmethod make-cs-one-voice ((cs function)(input string)(voices list)(domain string) (rests string)(percentage-mode string)(percentage t) (cs-mode string))
 (let ((instance (make-instance 'cs-one-voice 
					:input input
					:voices voices
					:domain domain
					:rests rests
					:percentage-mode percentage-mode
					:percentage percentage
					:cs-mode cs-mode)))
 (set-cs cs instance cs-mode)
 instance))

(defclass cs-harmony (screamer-score-constraint)
 ((constraint :initform nil :initarg :constraint :reader constraint :writer set-constraint :documentation "lambda function")
 (input :initform nil :initarg :input :reader get-input :writer set-input :documentation "string")
 (voice-select :initform nil :initarg :voice-select :reader get-v-sel :writer set-v-sel :documentation "string")
 (voices :initform nil :initarg :voices :reader get-voices :writer set-voices :documentation "list of voice numbers")
 (domain :initform nil :initarg :domain :reader get-domain :writer set-domain :documentation "string")
 (rests :initform nil :initarg :rests :reader get-rests :writer set-rests :documentation "string")
 (beats :initform nil :initarg :beats :reader get-beats :writer set-beats :documentation "string")
 (percentage-mode :initform nil :initarg :percentage-mode :reader get-perc-mode :writer set-perc-mode :documentation "string")
 (percentage :initform nil :initarg :percentage :reader get-perc :writer set-perc :documentation "number or list of numbers")
 (cs-mode :initform nil :initarg :cs-mode :reader get-cs-mode :writer set-cs-mode :documentation "string"))
 (:documentation "A simple container for constraint-harmony. A description of each slot is presented below:
 :CONSTRAINT = lambda function.
 :INPUT = list, n-inputs, car-cdr or growing [string].
 :VOICE-SELECT = all-voices or voices-list [string].
 :VOICES = list with voice numbers.
 :DOMAIN = pitch, pitch+dur, pitch+onset or pitch+dur+onset.
 :RESTS = include or exclude.
 :BEATS = all, on-beat, off-beat or 1st-beat.
 :PERCENTAGE-MODE = off, exactly, less-than, greather-than or between.
 :PERCENTAGE = number of list of numbers (used for between).
 :CS-MODE = propagation or backtrack [string] (not implemented yet). This should be extended in the future to include heuristics or other search optimizations."))

(defmethod cs-harmony-p ((self cs-harmony)) t)
(defmethod cs-harmony-p ((self t)) nil)
(defmethod screamer-score-constraint-p ((self cs-harmony)) t)

(defmethod set-cs ((constraint function) (self cs-harmony) &optional cs-mode)
 (let ((compiled-constraint (fdefinition (if (string-equal "backtrack" cs-mode)
 	                                         (compile-screamer-backtrack-constraint constraint)
                       (compile-screamer-constraint constraint)))))
  (set-constraint compiled-constraint self)))

(defmethod make-cs-harmony ((cs function)(input string)(voice-select string)(voices list)(domain string)(rests string)(beats string)(percentage-mode string)(percentage t) (cs-mode string))
  (let ((instance (make-instance 'cs-harmony
	  	            :input input 
					:voice-select voice-select
					:voices voices
					:domain domain
					:rests rests
					:beats beats
					:percentage-mode percentage-mode
					:percentage percentage
					:cs-mode cs-mode)))				
   (set-cs cs instance cs-mode)
  instance))

 (defclass cs-profile (screamer-score-constraint)
   ((bpf :initform nil :initarg :bpf :reader get-bpf :writer set-bpf :documentation "bpf or bpf-lib object")
   (voices :initform nil :initarg :voices :reader get-voices :writer set-voices :documentation "list of voice numbers")
   (approx :initform nil :initarg :approx :reader get-approx :writer set-approx :documentation "number")
   (range :initform nil :initarg :range :reader get-range :writer set-range :documentation "number or list")
   (scale-time? :initform nil :initarg :scale-time? :reader scale-time? :writer set-scale-time :documentation "symbol - t or nil")  
   (cs-mode :initform nil :initarg :cs-mode :reader get-cs-mode :writer set-cs-mode :documentation "string"))
   (:documentation "A simple container for constraint-profile. A description of each slot is presented below:
   :BPF = bpf or bpf-lib.
   :VOICES = list with voice numbers.
   :APPROX = number in midi cents.
   :RANGE = number or list of numbers (min max).
   :SCALE-TIME? = t or nil. If nil, the durations are ignored.	   
   :CS-MODE = propagation or backtrack [string] (not implemented yet). This should be extended in the future to include heuristics or other search optimizations."))

(defmethod cs-profile-p ((self cs-profile)) t)
(defmethod cs-profile-p ((self t)) nil)
(defmethod screamer-score-constraint-p ((self cs-profile)) t)

(defmethod make-cs-profile ((bpf t)(voices list)(approx integer)(range t) (scale-time? t)(cs-mode string))
 (make-instance 'cs-profile
   :bpf bpf
   :voices voices
   :approx approx
   :range range
   :scale-time? scale-time?
   :cs-mode cs-mode))

(defclass cs-measure (screamer-score-constraint)
 ((constraint :initform nil :initarg :constraint :reader constraint :writer set-constraint :documentation "constraint object")
  (measure :initform nil :initarg :measure :reader measure :writer set-measure :documentation "number or list")
  (cs-mode :initform nil :initarg :cs-mode :reader get-cs-mode :writer set-cs-mode :documentation "string"))
 (:documentation "A simple container for constraints. A description of each slot is presented below:
 :CONSTRAINT = constraint-one-voice, constraint-harmony or constraint-profile object.
 :MEASURES = number or list with measure numbers.
 :CS-MODE = propagation or backtrack [string] (not implemented yet).
  This should be extended in the future to include heuristics or other search optimizations."))

(defmethod cs-measure-p ((self cs-measure)) t)
(defmethod cs-measure-p ((self t)) nil)
(defmethod screamer-score-constraint-p ((self cs-measure)) t)

(defmethod make-cs-measure ((cs t)(measure t))
 (make-instance 'cs-measure
    :constraint cs
    :measure measure))

(defclass cs-measures (screamer-score-constraint)
 ((constraint :initform nil :initarg :constraint :reader constraint :writer set-constraint :documentation "lambda-patch")
  (measures :initform nil :initarg :measures :reader measures :writer set-measures :documentation "list")
  (cs-type :initform nil :initarg :cs-type :reader cs-type :writer set-cs-type :documentation "one-voice or harmony")
  (voices :initform nil :initarg :voices :reader get-voices :writer set-voices :documentation "list of voice numbers")
  (domain :initform nil :initarg :domain :reader get-domain :writer set-domain :documentation "string")
  (rests :initform nil :initarg :rests :reader get-rests :writer set-rests :documentation "string")
   )
 (:documentation "A simple container for constraints. A description of each slot is presented below:
 :CONSTRAINT = constraint-one-voice, constraint-harmony or constraint-profile object.
 :MEASURES = list with measure numbers.
 :CS-TYPE = one-voice or harmony.
 :VOICES = list with voice numbers.
 :DOMAIN = pitch, pitch+dur, pitch+onset or pitch+dur+onset.
 :RESTS = include or exclude.
  This should be extended in the future to include heuristics or other search optimizations."))

(defmethod cs-measures-p ((self cs-measures)) t)
(defmethod cs-measures-p ((self t)) nil)
(defmethod screamer-score-constraint-p ((self cs-measures)) t)

(defmethod make-cs-measures ((cs t)(measures list) (cs-type string) (voices list) (domain string) (rests string))
 (make-instance 'cs-measures
    :constraint cs
    :measures measures
	:cs-type cs-type
	:voices voices
	:domain domain
	:rests rests))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOMAINS

(defclass screamer-score-domain ()
 ()
 (:documentation "A superclass for all domains."))

(defmethod screamer-score-domain-p ((self screamer-score-domain)) t)
(defmethod screamer-score-domain-p ((self t)) nil)

(defclass score-domain (screamer-score-domain)
 ((domain :initform nil :initarg :domain :reader domain :writer set-domain :documentation "list of numbers")
  (domain-type :initform nil :initarg :domain-type :reader domain-type :writer set-domain-type :documentation "string")
  (n-notes :initform nil :initarg :n-notes :reader n-notes :writer set-n-notes :documentation "number or list"))
 (:documentation "A simple container for open voice domains (the enumerated domain of screamer variables).
A description of each slot is presented below:
  :DOMAIN = a list in midi cents.
  :TYPE = notes or chords [string].
  :N-NOTES = for chords only. The number of notes of each chord. If this argument is a list, e.g. [3 4 5],
  will generate chords with 3, 4 and 5 notes respectively, applied to the entire list."))

(defmethod make-score-domain ((domain list) (domain-type string) (n-notes t))
 (make-instance 'score-domain
  :domain domain
  :domain-type domain-type
  :n-notes n-notes))

(defmethod get-domain-parameters ((self score-domain))
 (remove nil (list (domain-type self) (domain self) (n-notes self))))

(defmethod score-domain-p ((self score-domain)) t)
(defmethod score-domain-p ((self t )) nil )
(defmethod screamer-score-domain-p ((self score-domain)) t)

(defclass screamer-variables-domain (screamer-score-domain)
   ((pitch :initform nil :initarg :pitch :reader pitch :writer set-pitch :documentation "list of lists")
    (pitch-include :initform nil :initarg :pitch-include :reader pitch-include :writer set-pitch-include :documentation "list of lists")
    (chords :initform nil :initarg :chords :reader chords :writer set-chords :documentation "list of lists")
    (chords-include :initform nil :initarg :chords-include :reader chords-include :writer set-chords-include :documentation "list of lists")	
	(pitch-dur :initform nil :initarg :pitch-dur :reader pitch-dur :writer set-pitch-dur :documentation "list of lists")
	(pitch-onset :initform nil :initarg :pitch-onset :reader pitch-onset :writer set-pitch-onset :documentation "list of lists")
	(pitch-dur-onset :initform nil :initarg :pitch-dur-onset :reader pitch-dur-onset :writer set-pitch-dur-onset :documentation "list of lists")
	(pitch-vars-all-onsets :initform nil :initarg :pitch-vars-all-onsets :reader pitch-vars-all-onsets :writer set-pitch-vars-all-onsets :documentation "list of lists")
	(pitch-dur-include :initform nil :initarg :pitch-dur-include :reader pitch-dur-include :writer set-pitch-dur-include :documentation "list of lists")
	(pitch-onset-include :initform nil :initarg :pitch-onset-include :reader pitch-onset-include :writer set-pitch-onset-include :documentation "list of lists")
	(pitch-dur-onset-include :initform nil :initarg :pitch-dur-onset-include :reader pitch-dur-onset-include :writer set-pitch-dur-onset-include :documentation "list of lists")
    (chords-on-beat :initform nil :initarg :chords-on-beat :reader chords-on-beat :writer set-chords-on-beat :documentation "list of lists")
    (chords-off-beat :initform nil :initarg :chords-off-beat :reader chords-off-beat :writer set-chords-off-beat :documentation "list of lists")
    (chords-1st-beat :initform nil :initarg :chords-1st-beat :reader chords-1st-beat :writer set-chords-1st-beat :documentation "list of lists")
    (chords-pitch-dur :initform nil :initarg :chords-pitch-dur :reader chords-pitch-dur :writer set-chords-pitch-dur :documentation "list of lists")
    (chords-pitch-onset :initform nil :initarg :chords-pitch-onset :reader chords-pitch-onset :writer set-chords-pitch-onset :documentation "list of lists")	
    (chords-pitch-dur-onset :initform nil :initarg :chords-pitch-dur-onset :reader chords-pitch-dur-onset :writer set-chords-pitch-dur-onset :documentation "list of lists")
	(chords-pitch-dur-include :initform nil :initarg :chords-pitch-dur-include :reader chords-pitch-dur-include :writer set-chords-pitch-dur-include :documentation "list of lists")
    (chords-pitch-onset-include :initform nil :initarg :chords-pitch-onset-include :reader chords-pitch-onset-include :writer set-chords-pitch-onset-include :documentation "list of lists")	
    (chords-pitch-dur-onset-include :initform nil :initarg :chords-pitch-dur-onset-include :reader chords-pitch-dur-onset-include :writer set-chords-pitch-dur-onset-include :documentation "list of lists")
    (midics-domain :initform nil :initarg :midics-domain :reader midics-domain :writer set-midics-domain :documentation "list of lists"))  ;<== CHANGED TO MIDI!
   (:documentation "A simple container for screamer-variables-domain.
 A description of each slot is presented below:
   :PITCH = list of lists of screamer variables (a-member-ofv or a-random-member-ofv) without rests (nil).
   :PITCH-INCLUDE = list of lists of screamer variables (a-member-ofv or a-random-member-ofv) with rests.
   :CHORDS = list of lists of screamer variables organized as chords, with rests inside chords.
   :CHORDS-INCLUDE = list of lists of screamer variables organized as chords, with chords with all rests.  
   :PITCH-DUR  = list of lists of screamer variables. Each sublist contains two arguments (pitch-variable dur). Does not include rests.
   :PITCH-ONSETS   = list of lists of screamer variables and ratios. Each sublist contains two arguments (pitch-variable onset). Does not include rests.
   :PITCH-DUR-ONSETS   = list of lists of screamer variables and ratios. Each sublist contains three arguments (pitch-variable onset). Does not include rests.
   :PITCH-VARS-ALL-ONSETS = list of lists of screamer variables in all onsets. The variable is repeated in case of long notes. Include rests.
   :PITCH-DUR-INCLUDE  = list of lists of screamer variables. Each sublist contains two arguments (pitch-variable dur). Include rests.
   :PITCH-ONSETS-INCLUDE     = list of lists of screamer variables and ratios. Each sublist contains two arguments (pitch-variable onset). Include rests.
   :PITCH-DUR-ONSETS-INCLUDE     = list of lists of screamer variables and ratios. Each sublist contains three arguments (pitch-variable onset). Include rests.    
   :CHORD-ON-BEAT = list of lists of screamer variables organized as chords. Contains only the first chord of each beat.
   :CHORDS-OFF-BEAT = list of lists of screamer variables organized as chords, removing the first chord of each beat.
   :CHORDS-1ST-BEAT = list of lists of screamer variables organized as chords. Contains only the first beat of each measure.
   :CHORDS-PITCH-DUR = TODO doc.
   :CHORDS-PITCH-ONSET = TODO doc.  
   :CHORDS-PITCH-DUR-ONSET = TODO doc.
   :CHORDS-PITCH-DUR-INCLUDE = TODO doc.
   :CHORDS-PITCH-ONSET-INCLUDE = TODO doc.  
   :CHORDS-PITCH-DUR-ONSET-INCLUDE = TODO doc.  
   :MIDICS-DOMAIN = list of lists of midicents, one list for each voice.")) ;<== CHANGED TO MIDI!

(defmethod screamer-variables-domain-p ((self screamer-variables-domain)) t)
(defmethod screamer-variables-domain-p ((self t )) nil )
(defmethod screamer-score-domain-p ((self screamer-variables-domain)) t)

(defmethod make-variables-domain ((pitch list) (pitch-include list) (chords list) (chords-include list) (pitch-durs list) (pitch-onset list) (pitch-dur-onset list) (pitch-vars-all-onsets list)
                                   (pitch-durs-include list) (pitch-onset-include  list) (pitch-dur-onset-include  list)(chords-on-beat list) (chords-off-beat list)
								   (chords-1st-beat list) (pitch-dur-chords list) (pitch-onset-chords list)(pitch-dur-onset-chords list) (pitch-dur-chords-include list)
								   (pitch-onset-chords-include list)(pitch-dur-onset-chords-include list)
								   (midics-domain list))
 (make-instance 'screamer-variables-domain
  :pitch pitch
  :pitch-include pitch-include
  :chords chords
  :chords-include chords-include 
  :pitch-dur pitch-durs
  :pitch-onset pitch-onset
  :pitch-dur-onset pitch-dur-onset
  :pitch-vars-all-onsets pitch-vars-all-onsets
  :pitch-dur-include pitch-durs-include
  :pitch-onset-include pitch-onset-include
  :pitch-dur-onset-include pitch-dur-onset-include
  :chords-on-beat chords-on-beat
  :chords-off-beat chords-off-beat
  :chords-1st-beat chords-1st-beat
  :chords-pitch-dur pitch-dur-chords
  :chords-pitch-onset pitch-onset-chords 
  :chords-pitch-dur-onset pitch-dur-onset-chords
  :chords-pitch-dur-include pitch-dur-chords-include
  :chords-pitch-onset-include pitch-onset-chords-include
  :chords-pitch-dur-onset-include pitch-dur-onset-chords-include
  :midics-domain midics-domain))

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
   ;(slot-value domain 'chords-pitch-onset)   
   ;(slot-value domain 'chords-pitch-dur-onset)
   (slot-value domain 'midics-domain)))


 (defclass screamer-measures-domain (screamer-score-domain)
   ((pitch :initform nil :initarg :pitch :reader pitch :writer set-pitch :documentation "list of lists")
    (pitch-include :initform nil :initarg :pitch-include :reader pitch-include :writer set-pitch-include :documentation "list of lists")
    (chords :initform nil :initarg :chords :reader chords :writer set-chords :documentation "list of lists")
    (chords-include :initform nil :initarg :chords-include :reader chords-include :writer set-chords-include :documentation "list of lists")	
	(pitch-dur :initform nil :initarg :pitch-dur :reader pitch-dur :writer set-pitch-dur :documentation "list of lists")
	(pitch-onset :initform nil :initarg :pitch-onset :reader pitch-onset :writer set-pitch-onset :documentation "list of lists")
	(pitch-dur-onset :initform nil :initarg :pitch-dur-onset :reader pitch-dur-onset :writer set-pitch-dur-onset :documentation "list of lists")
    (pitch-dur-include :initform nil :initarg :pitch-dur-include :reader pitch-dur-include :writer set-pitch-dur-include :documentation "list of lists")
	(pitch-onset-include :initform nil :initarg :pitch-onset-include :reader pitch-onset-include :writer set-pitch-onset-include :documentation "list of lists")
	(pitch-dur-onset-include :initform nil :initarg :pitch-dur-onset-include :reader pitch-dur-onset-include :writer set-pitch-dur-onset-include :documentation "list of lists")
    (chords-on-beat :initform nil :initarg :chords-on-beat :reader chords-on-beat :writer set-chords-on-beat :documentation "list of lists")
    (chords-off-beat :initform nil :initarg :chords-off-beat :reader chords-off-beat :writer set-chords-off-beat :documentation "list of lists")
	(chords-1st-beat :initform nil :initarg :chords-1st-beat :reader chords-1st-beat :writer set-chords-1st-beat :documentation "list of lists")
    (chords-pitch-dur :initform nil :initarg :chords-pitch-dur :reader chords-pitch-dur :writer set-chords-pitch-dur :documentation "list of lists")
    (chords-pitch-onset :initform nil :initarg :chords-pitch-onset :reader chords-pitch-onset :writer set-chords-pitch-onset :documentation "list of lists")		
    (chords-pitch-dur-onset :initform nil :initarg :chords-pitch-dur-onset :reader chords-pitch-dur-onset :writer set-chords-pitch-dur-onset :documentation "list of lists")
	(chords-pitch-dur-include :initform nil :initarg :chords-pitch-dur-include :reader chords-pitch-dur-include :writer set-chords-pitch-dur-include :documentation "list of lists")
    (chords-pitch-onset-include :initform nil :initarg :chords-pitch-onset-include :reader chords-pitch-onset-include :writer set-chords-pitch-onset-include :documentation "list of lists")	
    (chords-pitch-dur-onset-include :initform nil :initarg :chords-pitch-dur-onset-include :reader chords-pitch-dur-onset-include :writer set-chords-pitch-dur-onset-include :documentation "list of lists")	
    (midics-domain :initform nil :initarg :midics-domain :reader midics-domain :writer set-midics-domain :documentation "list of lists"))
   (:documentation "A simple container for screamer-measures-domain.
 A description of each slot is presented below:
   :PITCH = list of lists of screamer variables (a-member-ofv or a-random-member-ofv) without rests (nil).
   :PITCH-INCLUDE = list of lists of screamer variables (a-member-ofv or a-random-member-ofv) with rests.
   :CHORDS = list of lists of screamer variables organized as chords, without rests.
   :CHORDS-INCLUDE = list of lists of screamer variables organized as chords, with rests.  
   :PITCH-DUR  = list of lists of screamer variables. Each sublist contains two arguments (pitch-variable dur). Include rests.
   :PITCH-ONSETS   = list of lists of screamer variables and ratios. Each sublist contains two arguments (pitch-variable onset). Include rests.
   :PITCH-DUR-ONSETS   = list of lists of screamer variables and ratios. Each sublist contains three arguments (pitch-variable onset). Include rests.
   :PITCH-VARS-WITH-RESTS = list of lists of screamer variables with rests (nil). Used to build measures-domain.
   :PITCH-DUR-INCLUDE  = list of lists of screamer variables. Each sublist contains two arguments (pitch-variable dur). Include rests.
   :PITCH-ONSETS-INCLUDE     = list of lists of screamer variables and ratios. Each sublist contains two arguments (pitch-variable onset). Include rests.
   :PITCH-DUR-ONSETS-INCLUDE     = list of lists of screamer variables and ratios. Each sublist contains three arguments (pitch-variable onset). Include rests.    
   :CHORD-ON-BEAT = list of lists of screamer variables organized as chords. Contains only the first chord of each beat.  
   :CHORDS-OFF-BEAT = list of lists of screamer variables organized as chords, removing the first chord of each beat. 
   :CHORDS-1ST-BEAT = list of lists of screamer variables organized as chords. Contains only the first beat of each measure.
   :CHORDS-PITCH-DUR = TODO doc.
   :CHORDS-PITCH-ONSET = TODO doc.   
   :CHORDS-PITCH-DUR-ONSET = TODO doc.
   :CHORDS-PITCH-DUR-INCLUDE = TODO doc.
   :CHORDS-PITCH-ONSET-INCLUDE = TODO doc.  
   :CHORDS-PITCH-DUR-ONSET-INCLUDE = TODO doc. 
   :MIDICS-DOMAIN = list of lists of midicents, one list for each voice."))

(defmethod screamer-measures-domain-p ((self screamer-measures-domain)) t)
(defmethod screamer-measures-domain-p ((self t )) nil )
(defmethod screamer-score-domain-p ((self screamer-measures-domain)) t)

;; INCLUDE 
(defmethod make-measures-domain ((pitch list) (pitch-include list) (chords list) (chords-include list)(pitch-durs list) (pitch-onset list)
								 (pitch-dur-onset list) (pitch-durs-include list) (pitch-onset-include  list) (pitch-dur-onset-include  list) (chords-on-beat list) (chords-off-beat list) (chords-1st-beat list)
								 (pitch-dur-chords list) (pitch-onset-chords list) (pitch-dur-onset-chords list) (pitch-dur-chords-include list)
								   (pitch-onset-chords-include list)(pitch-dur-onset-chords-include list) (midics-domain list))
  (make-instance 'screamer-measures-domain
   :pitch pitch
   :pitch-include pitch-include
   :chords chords
   :chords-include chords-include
   :pitch-dur pitch-durs
   :pitch-onset pitch-onset
   :pitch-dur-onset pitch-dur-onset
   :pitch-dur-include pitch-durs-include
   :pitch-onset-include pitch-onset-include
   :pitch-dur-onset-include pitch-dur-onset-include
   :chords-on-beat chords-on-beat
   :chords-off-beat chords-off-beat
   :chords-1st-beat chords-1st-beat
   :chords-pitch-dur pitch-dur-chords
   :chords-pitch-onset pitch-onset-chords  
   :chords-pitch-dur-onset pitch-dur-onset-chords
   :chords-pitch-dur-include pitch-dur-chords-include
   :chords-pitch-onset-include pitch-onset-chords-include
   :chords-pitch-dur-onset-include pitch-dur-onset-chords-include  
   :midics-domain midics-domain))

(defmethod list-all-slots ((domain screamer-measures-domain))
 (list
  (slot-value domain 'pitch)
  (slot-value domain 'chords)
  (slot-value domain 'pitch-dur)
  (slot-value domain 'pitch-dur-onset)
  ;(slot-value domain 'chords-on-beat)
  ;(slot-value domain 'chords-off-beat)
  ;(slot-value domain 'chords-1st-beat)  
  ;(slot-value domain 'chords-pitch-dur)
  ;(slot-value domain 'chords-pitch-onset)  
  ;(slot-value domain 'chords-pitch-dur-onset)
  (slot-value domain 'midics-domain)))

(defclass screamer-all-domains (screamer-score-domain)
  ((variables-domain :initform nil :initarg :var-domain :reader var-domain :writer set-var-domain :documentation "screamer-variables-domain object")
   (measures-domain :initform nil :initarg :mes-domain :reader mes-domain :writer set-mes-domain :documentation "screamer-measures-domain object"))
  (:documentation "A simple container for screamer-variable-domains and screamer-measures-domains.
A description of each slot is presented below:
  :VARIABLES-DOMAIN = (see class difinition of screamer-variables-domain).
  :MEASURES-DOMAIN = (see class difinition of screamer-measures-domain)"))

(defmethod screamer-all-domains-p ((self screamer-all-domains)) t)
(defmethod screamer-all-domains-p ((self t )) nil)
(defmethod screamer-score-domain-p ((self screamer-all-domains)) t)

(defmethod make-screamer-all-domains ((vars-domain screamer-variables-domain) (mes-domain list))
 (make-instance 'screamer-all-domains
	   :var-domain vars-domain
	   :mes-domain mes-domain))

(defmethod get-one-voice-domain ((self cs-one-voice) (domain screamer-variables-domain))
 (let* ((rests (get-rests self))
	    (fun (read-from-string (if (string-equal rests "include")
		                           (concatenate 'string (get-domain self) "-" rests)
								   (get-domain self)))))
  (funcall fun domain)))

(defmethod get-one-voice-domain ((self cs-one-voice) (domain screamer-measures-domain))
 (let* ((rests (get-rests self))
	    (fun (read-from-string (if (string-equal rests "include")
		                           (concatenate 'string (get-domain self) "-" rests)
								   (get-domain self)))))
  (funcall fun domain)))

(defmethod get-one-voice-domain ((self cs-one-voice) (domain screamer-all-domains))
 (get-one-voice-domain self (var-domain domain)))
 
 (defun rec-merge-domain (domains mes-length)
 (cond ((= 1 mes-length) domains)
        (t (let* ((doms domains)
                  (merge-once (apply #'x-append  (list (pop doms) (pop doms)))))
	        (if (null doms)
		         merge-once
	            (rec-merge-domain (x-append (list merge-once) doms) (1- mes-length)))))))
	
 (defmethod get-one-voice-domain ((self cs-one-voice) (domain list))
 "List of measures."
 (let ((measures (mat-trans (loop for mes in domain
	                   collect (get-one-voice-domain self mes)))))
(loop for voice in measures 
      collect (rec-merge-domain voice (length voice)))))					   

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

(defmethod get-chord-beat ((self cs-harmony) (domain list))
 "List of measures."
(let ((measures (mat-trans (loop for mes in domain
	                   collect (get-chord-beat self mes)))))
(loop for voice in measures 
      collect (rec-merge-domain voice (length voice)))))

(defmethod get-chord-beat ((self cs-harmony) (domain screamer-all-domains))
 (get-chord-beat self (var-domain domain)))

(defmethod get-chord-domain ((self cs-harmony) (domain screamer-variables-domain))
  (let* ((rests (get-rests self))
	    (fun (read-from-string (if (string-equal rests "include")
		                           (concatenate 'string (get-domain self) "-" rests)
								   (get-domain self)))))
  (funcall fun domain)))
  
(defmethod get-chord-domain ((self cs-harmony) (domain screamer-measures-domain))
  (let* ((rests (get-rests self))
	    (fun (read-from-string (if (string-equal rests "include")
		                           (concatenate 'string (get-domain self) "-" rests)
								   (get-domain self)))))
  (funcall fun domain)))
   
(defmethod get-chord-domain ((self cs-harmony) (domain screamer-all-domains))
   (get-chord-domain self (var-domain domain)))     
  
 (defmethod get-chord-domain ((self cs-harmony) (domain list))
  "List of measures."
 (let ((measures (mat-trans (loop for mes in domain
	                   collect (get-chord-domain self mes)))))
(loop for voice in measures 
      collect (rec-merge-domain voice (length voice)))))

