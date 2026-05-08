;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;
;;;; Copyright (c) 2026 Paulo Henrique Raposo
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;;; this software and associated documentation files (the "Software"), to deal in
;;;; the Software without restriction, including without limitation the rights to
;;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;;;; the Software, and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; The above copyright and authorship notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :screamer-score)

;;;; ===========================================================================
;;;; SCREAMER-NOTE STRUCT
;;;; ===========================================================================

(defstruct (screamer-note (:print-function print-screamer-note)
                          (:predicate screamer-note?))
  pitch                                  ; Screamer var, MIDI pitch
  pc-slot                                ; Screamer var, (modv pitch 12); mutated to NIL via noticer when pitch=-0.5
  rhythm                                 ; Screamer var (negative = rest)
  onset                                  ; Screamer var (absolute timing)
  vel                                    ; Screamer var velocity (NIL when unused)
  is-note?                               ; Screamer boolean var, (>=v rhythm 0)
  (voice-idx 0 :type integer)            ; voice index (constant)
  (position 0 :type integer))            ; index within voice (constant)

(defun screamer-note-pc (note)
  "PC accessor with rest-aware NIL sentinel.
Returns NIL when the note slot is rest (pitch bound to -0.5, or pc-slot
already NIL'd by the noticer); otherwise returns the underlying pc var.
With pitch sentinel -0.5 (float), pc-slot via (modv pitch 12) produces
11.5 (float) for rests -- distinct from any integer pc, so memberv pc
constraints naturally exclude rests. The NIL sentinel here is for slot-
level user code that prefers a clean nil marker."
  (cond ((null (screamer-note-pc-slot note)) nil)
        ((screamer::known? (screamer::=v (screamer-note-pitch note) -0.5)) nil)
        (t (screamer-note-pc-slot note))))

(defun print-screamer-note (note stream depth)
  "User-friendly print: voice, position, and key slots.
Format: [NOTE V<voice-idx> P<position> pitch:<v> rhythm:<v> onset:<v> pc:<v> [vel:<v>]]"
  (declare (ignore depth))
  (format stream "[NOTE V~A P~A"
          (screamer-note-voice-idx note)
          (screamer-note-position note))
  (format stream " pitch:~S"  (screamer-note-pitch note))
  (format stream " rhythm:~S" (screamer-note-rhythm note))
  (format stream " onset:~S"  (screamer-note-onset note))
  (format stream " pc:~S"     (screamer-note-pc note))
  (when (screamer-note-vel note)
    (format stream " vel:~S" (screamer-note-vel note)))
  (format stream "]"))

;;;; ===========================================================================
;;;; SCREAMER-CHORD STRUCT
;;;; ===========================================================================

(defstruct (screamer-chord (:print-function print-screamer-chord)
                           (:predicate screamer-chord?))
  pitch                                  ; list of N Screamer vars (each a MIDI pitch)
  pc-slot                                ; list of N Screamer vars, each (modv p 12); each element mutated to NIL via noticer when its pitch=-0.5
  rhythm                                 ; single Screamer var (negative = rest); shared by the whole chord
  onset                                  ; single Screamer var
  vel                                    ; single Screamer velocity var (NIL when unused)
  is-note?                               ; Screamer boolean var, (>=v rhythm 0)
  (voice-idx 0 :type integer)            ; voice index (constant)
  (position 0 :type integer))            ; chord index within voice (constant)

(defun screamer-chord-pc (chord)
  "PC accessor for chord with rest-aware NIL sentinel per element.
Returns a list of N elements, one per chord-note. Each element is NIL
when the corresponding pitch is bound to -0.5 (or its pc-slot was already
NIL'd by the noticer); otherwise the underlying pc var. Rest-link forces
all elements NIL together when the slot is a rest."
  (loop for p-var  in (screamer-chord-pitch chord)
        for pc-var in (screamer-chord-pc-slot chord)
        collect (cond ((null pc-var) nil)
                      ((screamer::known? (screamer::=v p-var -0.5)) nil)
                      (t pc-var))))

(defun print-screamer-chord (chord stream depth)
  "User-friendly print: voice, position, key slots.
Format: [CHORD V<voice-idx> P<position> pitch:<list> rhythm:<v> onset:<v> pc:<list> [vel:<v>]]"
  (declare (ignore depth))
  (format stream "[CHORD V~A P~A"
          (screamer-chord-voice-idx chord)
          (screamer-chord-position chord))
  (format stream " pitch:~S"  (screamer-chord-pitch chord))
  (format stream " rhythm:~S" (screamer-chord-rhythm chord))
  (format stream " onset:~S"  (screamer-chord-onset chord))
  (format stream " pc:~S"     (screamer-chord-pc chord))
  (when (screamer-chord-vel chord)
    (format stream " vel:~S" (screamer-chord-vel chord)))
  (format stream "]"))


;;;; ===========================================================================
;;;; SLOT EXTRACTORS (helpers for the constraint API)
;;;; ===========================================================================

(defun pitches-of (notes) (mapcar #'screamer-note-pitch notes))
(defun rhythms-of (notes) (mapcar #'screamer-note-rhythm notes))
(defun onsets-of  (notes) (mapcar #'screamer-note-onset notes))
(defun vels-of    (notes) (mapcar #'screamer-note-vel notes))
(defun pcs-of     (notes) (mapcar #'screamer-note-pc notes))
(defun is-notes   (notes) (mapcar #'screamer-note-is-note? notes))


;;;; ===========================================================================
;;;; SCREAMER-TIME-SIGNATURE STRUCT
;;;; ===========================================================================

(defstruct (screamer-time-signature (:print-function print-screamer-ts)
                                    (:predicate screamer-ts?))
  ts                                     ; Screamer var, holds (top bottom) cons
  top                                    ; Screamer var, (carv ts) -- numerator
  bottom                                 ; Screamer var, (cadrv ts) -- denominator
  unit                                   ; Screamer var, 1/bottom (beat value)
  onset                                  ; Screamer var, onset of this measure
  duration                               ; Screamer var, top * unit (= measure duration)
  (measure-idx 0 :type integer))         ; measure index (constant)

(defun print-screamer-ts (sig stream depth)
  "User-friendly print: measure index and key slots.
Format: [TS M<idx> ts:<v> top:<v> bottom:<v> onset:<v> duration:<v>]"
  (declare (ignore depth))
  (format stream "[TS M~A"
          (screamer-time-signature-measure-idx sig))
  (format stream " ts:~S"       (screamer-time-signature-ts sig))
  (format stream " top:~S"      (screamer-time-signature-top sig))
  (format stream " bottom:~S"   (screamer-time-signature-bottom sig))
  (format stream " unit:~S"     (screamer-time-signature-unit sig))
  (format stream " onset:~S"    (screamer-time-signature-onset sig))
  (format stream " duration:~S" (screamer-time-signature-duration sig))
  (format stream "]"))

;;;; ===========================================================================
;;;; SLOT EXTRACTORS for signatures
;;;; ===========================================================================

(defun tops-of      (sigs) (mapcar #'screamer-time-signature-top sigs))
(defun bottoms-of   (sigs) (mapcar #'screamer-time-signature-bottom sigs))
(defun units-of     (sigs) (mapcar #'screamer-time-signature-unit sigs))
(defun ts-onsets-of (sigs) (mapcar #'screamer-time-signature-onset sigs))
(defun durations-of (sigs) (mapcar #'screamer-time-signature-duration sigs))

;;;; ===========================================================================
;;;; VARIABLES-IN / APPLY-SUBSTITUTION unified (notes + signatures)
;;;; ===========================================================================

(defun variables-in-list (items)
  "Extract PRIMARY vars (driven by search forcing) from a list of
screamer-notes, screamer-chords and/or screamer-time-signatures. For notes:
pitch, rhythm, vel (vel only when present). For chords: each pitch in the
list, rhythm, vel. For signatures: ts. Derived slots (pc, onset, is-note?,
top, bottom, unit, duration) auto-bind via noticer and need no forcing."
  (let ((acc '()))
    (dolist (it items)
      (etypecase it
        (screamer-note
         (let ((v (screamer-note-vel it)))
           (when v (push v acc)))
         (push (screamer-note-rhythm it) acc)
         (push (screamer-note-pitch it) acc))
        (screamer-chord
         (let ((v (screamer-chord-vel it)))
           (when v (push v acc)))
         (push (screamer-chord-rhythm it) acc)
         (dolist (p (screamer-chord-pitch it))
           (push p acc)))
        (screamer-time-signature
         (push (screamer-time-signature-ts it) acc))))
    (screamer::variables-in (nreverse acc))))

(defun apply-substitution-list (items)
  "Substitute Screamer-var slots in a list of screamer-notes,
screamer-chords and/or screamer-time-signatures. Dispatches per item via
etypecase, accepting a mixed list."
  (mapcar
    #'(lambda (it)
        (etypecase it
          (screamer-note
           (let* ((r     (screamer::apply-substitution (screamer-note-rhythm it)))
                  (rest? (and (numberp r) (minusp r))))
             (make-screamer-note
               :pitch     (screamer::apply-substitution (screamer-note-pitch it))
               :pc-slot   (if rest? nil
                              (screamer::apply-substitution (screamer-note-pc-slot it)))
               :rhythm    r
               :onset     (screamer::apply-substitution (screamer-note-onset it))
               :vel       (let ((v (screamer-note-vel it)))
                            (when v (screamer::apply-substitution v)))
               :is-note?  (screamer::apply-substitution (screamer-note-is-note? it))
               :voice-idx (screamer-note-voice-idx it)
               :position  (screamer-note-position it))))
          (screamer-chord
           (let* ((r     (screamer::apply-substitution (screamer-chord-rhythm it)))
                  (rest? (and (numberp r) (minusp r))))
             (make-screamer-chord
               :pitch     (mapcar #'screamer::apply-substitution
                                  (screamer-chord-pitch it))
               :pc-slot   (if rest?
                              (make-list (length (screamer-chord-pc-slot it))
                                         :initial-element nil)
                              (mapcar #'(lambda (pc) (and pc (screamer::apply-substitution pc)))
                                      (screamer-chord-pc-slot it)))
               :rhythm    r
               :onset     (screamer::apply-substitution (screamer-chord-onset it))
               :vel       (let ((v (screamer-chord-vel it)))
                            (when v (screamer::apply-substitution v)))
               :is-note?  (screamer::apply-substitution (screamer-chord-is-note? it))
               :voice-idx (screamer-chord-voice-idx it)
               :position  (screamer-chord-position it))))
          (screamer-time-signature
           (make-screamer-time-signature
             :ts          (screamer::apply-substitution (screamer-time-signature-ts it))
             :top         (screamer::apply-substitution (screamer-time-signature-top it))
             :bottom      (screamer::apply-substitution (screamer-time-signature-bottom it))
             :unit        (screamer::apply-substitution (screamer-time-signature-unit it))
             :onset       (screamer::apply-substitution (screamer-time-signature-onset it))
             :duration    (screamer::apply-substitution (screamer-time-signature-duration it))
             :measure-idx (screamer-time-signature-measure-idx it)))))
    items))

;;;; ===========================================================================
;;;; SCORE-SOLUTION (notes + optional signatures)
;;;; ===========================================================================

(screamer::defun score-solution (notes ordering-force-function &optional signatures)
  "Analogue of screamer::solution for lists of screamer-note (and
optionally screamer-time-signatures). Forces all Screamer-vars in the
slots via ORDERING-FORCE-FUNCTION and returns a list of substituted
notes. When SIGNATURES is provided, returns
(values substituted-notes substituted-signatures).

Note: defined with screamer::defun because it calls FUNCALL-NONDETERMINISTIC."
  (screamer::funcall-nondeterministic
    (screamer::value-of ordering-force-function)
    (variables-in-list (append notes signatures)))
  (if signatures
      (values (apply-substitution-list notes)
              (apply-substitution-list signatures))
      (apply-substitution-list notes)))

;;;; ===========================================================================
;;;; CONSTRUCTORS
;;;; ===========================================================================

(defun make-voice-notes (n-slots p-vars r-vars o-vars v-vars voice-idx)
  "Build a list of N-SLOTS screamer-notes for one voice.
- p-vars/r-vars/o-vars: lists of Screamer-vars (pitch, rhythm, onset).
- v-vars: list or NIL (velocity optional).
- voice-idx: voice index (constant).
Each note has pc-slot = (modv pitch 12) precomputed and is-note? = (>=v rhythm 0).
Attaches a noticer on each pitch var: when bound to -0.5, NILs the pc-slot
of the corresponding note via (local setf), preserving reversal on backtrack."
  (loop for k from 0 below n-slots
        for p-var = (nth k p-vars)
        for note  = (make-screamer-note
                      :pitch     p-var
                      :pc-slot   (screamer+::modv p-var 12)
                      :rhythm    (nth k r-vars)
                      :onset     (nth k o-vars)
                      :vel       (when v-vars (nth k v-vars))
                      :is-note?  (screamer::>=v (nth k r-vars) 0)
                      :voice-idx voice-idx
                      :position  k)
        do (let ((captured-note note)
                 (captured-p    p-var))
             (screamer::attach-noticer!
               #'(lambda ()
                   (when (and (screamer::bound? captured-p)
                              (= (screamer::value-of captured-p) -0.5))
                     (screamer::local
                       (setf (screamer-note-pc-slot captured-note) nil))))
               captured-p))
        collect note))

(defun make-voice-chords (n-slots p-vars-per-slot r-vars o-vars v-vars voice-idx)
  "Build a list of N-SLOTS screamer-chords for one polyphonic voice.
- p-vars-per-slot: list of N-SLOTS sublists; each sublist holds the N
  pitch-vars of the chord at that slot (N varies per chord-domain
  notes-per-chord).
- r-vars/o-vars: lists of Screamer-vars, one per slot (shared across all
  notes of the chord).
- v-vars: list or NIL (velocity optional, one per slot).
- voice-idx: voice index (constant).
Each chord has pc-slot precomputed per-note as (modv p 12) and is-note?
= (>=v rhythm 0). Each pitch var receives a noticer that NILs its
corresponding entry in pc-slot when bound to -0.5."
  (loop for k from 0 below n-slots
        for p-list = (nth k p-vars-per-slot)
        for chord  = (make-screamer-chord
                       :pitch     p-list
                       :pc-slot   (mapcar #'(lambda (p) (screamer+::modv p 12)) p-list)
                       :rhythm    (nth k r-vars)
                       :onset     (nth k o-vars)
                       :vel       (when v-vars (nth k v-vars))
                       :is-note?  (screamer::>=v (nth k r-vars) 0)
                       :voice-idx voice-idx
                       :position  k)
        do (loop for p-var in p-list
                 for idx from 0
                 do (let ((captured-chord chord)
                          (captured-p     p-var)
                          (captured-idx   idx))
                      (screamer::attach-noticer!
                        #'(lambda ()
                            (when (and (screamer::bound? captured-p)
                                       (= (screamer::value-of captured-p) -0.5))
                              ;; Rebuild pc-slot via slot-level setf so that
                              ;; SCREAMER::LOCAL trails the change correctly:
                              ;; SCREAMER's LOCAL handles SETF on a slot place
                              ;; but not (SETF (NTH ...)) reliably, since
                              ;; setf-nth expands to RPLACA which LOCAL treats
                              ;; as a global side-effect.
                              ;;
                              ;; The new list is computed OUTSIDE the LOCAL
                              ;; body. LOOP forms inside LOCAL trip the
                              ;; SCREAMER code walker (LOOP-FINISH, GO tags
                              ;; produced by the expansion are seen as
                              ;; non-symbol function names by the substitutor).
                              (let ((new-pc-slot
                                      (loop for pc in (screamer-chord-pc-slot
                                                        captured-chord)
                                            for i from 0
                                            collect (if (= i captured-idx) nil pc))))
                                (screamer::local
                                  (setf (screamer-chord-pc-slot captured-chord)
                                        new-pc-slot)))))
                        captured-p)))
        collect chord))

(defun make-time-signatures (ts-vars ts-onset-vars)
  "Build a list of screamer-time-signatures, one per measure.
- ts-vars: list of Screamer-vars, each holding a (top bottom) cons.
- ts-onset-vars: list of measure onsets (length = (length ts-vars) + 1,
  the last entry = end-of-piece, used to compute the last measure's duration).
Each signature has top/bottom/unit/duration precomputed."
  (loop for ts-var in ts-vars
        for onset-var in ts-onset-vars
        for next-onset in (rest ts-onset-vars)
        for k from 0
        for top-var = (screamer+::carv ts-var)
        for bottom-var = (screamer+::carv (screamer+::cdrv ts-var))
        collect (make-screamer-time-signature
                  :ts          ts-var
                  :top         top-var
                  :bottom      bottom-var
                  :unit        (screamer::funcallv #'(lambda (b) (/ 1 b)) bottom-var)
                  :onset       onset-var
                  :duration    (screamer::-v next-onset onset-var)
                  :measure-idx k)))
