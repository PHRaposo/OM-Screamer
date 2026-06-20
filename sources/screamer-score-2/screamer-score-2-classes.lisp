;; Copyright (c) 2025 Paulo Henrique Raposo

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

(in-package :screamer-score)


;;;; ===========================================================================
;;;; DOMAIN CONTAINER
;;;; ===========================================================================

(defclass ss2-domain ()
  ((spec :initform nil
         :initarg  :spec
         :accessor ss2-domain-spec
         :documentation "Plist produced by METRIC-DOMAIN, RHYTHM-DOMAIN, PITCH-DOMAIN, or VELOCITY-DOMAIN."))
  (:documentation "Lightweight container for a SCREAMER-SCORE-2 domain spec.
Created by MK-METRIC-DOMAIN, MK-RHYTHM-DOMAIN, MK-PITCH-DOMAIN, or MK-VEL-DOMAIN.
Pass to SCREAMER-SCORE-2 as METRIC-DOM, RHYTHM-DOMS, or PITCH-DOMS arguments."))

(defmethod ss2-domain-p ((self ss2-domain)) t)
(defmethod ss2-domain-p ((self t)) nil)


;;;; ===========================================================================
;;;; RICH METRIC DOMAIN (with per-TS beat specs)
;;;; ===========================================================================

(defclass metric-domain-spec ()
  ((ts-domain  :initarg :ts-domain  :accessor mds-ts-domain)
   (beat-map   :initarg :beat-map   :accessor mds-beat-map  :initform nil))
  (:documentation "Rich metric domain with per-TS beat specs. Created by METRIC-DOMAIN-FULL.
BEAT-MAP: alist ((ts . beat-spec) ...) where beat-spec is nil (auto=1/denom),
  a rational (uniform override, e.g. 3/8), or a list (non-uniform, e.g. (2/8 3/8))."))


;;;; ===========================================================================
;;;; CHORD PITCH DOMAIN (polyphonic voice marker)
;;;; ===========================================================================

(defclass chord-domain ()
  ((domain
    :initarg :domain :accessor chord-domain-domain
    :documentation
    "Pool of MIDI pitches (integers 0-127) used to build each chord.
Every chord-note in every chord-slot of the voice picks from this pool.
The pool may contain repeated values; ordering and uniqueness inside a
chord are not enforced.")
   (notes-per-chord
    :initarg :notes-per-chord :accessor chord-domain-notes-per-chord
    :documentation
    "Number of notes per chord-slot. Either an integer (every slot has
the same number of notes) or a list of integers used as a circular
specification (slot k has size (nth (mod k (length list)) list))."))
  (:documentation
   "Polyphonic pitch-domain marker passed in place of a flat MIDI list to
MK-VOICE-DOMAIN. When the engine sees a CHORD-DOMAIN at the pitch
position of a voice, it builds N pitch variables per slot (instead of
one) using DOMAIN, with the standard rest sentinel -1 added to each
variable's domain. Rest-link is bidirectional and binds the whole chord
together: a slot with negative rhythm forces all N pitch variables to
-1 simultaneously, and a slot with non-negative rhythm forbids -1 in
every pitch variable."))

(defmethod chord-domain-p ((self chord-domain)) t)
(defmethod chord-domain-p ((self t)) nil)

(defun chord-pitch-spec-p (pd-spec)
  "T if PD-SPEC is a pitch-spec carrying a CHORD-DOMAIN as its :DOMAIN value
(polyphonic voice). Used by the engine to dispatch between flat and
per-slot-N pitch-variable construction."
  (and pd-spec (chord-domain-p (getf pd-spec :domain))))

(defun resolve-notes-per-chord (notes-per-chord k)
  "Number of notes for slot K given the NOTES-PER-CHORD spec.
Integer N -> N for every slot. List '(n0 n1 n2 ...) -> circular: slot k
has size (nth (mod k (length list)) list)."
  (cond
    ((integerp notes-per-chord) notes-per-chord)
    ((and (listp notes-per-chord) notes-per-chord)
     (nth (mod k (length notes-per-chord)) notes-per-chord))
    (t (error "Invalid NOTES-PER-CHORD spec: ~S" notes-per-chord))))


;;;; ===========================================================================
;;;; CONSTRAINT CONTAINER
;;;; ===========================================================================

(defclass ss2-constraint ()
  ((spec :initform nil
         :initarg  :spec
         :accessor ss2-constraint-spec
         :documentation "Plist produced by MAKE-SCREAMER-SCORE-CONSTRAINT."))
  (:documentation "Lightweight container for a SCREAMER-SCORE-2 constraint spec.
Created by MK-ONE-VOICE-CONSTRAINT, MK-HARMONY-CONSTRAINT, or MK-METRIC-CONSTRAINT.
Pass to SCREAMER-SCORE-2 as CONSTRAINTS argument (single instance or list)."))

(defmethod ss2-constraint-p ((self ss2-constraint)) t)
(defmethod ss2-constraint-p ((self t)) nil)


;;;; ===========================================================================
;;;; VOICE DOMAIN CONTAINER  (search voice - mk-voice-domain)
;;;; ===========================================================================

(defclass screamer-voice-domain ()
  ((rhythm-spec
    :initform nil :initarg :rhythm-spec :accessor svd-rhythm-spec
    :documentation
    "Rhythm domain plist (:n-notes N :domain (...)) for this voice.")
   (pitch-spec
    :initform nil :initarg :pitch-spec :accessor svd-pitch-spec
    :documentation
    "Pitch domain plist (:domain (...)) for this voice.
Notes mode:  domain is a list of MIDI integers e.g. (60 62 64 67).
Chords mode: domain is a list of complete chords e.g. ((60 64 67) (62 65 69)).")
   (vel-spec
    :initform nil :initarg :vel-spec :accessor svd-vel-spec
    :documentation
    "Velocity domain plist (:domain (...)), or nil (no velocity search)."))
  (:documentation
   "Bundles rhythm, pitch, and velocity SEARCH DOMAINS for one voice.
Created by MK-VOICE-DOMAIN.  Pass to SCREAMER-SCORE-2 as VOICE-DOMS.
For a fully fixed (locked) voice created from an OM VOICE object, use
MK-SCREAMER-VOICE instead."))

(defmethod screamer-voice-domain-p ((self screamer-voice-domain)) t)
(defmethod screamer-voice-domain-p ((self t)) nil)


;;;; ===========================================================================
;;;; LOCKED VOICE CONTAINER  (fixed voice - mk-screamer-voice)
;;;; ===========================================================================

(defclass screamer-voice ()
  ((rhythm-spec
    :initform nil :initarg :rhythm-spec :accessor sv-rhythm-spec
    :documentation
    "Locked rhythm plist (:n-notes N :domain DURS :locked t).
DURS is a list of concrete rational durations (positive=note, negative=rest).")
   (pitch-spec
    :initform nil :initarg :pitch-spec :accessor sv-pitch-spec
    :documentation
    "Locked pitch plist (:domain PITCHES :locked t).
PITCHES is a list with one element per slot (note OR rest):
  atom  (e.g. 60)           - single note (MIDI number)
  list  (e.g. (60 64 67))   - chord (list of MIDI numbers)
  -1                        - rest slot (engine sentinel)")
   (vel-spec
    :initform nil :initarg :vel-spec :accessor sv-vel-spec
    :documentation
    "Locked velocity plist (:domain VELS :locked t).
VELS is a list with one element per slot (note OR rest):
  integer 0-127  - velocity at note slot
  -1             - rest slot (engine sentinel)
The whole spec may be nil if no velocity was provided.")
   (metric-spec
    :initform nil :initarg :metric-spec :accessor sv-metric-spec
    :documentation
    "Locked metric plist (:domain TS-SEQ :locked t).
TS-SEQ is a list of (num denom) pairs, one per measure - e.g. ((4 4) (3 4) (4 4)).
Used by SCREAMER-SCORE-2 to validate or auto-derive the metric domain."))
  (:documentation
   "A fully locked voice created from a concrete sequence (e.g. an OM VOICE object).
All domains carry :locked t so no search is performed for this voice.
Created by MK-SCREAMER-VOICE (OM interface).  Pass to SCREAMER-SCORE-2 as VOICE-DOMS
alongside SCREAMER-VOICE-DOMAIN objects (from MK-VOICE-DOMAIN) freely.

Pitch convention (consistent with free-search voices):
  Single note -> atom (e.g. 60)
  Chord       -> list (e.g. (60 64 67))
  Rest        -> -1   (engine sentinel)"))

(defmethod screamer-voice-p ((self screamer-voice)) t)
(defmethod screamer-voice-p ((self t)) nil)

