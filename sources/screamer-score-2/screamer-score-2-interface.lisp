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

(in-package :om)


;;;; ===========================================================================
;;;; SHARED MENUS (built once at load time)
;;;; ===========================================================================

(defvar *variable-type-menu* nil
  "Menu pairs (\"vt\" \"vt\") for variable-type slots in mk-one-voice-constraint
and mk-measures-constraint. Built from screamer-score::*variable-type-options*
at load time. Long list (31 entries) -- LIST form in :menuins gives OM's
slider menu treatment.")

(setf *variable-type-menu*
      (loop for vt in screamer-score::*variable-type-options*
            collect (list vt vt)))


;;;; ===========================================================================
;;;; PART 1 - DOMAIN BUILDER BOXES
;;;; ===========================================================================
(defmethod! mk-metric-domain (&rest specs)
  :initvals '((6 8) 3/8 (5 8) (2/8 3/8))
  :indoc    '("list" "ratio or list")
  :doc      "Create a rich metric domain with per-TS beat specs for SCREAMER-SCORE-2.
SPECS: list of (ts beat-spec) pairs.
  ts:        (num denom) e.g. (4 4)
  beat-spec: nil (auto = 1/denom), rational (uniform), or list (non-uniform).

Returns a METRIC-DOMAIN-SPEC object. Pass to SCREAMER-SCORE-2 as METRIC-DOM."
  :icon     486
  (apply #'screamer-score::make-metric-domain specs))


(defmethod! mk-chord-domain ((domain list) (notes-per-chord t))
  :initvals '('(60 62 64 65 67 69 71) 4)
  :indoc    '("list of MIDI note numbers used as the chord pool"
              "integer (uniform N) or list of integers (circular per-slot N)")
  :doc      "Create a polyphonic pitch domain for one voice in SCREAMER-SCORE-2.

DOMAIN: list of MIDI note numbers (integers 0-127). Every chord-note
        picks from this pool. Repeated values allowed; ordering and
        uniqueness inside a chord are not enforced.
NOTES-PER-CHORD: integer or list.
  Integer N        -> every chord-slot has N notes.
  List '(3 4 5)    -> circular: slot 0 has 3 notes, slot 1 has 4, slot
                     2 has 5, slot 3 has 3 again, and so on.

Pass the result as the PITCH-DOM argument of MK-VOICE-DOMAIN to obtain
a polyphonic search voice. The engine builds N pitch variables per slot
and links them through the slot rhythm via bidirectional rest-link
(negative rhythm forces all N pitches to -1; non-negative rhythm forbids
-1 in any pitch). Inside constraint lambdas, PITCH and PC return lists;
DUR, ONSET and VEL return single shared variables.

Returns a CHORD-DOMAIN object."
  :icon     486
  (make-instance 'screamer-score::chord-domain
    :domain          domain
    :notes-per-chord notes-per-chord))


;;;; ===========================================================================
;;;; PART 2 - VOICE DOMAIN CONTAINER BOX
;;;; ===========================================================================

(defmethod! mk-voice-domain ((n-notes integer) (rhythm-dom list) (pitch-dom list)
                             &optional (vel-dom nil))
  :initvals '(4 (1/4 1/8 -1/8) (60 62 64 67) nil)
  :indoc    '("number of note/rest slots to generate for this voice"
              "rhythm domain - list of durations e.g. (1/4 1/8 -1/4)"
              "pitch domain - list of MIDI notes e.g. (60 62 64 67), or NIL to omit pitch search"
              "velocity domain - list of symbols/integers e.g. (mp mf f), or nil")
  :doc      "Create a monophonic search voice domain for SCREAMER-SCORE-2.

Bundles n-notes, rhythm, pitch, and velocity SEARCH DOMAINS for one voice.
For a fully fixed voice, use MK-SCREAMER-VOICE instead. For polyphonic
chord voices, build a CHORD-DOMAIN with MK-CHORD-DOMAIN and pass it to
the chord-domain method of MK-VOICE-DOMAIN.

N-NOTES:    Number of note/rest slots to search for this voice.
RHYTHM-DOM: List of allowed durations (positive=note, negative=rest).
PITCH-DOM:  List of MIDI integers, or NIL to omit pitch search.
VEL-DOM:    List of velocity symbols/integers, or NIL to omit.

Returns a SCREAMER-VOICE-DOMAIN object."
  :icon     486
  (screamer-score::make-voice-domain n-notes rhythm-dom pitch-dom vel-dom))


(defmethod! mk-voice-domain ((n-notes integer) (rhythm-dom list)
                             (pitch-dom screamer-score::chord-domain)
                             &optional (vel-dom nil))
  :initvals '(4 (1/4 1/8 -1/8) nil nil)
  :indoc    '("number of chord-slots to generate for this voice"
              "rhythm domain - list of durations e.g. (1/4 1/8 -1/4)"
              "chord-domain instance built with MK-CHORD-DOMAIN"
              "velocity domain - list of symbols/integers e.g. (mp mf f), or nil")
  :doc      "Create a polyphonic (chord) search voice domain for SCREAMER-SCORE-2.

Bundles n-notes (chord-slots), rhythm, chord-pitch and velocity SEARCH
DOMAINS for one voice. The engine builds N pitch variables per slot
(N coming from the CHORD-DOMAIN's NOTES-PER-CHORD spec) and links them
through the slot rhythm via bidirectional rest-link.

N-NOTES:    Number of chord-slots to search.
RHYTHM-DOM: List of allowed durations (positive=note, negative=rest);
            applies to the whole chord-slot.
PITCH-DOM:  CHORD-DOMAIN instance (built with MK-CHORD-DOMAIN).
VEL-DOM:    List of velocity symbols/integers (single per chord-slot),
            or NIL to omit.

Returns a SCREAMER-VOICE-DOMAIN object whose pitch-spec carries the
CHORD-DOMAIN value, signalling polyphonic search to the engine."
  :icon     486
  (screamer-score::make-voice-domain n-notes rhythm-dom pitch-dom vel-dom))


;;;; ===========================================================================
;;;; PART 3 - SLOT ACCESSORS FOR SCREAMER-NOTE / SCREAMER-TIME-SIGNATURE
;;;;
;;;; Short-name CLOS methods for use inside constraint lambdas.
;;;; Generics are interned in :om; specializers reference :screamer-score.
;;;; ===========================================================================

(defmethod pitch ((note screamer-score::screamer-note))
  (screamer-score::screamer-note-pitch note))

(defmethod dur ((note screamer-score::screamer-note))
  (screamer-score::screamer-note-rhythm note))

(defmethod onset ((note screamer-score::screamer-note))
  (screamer-score::screamer-note-onset note))

(defmethod vel ((note screamer-score::screamer-note))
  (screamer-score::screamer-note-vel note))

(defmethod pc ((note screamer-score::screamer-note))
  (screamer-score::screamer-note-pc note))

(defmethod is-note? ((note screamer-score::screamer-note))
  (screamer-score::screamer-note-is-note? note))

(defmethod voice-idx ((note screamer-score::screamer-note))
  (screamer-score::screamer-note-voice-idx note))

(defmethod note-position ((note screamer-score::screamer-note))
  (screamer-score::screamer-note-position note))

;;; Same generics dispatched on screamer-chord. Returned shape differs:
;;; pitch and pc return lists of N variables (or N NILs in pc when slot is rest);
;;; dur/onset/vel/is-note? return single shared variables.

(defmethod pitch ((chord screamer-score::screamer-chord))
  (screamer-score::screamer-chord-pitch chord))

(defmethod dur ((chord screamer-score::screamer-chord))
  (screamer-score::screamer-chord-rhythm chord))

(defmethod onset ((chord screamer-score::screamer-chord))
  (screamer-score::screamer-chord-onset chord))

(defmethod vel ((chord screamer-score::screamer-chord))
  (screamer-score::screamer-chord-vel chord))

(defmethod pc ((chord screamer-score::screamer-chord))
  (screamer-score::screamer-chord-pc chord))

(defmethod is-note? ((chord screamer-score::screamer-chord))
  (screamer-score::screamer-chord-is-note? chord))

(defmethod voice-idx ((chord screamer-score::screamer-chord))
  (screamer-score::screamer-chord-voice-idx chord))

(defmethod note-position ((chord screamer-score::screamer-chord))
  (screamer-score::screamer-chord-position chord))

(defmethod ts ((sig screamer-score::screamer-time-signature))
  (screamer-score::screamer-time-signature-ts sig))

(defmethod top ((sig screamer-score::screamer-time-signature))
  (screamer-score::screamer-time-signature-top sig))

(defmethod bottom ((sig screamer-score::screamer-time-signature))
  (screamer-score::screamer-time-signature-bottom sig))

(defmethod unit ((sig screamer-score::screamer-time-signature))
  (screamer-score::screamer-time-signature-unit sig))

(defmethod onset ((sig screamer-score::screamer-time-signature))
  (screamer-score::screamer-time-signature-onset sig))

(defmethod duration ((sig screamer-score::screamer-time-signature))
  (screamer-score::screamer-time-signature-duration sig))

(defmethod measure-idx ((sig screamer-score::screamer-time-signature))
  (screamer-score::screamer-time-signature-measure-idx sig))


;;;; ===========================================================================
;;;; PART 4 - CONSTRAINT BUILDER BOXES
;;;; ===========================================================================

(defmethod! mk-one-voice-constraint ((constraint-fn function)
                                     (voices t)
                                     (variable-type string)
                                     (input string)
                                     &key (rests "include") (index nil)
                                          (measure nil) (cs-mode "propagation"))
  :initvals '(nil '(0) "pitch" "list" "include" nil nil "propagation")
  :indoc    '("<lambda patch>"
              "nil = all voices, integer, or list of integers"
              "variable-type string (31 combinations of pitch/pc/dur/onset/vel)"
              "input mode string"
              "rests filter string"
              "note positions e.g. '(0 10), or nil"
              "measure e.g. '(0 1) each-independently or '((0 1)) fused, or nil"
              "constraint mode string")
  :menuins  (list (list 2 *variable-type-menu*)
                  (list 3 (list (list "list"     "list")
                                (list "n-inputs" "n-inputs")
                                (list "car-cdr"  "car-cdr")
                                (list "growing"  "growing")))
                  (list 4 (list (list "include" "include")
                                (list "exclude" "exclude")))
                  (list 7 (list (list "propagation" "propagation")
                                (list "backtrack"   "backtrack"))))
  :doc      "Create a one-voice constraint for SCREAMER-SCORE-2.

CONSTRAINT-FN: lambda whose signature depends on VARIABLE-TYPE and INPUT.
  Single-part variable-type (pitch / pc / dur / onset / vel):
    list      -> (lambda (vars) ...)        ; vars = flat list of Screamer vars
    n-inputs  -> (lambda (v1 v2 ...) ...)   ; arity = window size
    car-cdr   -> (lambda (head tail) ...)
    growing   -> (lambda (prefix) ...)
  Multi-part variable-type (e.g. pitch-pc, pitch-dur):
    list      -> (lambda (cols) ...)        ; cols = list of N sublists
                                              one per part in declared order
    n-inputs  -> (lambda (tuple1 tuple2 ...) ...) ; each tuple = list of N vars
    car-cdr   -> (lambda (head-tuple rest-tuples) ...)
    growing   -> (lambda (prefix-tuples) ...)

VOICES: nil (all voices), integer (single voice index), or list of indices.

VARIABLE-TYPE: 31 options listing which Screamer-vars the engine extracts
  from each note before calling the predicate. Single-part covers PITCH
  (MIDI 0-127), PC (pitch-class 0-11, mod 12 of pitch), DUR (rhythm),
  ONSET, VEL; multi-part composes 2..5 of these in canonical declaration
  order (pitch -> pc -> dur -> onset -> vel).

INPUT:  \"list\"     - fn receives the full extracted vars at once.
        \"n-inputs\" - fn receives N consecutive elements (vars or tuples).
        \"car-cdr\"  - fn receives (first rest) pairs.
        \"growing\"  - fn receives growing prefixes.
RESTS:  \"include\" (default) - fn receives all slots including rest slots.
        \"exclude\"            - fn receives only note slots (rests filtered).

MEASURE: list of application specs. Each spec is applied independently.
  number n   -> fn applied to events in measure n alone.
  list (n m) -> fn applied to events in measures n+m fused into one list.
  Mixed: '(0 (1 2) 3) -> fn applied to m0, then m1+m2 fused, then m3.

Returns an SS2-CONSTRAINT object. Pass to SCREAMER-SCORE-2 as CONSTRAINTS."
  :icon     486
  (screamer-score::make-one-voice-cs constraint-fn voices variable-type input
                                     :rests   rests
                                     :index   index
                                     :measure measure
                                     :cs-mode cs-mode))


(defmethod! mk-metric-constraint ((constraint-fn function)
                                  &key (input "list"))
  :initvals '(nil "list")
  :indoc    '("<lambda patch>"
              "input mode string")
  :menuins  '((1  (("list"     "list")
                   ("n-inputs" "n-inputs")
                   ("car-cdr"  "car-cdr")
                   ("growing"  "growing"))))
  :doc      "Create a metric constraint for SCREAMER-SCORE-2.

CONSTRAINT-FN: lambda patch applied to the metric Screamer variables before
  the rhythm search begins. Receives a list of (top bottom) pairs - one per
  measure - where top and bottom are Screamer variables for the numerator and
  denominator. Example: 4/4 -> (4 4), 3/8 -> (3 8).

  Since top and bottom are Screamer vars (still open at Level 1), the fn can
  use propagation ops to prune the metric search space:
    (assert! (=v (car ts) 4))        -> force numerator to 4
    (assert! (=v (cadr ts) 4))       -> force denominator to 4
    (assert! (memberv (cadr ts) '(4 8))) -> denom must be 4 or 8

INPUT: \"list\" (fn receives full list of pairs at once),
       \"n-inputs\" (fn receives one pair at a time),
       \"car-cdr\"  (first/rest pairs),
       \"growing\"  (growing prefixes).

Returns an SS2-CONSTRAINT object. Pass to SCREAMER-SCORE-2 as CONSTRAINTS."
  :icon     486
  (screamer-score::make-metric-cs constraint-fn :input input))


(defmethod! mk-measures-constraint ((constraint-fn function)
                                    (measures list) (voices list)
                                    (variable-type string)
                                    &key (rests "include") (cs-mode "propagation"))
  :initvals '(nil '((0 1) 2) '(0 1) "pitch" "include" "propagation")
  :indoc    '("<lambda patch>"
              "list of measure specs: number = single measure, list = fused group"
              "list of voice indices parallel to measures"
              "variable-type string (31 combinations of pitch/pc/dur/onset/vel)"
              "rests filter string"
              "constraint mode string")
  :menuins  (list (list 3 *variable-type-menu*)
                  (list 4 (list (list "include" "include")
                                (list "exclude" "exclude")))
                  (list 5 (list (list "propagation" "propagation"))))
  :doc      "Create a cross-measure constraint for SCREAMER-SCORE-2.

Applies CONSTRAINT-FN across multiple (measure-group, voice) pairs.
Each pair produces one extracted-vars argument to the function.

CONSTRAINT-FN: lambda receiving N arguments (one per pair).
  Single-part variable-type: each arg is a flat list of Screamer-vars for
    that pair (e.g. all pitches from m0+v0 fused).
  Multi-part variable-type: each arg is a list of K sublists (one per part
    in declared order: pitch, pc, dur, onset, vel).

MEASURES: list of measure specs, parallel to VOICES.
  Each spec: number n -> events from measure n alone.
             list (n m) -> events from measures n and m fused.
  Example: '(0 1)    with voices '(0 0) -> fn receives (m0-v0) (m1-v0).
  Example: '((0 1) 2) with voices '(0 1) -> fn receives (m0+m1-v0) (m2-v1).

VOICES: list of voice indices parallel to MEASURES.

VARIABLE-TYPE: 31 options selecting which Screamer-vars are extracted from
  each (measure-group, voice) pair before being passed to CONSTRAINT-FN.

RESTS: \"include\" (default) - all slots including rests.
       \"exclude\" - only note slots.

Returns an SS2-CONSTRAINT object. Pass to SCREAMER-SCORE-2 as CONSTRAINTS."
  :icon     486
  (screamer-score::make-measures-cs constraint-fn measures voices variable-type
                                    :rests   rests
                                    :cs-mode cs-mode))


(defmethod! mk-constraint-profile ((bpf t)
                                   (voices t)
                                   (variable-type string)
                                   (approx number)
                                   (range t)
                                   &key (sampling "x-axis")
                                        (rests "exclude"))
  :initvals '(nil nil "pitch" 1 "voice-range" "x-axis" "exclude")
  :indoc    '("BPF / BPF-LIB / SCREAMER-BPF / SCREAMER-BPF-LIB"
              "nil = all voices, integer, or list of integers"
              "pitch / dur / onset / vel"
              "half-window tolerance (number)"
              "voice-range / all / list (lo hi)"
              "x-axis / index / time"
              "rests filter")
  :menuins  '((2  (("pitch" "pitch")
                   ("dur"   "dur")
                   ("onset" "onset")
                   ("vel"   "vel")))
              (4  (("voice-range" "voice-range")
                   ("all"         "all")))
              (5  (("x-axis" "x-axis")
                   ("index"  "index")
                   ("time"   "time")))
              (6  (("exclude" "exclude")
                   ("include" "include"))))
  :doc      "Create a profile constraint for SCREAMER-SCORE-2.

Samples BPF to N points (N = number of attribute variables in each target
voice), maps samples to per-voice (or pooled) attribute domain, and
applies bounds (>=v var (- target approx)) and (<=v var (+ target approx))
to each variable.

BPF           SCREAMER-BPF, SCREAMER-BPF-LIB, OM BPF, or OM BPF-LIB.
              Single BPF is broadcast to all target voices; BPF-LIB must
              have one BPF per target voice.
VOICES        nil (all voices), integer (single voice), or list of
              indices.
VARIABLE-TYPE \"pitch\" / \"dur\" / \"onset\" / \"vel\". Multi-part not
              supported in profile.
APPROX        half-window tolerance around each sampled target value
              (MIDI semitones for pitch, ratio for dur / onset, integer for vel).
RANGE         \"voice-range\" -- each voice's own attribute domain.
              \"all\"         -- deduped union of all target voices' domains.
              (lo hi) list  -- per voice, filter domain to that interval.
SAMPLING      \"x-axis\" (default) -- sample BPF over its declared x-axis.
              \"index\"   -- sample y-points by integer index (classic
                            SCREAMER-SCORE behaviour; ignores x-points
                            spacing).
              \"time\"    -- sample at proportional onset positions; only
                            valid for voices with locked rhythms
                            (MK-SCREAMER-VOICE). Errors on open-rhythm
                            voices.
RESTS         \"exclude\" (default for pitch / dur): bound only fires on
              non-rest slots (impliesv guard).
              \"include\": bound fires unconditionally.

Returns an SS2-CONSTRAINT object. Pass to SCREAMER-SCORE-2 as CONSTRAINTS."
  :icon     486
  (screamer-score::make-cs-profile bpf voices variable-type approx range
                                   :sampling sampling
                                   :rests    rests))


;;;; ===========================================================================
;;;; PART 5 - MAIN BOX: SCREAMER-SCORE-2
;;;; ===========================================================================

(defun decode-to-poly (result)
  "Convert the output of SCREAMER-SCORE-2 into an OM POLY object.

RESULT: output of SCREAMER-SCORE-2 -- plist (:tempo :notes-per-v :signatures), or NIL.

Slot accessors are dispatched via the CLOS generics PITCH, DUR and VEL,
which work on both SCREAMER-NOTE (mono voice) and SCREAMER-CHORD
(polyphonic voice). For a chord-slot, PITCH returns a list of MIDI
integers and VEL returns a single shared velocity; for a note-slot,
PITCH and VEL are scalars. The downstream filter (LIST! + OM*) handles
both shapes uniformly: a scalar pitch becomes a single-note chord, a
list pitch becomes a multi-note chord.

Voice ordering: the engine internally treats voice 0 as the bottom
voice (bass-up convention, natural for harmonic reasoning). OM POLY
uses voice 0 as the top, so the voices list is reversed before
constructing the POLY.

Returns a POLY instance, or NIL if RESULT is NIL."
  (when result
    (let* ((tempo        (getf result :tempo))
           (notes-per-v  (getf result :notes-per-v))
           (signatures   (getf result :signatures))
           (any-vels?    (some (lambda (ns) (some #'vel ns)) notes-per-v))
           (legacy
            (list :tempo   tempo
                  :timesig (mapcar #'screamer-score::screamer-time-signature-ts signatures)
                  :dur     (mapcar (lambda (ns) (mapcar #'dur ns))   notes-per-v)
                  :pitch   (mapcar (lambda (ns) (mapcar #'pitch ns)) notes-per-v)
                  :vel     (when any-vels?
                             (mapcar (lambda (ns) (mapcar #'vel ns)) notes-per-v))))
           (decoded (screamer-score::decode-solution legacy))
           (meters  (getf decoded :meters))
           (voices  (getf decoded :voices)))
      (make-instance 'poly
        :voices (loop for v in (reverse voices)
                      collect
                      (let* ((rhythms  (getf v :rhythms))
                             (raw-p    (getf v :pitches))
                             (raw-vel  (getf v :vels))
                             ;; Keep pitch+vel pairs, filter out rest slots.
                             ;; A pitch entry is either a scalar (mono voice) or
                             ;; a list (chord voice). Rest sentinel is the
                             ;; non-integer pitch -0.5; for chords, all pitches
                             ;; go to -0.5 simultaneously via rest-link, so
                             ;; testing the first entry is enough.
                             (note-pairs
                              (loop for p   in raw-p
                                    for vel in (or raw-vel (make-list (length raw-p)))
                                    for first-p = (if (listp p) (first p) p)
                                    for rest-p? = (and (numberp first-p) (minusp first-p))
                                    unless rest-p?
                                    collect (list p vel)))
                             (chord-objs
                              (loop for (p vel) in note-pairs
                                    collect (let* ((midics (om* (list! p) 100))
                                                   (n      (length midics)))
                                              (make-instance 'chord
                                                :lmidic midics
                                                :lvel   (make-list n :initial-element (or vel 80)))))))
                        (make-instance 'voice
                          :tree   (reduce-rt (mktree rhythms meters))
                          :chords chord-objs
                          :tempo  tempo)))))))

(defmethod! screamer-score-2 ((tempo integer)
                               (metric-dom t)
                               (voice-doms t)
                               (constraints t)
                               &key (random? t)
                                    (count-failures-timed? nil)
                                    (metric-grid nil)
                                    (beat-unit nil)
                                    (no-consecutive-rests t)
                                    (ordering '("reorder" "onset-position" nil "<" "linear-force")))
  :initvals '(120 '((4 4)) nil nil t nil nil nil t ("reorder" "onset-position" nil "<" "linear-force"))
  :indoc    '("tempo in BPM - integer"
              "metric domain - list e.g. '((4 4)) or '((3 4)(4 4)); or nil to auto-derive from locked voices"
              "voice domains - SCREAMER-VOICE-DOMAIN or list (one per voice, from MK-VOICE-DOMAIN)"
              "constraints - SS2-CONSTRAINT or list of SS2-CONSTRAINT (nil = none)"
              "t = random search order, nil = deterministic"
              "t = print failures and elapsed time, nil = silent"
              "t = constrain onsets to beat grid; nil = no grid (auto-t when metric-domain-full)"
              "beat unit override - rational e.g. 1/8; nil = auto from time signature"
              "t = forbid two consecutive rests per voice (default t)"
              "ordering / force function spec - nil = default (reorder onset-position)")
  :menuins  '((4  (("t"   t)
                   ("nil" nil)))
               (5  (("t"   t)
                    ("nil" nil)))
               (6  (("t"   t)
                    ("nil" nil)))
               (8  (("t"   t)
                    ("nil" nil))))
  :doc      "SCREAMER-SCORE-2: flat onset-ordered metric + rhythm + pitch search.

Searches simultaneously for time signatures, rhythms, pitches and velocities
that satisfy all given constraints, returning an OM POLY object.
All variable types are bound in chronological onset order using a single
ONE-VALUE search driven by the ONSET-POSITION cost function.

TEMPO:       BPM (integer). Stored in the result, not used during search.
METRIC-DOM:  Defines which time signatures are allowed.
             List e.g. '((4 4)) = fixed 4/4;  '((3 4)(4 4)) = variable meter.
             nil = auto-derive from locked voices (MK-SCREAMER-VOICE).
               All locked voices must share the same time signature sequence.
               If they agree, the metric is fixed to their sequence (no metric search).
             If metric-dom is provided AND locked voices are present, each locked TS
               value is validated against metric-dom. Incompatible values cause an error
               with a clear message identifying the offending measure and TS.
VOICE-DOMS:  One or a list of voice domain objects (one per voice):
               SCREAMER-VOICE-DOMAIN - search voice (from MK-VOICE-DOMAIN)
               SCREAMER-VOICE        - fixed voice  (from MK-SCREAMER-VOICE)
             Both types can be mixed freely in the same list.
CONSTRAINTS: nil, a single SS2-CONSTRAINT, or a list of SS2-CONSTRAINT objects.
             Created by MK-ONE-VOICE-CONSTRAINT or MK-METRIC-CONSTRAINT.

KEY ARGUMENTS:
  :RANDOM?      - t (default) = a-random-member-ofv; nil = a-member-ofv.
  :METRIC-GRID           - t = constrain onsets to beat positions (per ts denominator).
                           nil (default) = no grid constraint.
                           Auto-activated by the engine when METRIC-DOM is from METRIC-DOMAIN-FULL.
  :BEAT-UNIT             - rational override for the grid beat unit (e.g. 1/8).
                           nil (default) = auto-derive from time signature (1/denom).
  :NO-CONSECUTIVE-RESTS  - t (default) = forbid two consecutive rests per voice.
                           nil = allow consecutive rests.
  :ORDERING              - force-function spec for the search.
                           nil = default (reorder onset-position).

RETURNS: OM POLY object, or NIL if no solution exists."
  :icon     486
  (let ((result (screamer-score::solve-score
                  tempo metric-dom voice-doms constraints
                  :random?               random?
                  :count-failures-timed? count-failures-timed?
                  :metric-grid           metric-grid
                  :beat-unit             beat-unit
                  :no-consecutive-rests  no-consecutive-rests
                  :ordering              ordering)))
    (if (and result (not (screamer::contains-variables? result)))
        (decode-to-poly result)
        (progn (om-message-dialog "SCREAMER-SCORE-2: Unable to find a solution!")
               (om-abort)))))


;;;; ===========================================================================
;;;; PART 6 - SCREAMER-VOICE ACCESSOR BRIDGES
;;;;
;;;; screamer-score-2 calls svd-rhythm-spec / svd-pitch-spec / svd-vel-spec on
;;;; every voice-dom object.  These methods bridge both screamer-voice (sv-* accessors)
;;;; and screamer-voice-domain (svd-* accessors) -- both now in :screamer-score -- to
;;;; the om::svd-* generic functions called by screamer-score-2.
;;;; ===========================================================================

(defmethod svd-rhythm-spec ((self screamer-score::screamer-voice))
  (screamer-score::sv-rhythm-spec self))

(defmethod svd-pitch-spec ((self screamer-score::screamer-voice))
  (screamer-score::sv-pitch-spec self))

(defmethod svd-vel-spec ((self screamer-score::screamer-voice))
  (screamer-score::sv-vel-spec self))

(defmethod svd-metric-spec ((self screamer-score::screamer-voice))
  (screamer-score::sv-metric-spec self))

(defmethod svd-rhythm-spec ((self screamer-score::screamer-voice-domain))
  (screamer-score::svd-rhythm-spec self))

(defmethod svd-pitch-spec ((self screamer-score::screamer-voice-domain))
  (screamer-score::svd-pitch-spec self))

(defmethod svd-vel-spec ((self screamer-score::screamer-voice-domain))
  (screamer-score::svd-vel-spec self))

;; screamer-voice-domain has no metric spec (metric-dom is a separate argument).
(defmethod svd-metric-spec ((self screamer-score::screamer-voice-domain)) nil)


;;;; ===========================================================================
;;;; PART 7 - MK-SCREAMER-VOICE (locked voice from OM VOICE object)
;;;; ===========================================================================

(defun %lmidic->midi (midics)
  "Convert a list of midicents to MIDI note numbers (integers 0-127).
Single element -> atom.  Multiple elements -> list."
  (let ((notes (mapcar #'(lambda (m) (round (/ m 100))) midics)))
    (if (= (length notes) 1)
        (car notes)
        notes)))

(defun open-pitch? (domain)
"Returns T if the OM voice object contains only rests OR all midics are 6000 (middle C)."
 (let ((no-duplicates (remove-duplicates (remove-if #'(lambda (x) (or (null x) (minusp x))) (flat domain)))))
  (and (or (null (first no-duplicates)) (= 60 (first no-duplicates)))
       (= 1 (length no-duplicates)))))

(defmethod! mk-screamer-voice ((durs list) (pitches list)
                               &key (vel-dom nil) (om-voice nil))
  :initvals '('(1/4 -1/8 1/4 1/4) '(60 62 64) nil nil)
  :indoc    '("concrete rhythm sequence - one rational per slot (positive=note, negative=rest)"
              "concrete pitch list - one value per NOTE (no nils): MIDI integer or chord list"
              "concrete velocity list - one integer per NOTE (0-127); or nil to omit"
              "OM VOICE object - when provided, overrides durs/pitches/vel-dom entirely")
  :doc      "Create a locked SCREAMER-VOICE from explicit lists or an OM VOICE object.

PRIMARY MODE (durs and pitches provided as lists):

DURS:    Concrete duration list - one rational per slot.
         Positive = note  (e.g. 1/4),  negative = rest  (e.g. -1/8).
PITCHES: One value per NOTE slot only - do NOT include -1 for rests.
         Rests are inferred automatically from negative values in DURS.
         atom  (e.g. 60)         - single note (MIDI number 0-127)
         list  (e.g. (60 64 67)) - chord (list of MIDI numbers)
         Example: durs=(1/4 1/4 -1/4 1/4) pitches=(60 64 67)
                  -> internal: (60 64 -1 67)
VEL-DOM: One velocity per NOTE slot only (0-127) - no -1 for rests.
         Rests are filled in automatically (-1), same as PITCHES.
         Pass nil to omit velocity entirely.

OM VOICE CONVERSION MODE (pass :om-voice):

:OM-VOICE    When an OM VOICE object is supplied, DURS, PITCHES and VEL-DOM are ignored.
             Rhythm, pitch, velocity, and time signatures are extracted from the
             VOICE object automatically.

             However, if all MIDICS extracted from the VOICE object are equal 6000 OR NIL
            (rests), only durations and time-signatures are extracted, using PITCHES and
             VEL-DOM as the domain of SCREAMER variables for pitch and velocity.

:DEFAULT-VEL Velocity for notes with no explicit velocity (default 80).
             Only relevant when :OM-VOICE is used.

Time signatures (metric-spec) are set only in OM conversion mode.
In primary mode metric-spec is nil - METRIC-DOM in SCREAMER-SCORE-2 controls the metric.

Pitch convention (consistent with free-search voices):
  Single note -> atom  (e.g. 60)
  Chord       -> list  (e.g. (60 64 67))
  Rest slot   -> -1    (inserted automatically)

Returns a SCREAMER-VOICE object. Pass to SCREAMER-SCORE-2 as VOICE-DOMS
alongside SCREAMER-VOICE-DOMAIN objects (from MK-VOICE-DOMAIN) freely."
  :icon     486
  (if (and om-voice (typep om-voice 'om::voice))
      ;; OM-VOICE conversion mode (OM-specific: extracts from OM VOICE object,
      ;; supports open-pitch fallback for default-pitch voices).
      (let* ((ext-durs    (flat (tree2ratio (tree om-voice))))
             (n-slots     (length ext-durs))
             (all-chords  (remove-if #'cont-chord-p (get-all-chords om-voice)))
             (ext-pitches (mapcar #'(lambda (c)
                                      (if (rest-p c)
                                          -1
                                          (%lmidic->midi (lmidic c))))
                                  all-chords))
             (ext-vels    (mapcar #'(lambda (c)
                                      (if (rest-p c)
                                          -1
                                          (let ((vs (lvel c)))
                                            (if vs (car vs) 80))))
                                  all-chords))
             (ts-seq      (get-time-sig om-voice))
             (open-pitch? (open-pitch? ext-pitches)))
        (make-instance 'screamer-score::screamer-voice
          :rhythm-spec (list :n-notes n-slots :domain ext-durs     :locked t)
          :pitch-spec  (if open-pitch?
                          (list :domain pitches :locked nil)
                          (list :domain ext-pitches                    :locked t))
          :vel-spec    (if open-pitch?
                          (list :domain vel-dom :locked nil)
                          (list :domain ext-vels                       :locked t))
          :metric-spec (list :domain ts-seq                         :locked t)))
      ;; Primary mode -- delegates to make-screamer-voice after OM-specific
      ;; pad/truncate of pitches/vels to match note count.
      (let* ((n-notes     (count-if #'plusp durs))
             (adj-pitches (let ((diff (- n-notes (length pitches))))
                            (cond ((zerop diff) pitches)
                                  ((plusp diff) (append pitches (make-list diff :initial-element 60)))
                                  (t            (first-n n-notes pitches)))))
             (adj-vels    (when vel-dom
                            (let ((diff (- n-notes (length vel-dom))))
                              (cond ((zerop diff) vel-dom)
                                    ((plusp diff) (append vel-dom (make-list diff :initial-element 80)))
                                    (t            (first-n n-notes vel-dom)))))))
        (screamer-score::make-screamer-voice durs adj-pitches :vel-dom adj-vels))))
