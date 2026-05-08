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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :screamer-score)

;;;; ===========================================================================
;;;; INPUT NORMALIZATION
;;;; ===========================================================================

(defun %ensure-list (x)
  "Wrap X in a list unless it already is one."
  (cond ((null x) nil)
        ((listp x) x)
        (t (list x))))

(defun %flatten1 (xs)
  "Flatten one level."
  (let ((acc '()))
    (dolist (x xs (nreverse acc))
      (cond ((null x))
            ((listp x) (dolist (y x) (push y acc)))
            (t (push x acc))))))


;;;; ===========================================================================
;;;; DOMAIN CONSTRUCTORS
;;;; ===========================================================================

(defun make-metric-domain (&rest specs)
  "Build a METRIC-DOMAIN-SPEC from (ts beat-spec) pairs."
  (metric-domain-full (group-list specs 2 :linear)))

(defun make-rhythm-domain (n-notes domain)
  "Build a rhythm SS2-DOMAIN of N-NOTES slots over DOMAIN."
  (make-instance 'ss2-domain :spec (rhythm-domain n-notes domain)))

(defun make-pitch-domain (domain)
  "Build a pitch SS2-DOMAIN."
  (make-instance 'ss2-domain :spec (pitch-domain domain)))

(defun make-vel-domain (domain)
  "Build a velocity SS2-DOMAIN."
  (make-instance 'ss2-domain :spec (velocity-domain domain)))

(defun make-voice-domain (n-notes rhythm-dom pitch-dom &optional vel-dom)
  "Bundle rhythm, pitch and (optional) velocity domains for one search voice.
PITCH-DOM may be NIL (no pitch search), a flat list of MIDI integers
(monophonic voice), or a CHORD-DOMAIN instance (polyphonic voice). The
resulting pitch-spec carries either the flat list or the chord-domain
under :domain; the engine type-dispatches on the value to choose between
make-voice-notes and make-voice-chords."
  (let ((rhythm-spec (rhythm-domain n-notes rhythm-dom))
        (pitch-spec  (cond
                       ((null pitch-dom) nil)
                       ((typep pitch-dom 'chord-domain)
                        (list :domain pitch-dom))
                       ((listp pitch-dom)
                        (pitch-domain pitch-dom))
                       (t (error
                            "PITCH-DOM must be NIL, a list of MIDI integers, or a CHORD-DOMAIN instance; got ~S"
                            pitch-dom))))
        (vel-spec    (and vel-dom (velocity-domain vel-dom))))
    (make-instance 'screamer-voice-domain
      :rhythm-spec rhythm-spec
      :pitch-spec  pitch-spec
      :vel-spec    vel-spec)))


;;;; ===========================================================================
;;;; LOCKED VOICE
;;;; ===========================================================================

(defun %place-at-rests (durs values)
  "Distribute VALUES across DURS, inserting -1 at rest positions."
  (let ((remaining (copy-list values)))
    (mapcar #'(lambda (d)
                (if (minusp d) -1 (pop remaining)))
            durs)))

(defun make-screamer-voice (durs pitches &key vel-dom metric-spec)
  "Build a fully locked SCREAMER-VOICE from concrete sequences."
  (let* ((n-notes  (length durs))
         (pitches* (%place-at-rests durs pitches))
         (vels*    (when vel-dom (%place-at-rests durs vel-dom))))
    (make-instance 'screamer-voice
      :rhythm-spec (list :n-notes n-notes :domain durs :locked t)
      :pitch-spec  (list :domain pitches* :locked t)
      :vel-spec    (when vels* (list :domain vels* :locked t))
      :metric-spec (when metric-spec
                     (list :domain metric-spec :locked t)))))


(defun make-locked-rhythm-voice (durs pitch-dom &key vel-dom metric-spec)
  "Build a SCREAMER-VOICE with locked rhythm + open pitch (and optionally
open velocity). REPL/SBCL equivalent of the MK-SCREAMER-VOICE :om-voice path
when the supplied OM voice has all-60 (open) pitches: ritmo concreto, pitch
busca em PITCH-DOM.

Suitable as target for MK-CONSTRAINT-PROFILE with :sampling \"time\" --
the rhythms are bound at constraint-application time so the engine can
sample the BPF at the actual proportional onset of each note.

DURS       Concrete rhythm sequence (positive=note, negative=rest).
PITCH-DOM  Open pitch search domain (list of MIDI integers).
VEL-DOM    Open velocity domain, or nil to omit.

Returns a SCREAMER-VOICE."
  (let ((n-notes (length durs)))
    (make-instance 'screamer-voice
      :rhythm-spec (list :n-notes n-notes :domain durs :locked t)
      :pitch-spec  (list :domain pitch-dom :locked nil)
      :vel-spec    (when vel-dom (list :domain vel-dom :locked nil))
      :metric-spec (when metric-spec
                     (list :domain metric-spec :locked t)))))


;;;; ===========================================================================
;;;; CONSTRAINT CONSTRUCTORS
;;;; ===========================================================================

(defun make-one-voice-cs (constraint-fn voices variable-type input
                          &key (rests "include") (index nil)
                               (measure nil) (cs-mode "propagation"))
  "Build a one-voice SS2-CONSTRAINT.
VARIABLE-TYPE: mandatory string from *VARIABLE-TYPE-OPTIONS* declaring which
Screamer-vars the engine extracts from the per-voice screamer-notes before
calling CONSTRAINT-FN. See MAKE-SCREAMER-SCORE-CONSTRAINT for the calling
conventions per variable-type / input-mode combination."
  (make-instance 'ss2-constraint
    :spec (make-screamer-score-constraint
            :constraint    constraint-fn
            :type          "one-voice"
            :voices        voices
            :variable-type variable-type
            :input         input
            :rests         rests
            :index         index
            :measure       measure
            :cs-mode       cs-mode)))

(defun make-metric-cs (constraint-fn &key (input "list"))
  "Build a metric SS2-CONSTRAINT applied to time-signature variables."
  (make-instance 'ss2-constraint
    :spec (make-screamer-score-constraint
            :constraint constraint-fn
            :type       "metric"
            :input      input)))

(defun make-measures-cs (constraint-fn measures voices variable-type
                         &key (rests "include") (cs-mode "propagation"))
  "Build a cross-measure SS2-CONSTRAINT.
VARIABLE-TYPE: mandatory string from *VARIABLE-TYPE-OPTIONS* declaring which
Screamer-vars are extracted per (measure-spec voice) pair before calling
CONSTRAINT-FN. Each pair contributes one fn argument."
  (make-instance 'ss2-constraint
    :spec (make-screamer-score-constraint
            :constraint    constraint-fn
            :type          "measures"
            :measures      measures
            :voices        voices
            :variable-type variable-type
            :rests         rests
            :cs-mode       cs-mode)))


(defun make-cs-profile (bpf voices variable-type approx range
                        &key (sampling "x-axis")
                             (rests "exclude"))
  "Build a profile SS2-CONSTRAINT. The BPF curve is sampled to N points
where N is the number of attribute variables in each target voice; samples
are linearly scaled to the per-voice (or pooled) attribute domain, and
each variable receives bounds (>=v var (- target approx)) and
(<=v var (+ target approx)).

Profile constraints are propagation-only: the user provides a BPF rather
than a lambda, so no constraint function is compiled and there is no
backtrack option. Bounds are asserted upfront via (>=v ...) / (<=v ...).

BPF           SCREAMER-BPF, SCREAMER-BPF-LIB, OM BPF, or OM BPF-LIB.
              Single BPF is broadcast to all target voices; BPF-LIB must
              have one BPF per target voice.
VOICES        nil (all voices), integer (single voice), or list of indices.
VARIABLE-TYPE one of \"pitch\" / \"dur\" / \"onset\" / \"vel\". Multi-part not
              supported.
APPROX        half-window tolerance around each sampled target value
              (same unit as the attribute: MIDI semitones for pitch
              (integer 0-127 domain), ratio for dur / onset, integer
              for vel).
RANGE         \"voice-range\" (each voice's own domain),
              \"all\" (deduped union of all target voices' domains), or
              a 2-element list (lo hi) (filter per-voice domain to that
              interval).
SAMPLING      \"x-axis\" (default) -- sample BPF over its declared x-axis.
              \"index\"  -- sample y-points by integer index (classic
                            SCREAMER-SCORE behaviour; ignores x-points
                            spacing).
              \"time\"   -- sample at proportional onset positions; only
                            valid when the target voice has locked
                            rhythms (mk-screamer-voice). Errors on voices
                            with open rhythm domains.
RESTS         \"exclude\" (default for pitch / dur): bound only fires on
              non-rest slots (impliesv guard on rhythm >= 0).
              \"include\": bound fires unconditionally."
  (make-instance 'ss2-constraint
    :spec (make-screamer-score-constraint
            :type          "profile"
            :bpf           bpf
            :voices        voices
            :variable-type variable-type
            :approx        approx
            :range         range
            :sampling      sampling
            :rests         rests)))


;;;; ===========================================================================
;;;; INPUT NORMALIZATION FOR SOLVER
;;;; ===========================================================================

(defun %normalize-voice-list (voice-doms)
  "Return a flat list of voice objects from a single one or a (nested) list."
  (cond
    ((null voice-doms) nil)
    ((or (typep voice-doms 'screamer-voice-domain)
         (typep voice-doms 'screamer-voice))
     (list voice-doms))
    ((listp voice-doms)
     (remove nil (%flatten1 voice-doms)))
    (t (error "VOICE-DOMS: expected screamer-voice-domain, screamer-voice, or list -- got ~S" voice-doms))))

(defun %normalize-cs-list (constraints)
  "Return a list of constraint plists from a single SS2-CONSTRAINT, plist, or list."
  (labels ((->plist (c)
             (cond ((ss2-constraint-p c)                     (ss2-constraint-spec c))
                   ((and (listp c) (eq (first c) :constraint)) c)
                   (t nil))))
    (cond
      ((null constraints) nil)
      ((ss2-constraint-p constraints) (list (ss2-constraint-spec constraints)))
      ((and (listp constraints) (eq (first constraints) :constraint))
       (list constraints))
      ((listp constraints) (remove nil (mapcar #'->plist constraints)))
      (t nil))))

(defun %resolve-metric-spec-cl (metric-dom voice-list)
  "Derive or validate the metric spec from METRIC-DOM and locked voices."
  (let* ((metric-specs (mapcar #'sv-metric-spec
                               (remove-if-not #'(lambda (v) (typep v 'screamer-voice))
                                              voice-list)))
         (locked-seqs  (remove nil metric-specs))
         (ts-seqs      (mapcar #'(lambda (ms) (getf ms :domain)) locked-seqs)))
    (cond
      ((and (null ts-seqs) (null metric-dom))
       (error "SOLVE-SCORE: METRIC-DOM is nil and no locked voice provides time signatures."))
      ((null ts-seqs)
       (list :domain metric-dom))
      ((null metric-dom)
       (let ((ref (first ts-seqs)))
         (dolist (seq (rest ts-seqs))
           (unless (equal seq ref)
             (error "SOLVE-SCORE: polymetry between locked voices.~%Voice 1: ~S~%Other:   ~S"
                    ref seq)))
         (list :domain (remove-duplicates ref :test #'equal)
               :locked-seq ref)))
      (t
       (let ((ref (first ts-seqs)))
         (dolist (seq (rest ts-seqs))
           (unless (equal seq ref)
             (error "SOLVE-SCORE: polymetry between locked voices.~%Voice 1: ~S~%Other:   ~S"
                    ref seq)))
         (loop for ts in ref
               for i from 0
               unless (member ts metric-dom :test #'equal)
                 do (error "SOLVE-SCORE: locked TS ~S at measure ~D not in METRIC-DOM ~S"
                           ts i metric-dom))
         (list :domain metric-dom :locked-seq ref))))))


;;;; ===========================================================================
;;;; SOLVER
;;;; ===========================================================================

(defun solve-score (tempo metric-dom voice-doms constraints
                    &key (random? t)
                         (count-failures-timed? nil)
                         (metric-grid nil)
                         (beat-unit nil)
                         (no-consecutive-rests t)
                         (ordering '("reorder" "onset-position" nil "<" "linear-force")))
  "Solve the score and return a plist (:tempo :notes-per-v :signatures), or NIL."
  (let* ((voice-list (%normalize-voice-list voice-doms)))
    (unless voice-list
      (error "SOLVE-SCORE: no voice domains provided."))
    (let* ((raw-metric-dom  (if (typep metric-dom 'metric-domain-spec)
                                (mds-ts-domain metric-dom)
                                metric-dom))
           (metric-beat-map (when (typep metric-dom 'metric-domain-spec)
                              (mds-beat-map metric-dom)))
           (metric-spec     (%resolve-metric-spec-cl raw-metric-dom voice-list))
           (metric-spec     (if metric-beat-map
                                (append metric-spec (list :beat-map metric-beat-map))
                                metric-spec))
           (rhythm-specs    (mapcar #'(lambda (v)
                                        (etypecase v
                                          (screamer-voice         (sv-rhythm-spec v))
                                          (screamer-voice-domain  (svd-rhythm-spec v))))
                                    voice-list))
           (pitch-specs     (mapcar #'(lambda (v)
                                        (etypecase v
                                          (screamer-voice         (sv-pitch-spec v))
                                          (screamer-voice-domain  (svd-pitch-spec v))))
                                    voice-list))
           (vel-specs       (let ((ds (remove nil
                                              (mapcar #'(lambda (v)
                                                          (etypecase v
                                                            (screamer-voice         (sv-vel-spec v))
                                                            (screamer-voice-domain  (svd-vel-spec v))))
                                                      voice-list))))
                              (when ds ds)))
           (cs-plists       (%normalize-cs-list constraints)))
      (screamer-score-2-engine
        tempo metric-spec rhythm-specs pitch-specs
        :velocity-domain-specs    vel-specs
        :random?                  random?
        :ordering-force-functions ordering
        :constraints              cs-plists
        :count-failures-timed?    count-failures-timed?
        :metric-grid              metric-grid
        :beat-unit                beat-unit
        :no-consecutive-rests     no-consecutive-rests))))


;;;; ===========================================================================
;;;; REPL PRETTY-PRINTER
;;;; ===========================================================================

(defun pp-score-result (result &optional (stream *standard-output*))
  "Print RESULT (a SOLVE-SCORE plist) to STREAM."
  (cond
    ((null result)
     (format stream "~&No solution.~%"))
    (t
     (let ((tempo       (getf result :tempo))
           (notes-per-v (getf result :notes-per-v))
           (signatures  (getf result :signatures)))
       (format stream "~&Tempo:  ~A~%" tempo)
       (format stream "Meters: ~S~%"
               (mapcar #'screamer-time-signature-ts signatures))
       (loop for ns in notes-per-v
             for i from 0
             do (format stream "Voice ~D:~%" i)
                (dolist (n ns)
                  (format stream "  ~S~%" n))))))
  result)
