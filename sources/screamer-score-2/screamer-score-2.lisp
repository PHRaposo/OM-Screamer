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

(defun function-lambda-list (fun)
 #+sbcl (sb-introspect::function-lambda-list fun)
 #+lispworks (lw::function-lambda-list fun))
 
(defun function-name (fun)
 #+sbcl(let ((info (sb-introspect::function-debug-info fun)))
        (sb-c::compiled-debug-info-name info))
 #+lispworks (system::function-name fun))

;;;; ===========================================================================
;;;; STANDALONE CONSTRAINT HELPERS
;;;; ===========================================================================

;;; ---- List-splitting helpers ------------------------------------------------

(defun mk-growing-2 (vars)
  "Growing prefixes of VARS: ((v0) (v0 v1) ... (v0 ... vN-1)).
Equivalent to MK-GROWING without OM dependencies (posn-match, arithm-ser)."
  (loop for i from 1 to (length vars)
        collect (subseq vars 0 i)))

(defun mk-car-cdr-2 (vars)
  "CAR-CDR pairs in reverse: ((vN-2 (vN-1)) ... (v0 (v1 ... vN-1))).
Equivalent to MK-CAR-CDR without OM dependencies."
  (let ((n (length vars)))
    (loop for i from (- n 2) downto 0
          collect (list (nth i vars) (subseq vars (1+ i))))))

(defun split-domain-list1-2 (list-length n-inputs voice-domain)
  "Sliding windows of N-INPUTS elements: ((v0..vK-1) (v1..vK) ...).
Equivalent to SPLIT-DOMAIN-LIST1 without OM dependencies (posn-match, arithm-ser)."
  (loop for i from 0 to (- list-length n-inputs)
        collect (subseq voice-domain i (+ i n-inputs))))

(defun assert!-deep-mapcar-2 (fun fun1 list? &rest args)
  "Recursively apply FUN to each atom in LIST?. FUN is expected to be a
compiled CS that internally asserts via TRANSFORM-ASSERT! dispatch.
Equivalent to ASSERT!-DEEP-MAPCAR without the OM-Screamer package dependency."
  (cond
    ((null list?) nil)
    ((not (consp list?))
     (apply fun1 list? args))
    (t
     (apply #'assert!-deep-mapcar-2 fun fun1 (car list?) args)
     (apply #'assert!-deep-mapcar-2 fun fun1 (cdr list?) args))))


;;; ---- OM patch detection ---------------------------------------------------

;;; BUG: LispWorks returns "Non-lisp-function-code" for all lambda functions
;;;     Must find another way to detect OM patches.


;(defun om-lambda-patch-p (fun)
;  "T if FUN is an OM visual patch (compiled anonymous wrapper created via EVAL).
;Detected by FUNCTION-NAME returning the string \"Non-lisp-function code\",
;which is LispWorks' marker for compiled functions without a registered name."
;  (string= (format nil "~A" (function-name fun)) "Non-lisp-function code"))

;;; ---- Standalone constraint compilation ------------------------------------

(defun compile-screamer-constraint-2 (fun)
  "Compile FUN for use as a Screamer constraint.
Always recompiles by extracting FUN's lambda body and wrapping it in
SCREAMER::ASSERT!, so TRANSFORM-ASSERT! dispatches to specialized
assert!-=v / assert!-notv-memberv / assert!-andv / etc. at compile time
instead of materializing a boolean variable at runtime.

FUN must be a lambda whose LAMBDA-EXPRESSION is accessible.
Optional docstring (third element) captured as the constraint name."
  (let* ((expr (function-lambda-expression fun)))
    (unless (and (consp expr) (eq (first expr) 'lambda))
      (error "COMPILE-SCREAMER-CONSTRAINT-2: cannot recover lambda expression from ~S." fun))
    (let* ((lambda-list (second expr))
           (body-all    (cddr expr))
           (doc         (when (and (stringp (first body-all)) (rest body-all))
                          (first body-all)))
           (body-forms  (if doc (rest body-all) body-all))
           (leading     (butlast body-forms))
           (last-form   (car (last body-forms)))
           (name-str    (or doc "anon-constraint"))
           (defun-form  `(defun ,(gensym (concatenate 'string name-str "-"))
                             ,lambda-list
                           ,@(when doc (list doc))
                           ,@leading
                           ,(om::push-assert!-into last-form))))
      (format om::*om-stream*
              "~%[compile-screamer-constraint-2] defun-form:~%~S~%"
              defun-form)
      (let ((sym (compile (eval defun-form))))
        (fdefinition sym)))))

(defun compile-screamer-backtrack-constraint-2 (fun)
  "Compile FUN as a backtrack-mode constraint. The returned function takes
the same lambda-list as FUN; when invoked with Screamer vars, it defers
calling FUN until all vars are bound (via OM-SCREAMER:ANY-FN), then asserts
the boolean result against the search.

FUN must be a lambda whose LAMBDA-EXPRESSION is accessible. FUN's body
receives CONCRETE VALUES at fire time (not Screamer vars), so it can be
any deterministic Lisp function -- arithmetic, predicates, OM functions,
etc. -- without requiring Screamer-aware ops.

The wrapper has shape:

    (defun gensym-name (a1 a2 ... aN)
      (s::assert! (om?::any-fn fun a1 a2 ... aN)))

where ANY-FN attaches a noticer to the args, fires when DEEP-BOUND, and
returns a boolean variable EQV to (apply fun (concrete-values args))."
  (let* ((expr (function-lambda-expression fun)))
    (unless (and (consp expr) (eq (first expr) 'lambda))
      (error "COMPILE-SCREAMER-BACKTRACK-CONSTRAINT-2: cannot recover lambda expression from ~S." fun))
    (let* ((orig-lambda-list (function-lambda-list fun))
           (body-all         (cddr expr))
           (doc              (when (and (stringp (first body-all)) (rest body-all))
                               (first body-all)))
           (name-str         (or doc "anon-backtrack-constraint"))
           ;; Use fresh symbols for the wrapper's lambda-list to avoid
           ;; name collisions with package-bound symbols (e.g. CL:PI).
           (wrapper-args     (loop for i from 0 below (length orig-lambda-list)
                                   collect (gensym (format nil "BT-ARG-~A-" i))))
           (defun-form
            `(cl:defun ,(gensym (concatenate 'string name-str "-bt-"))
                 ,wrapper-args
               ,@(when doc (list doc))
               (s::assert! (om?::any-fn ,fun ,@wrapper-args)))))
      (format om::*om-stream*
              "~%[compile-screamer-backtrack-constraint-2] defun-form:~%~S~%"
              defun-form)
      (let ((sym (compile (eval defun-form))))
        (fdefinition sym)))))

;;; ---- Standalone apply-contv -----------------------------------------------

(defun apply-contv-2 (cs mode recursive? vars)
  "Apply constraint CS to VARS - standalone version without OM visual dependencies.

CS:         A compiled function (from COMPILE-SCREAMER-CONSTRAINT-2) or a lambda.
            For lambdas, include a docstring as the third element for error names:
              #'(lambda (x) \"my-constraint\" body...)

MODE:       \"list\" (default) - CS receives VARS as a structured argument.
            \"atom\"           - CS is applied to each atom in VARS recursively.

RECURSIVE?: Partitioning strategy in \"list\" mode:
            \"off\"      - CS receives the whole list at once (default for \"list\").
            \"n-inputs\" - CS receives sliding windows; window size = arity of CS.
            \"car-cdr\"  - CS receives (first rest) pairs in reverse order.
            \"growing\"  - CS receives growing prefixes."
  (let ((fn-name
         (cond
           ;((om-lambda-patch-p cs)  "om-patch-constraint")
           ((ignore-errors
              (let ((expr (function-lambda-expression cs)))
                (when (stringp (third expr)) (third expr)))))
           ((ignore-errors (symbol-name (function-name cs))))
           (t "unknown-constraint"))))
    (handler-bind
        ((error #'(lambda (c)
                    (error "Error in constraint ~S: ~A" fn-name c))))
      (cond
        ((equal mode "atom")
         (assert!-deep-mapcar-2 cs cs vars))
        ((equal mode "list")
         (cond
           ((equal recursive? "n-inputs")
            (mapc #'(lambda (x) (apply cs x))
                  (split-domain-list1-2 (length vars)
                                        (length (function-lambda-list cs))
                                        vars)))
           ((equal recursive? "car-cdr")
            (mapc #'(lambda (x) (funcall cs (first x) (second x)))
                  (mk-car-cdr-2 vars)))
           ((equal recursive? "growing")
            (mapc #'(lambda (x) (funcall cs x))
                  (mk-growing-2 vars)))
           (t
            (funcall cs vars))))
        (t
         (error "APPLY-CONTV-2: unknown mode ~S. Use \"atom\" or \"list\"."
                mode))))))

;;;; ===========================================================================
;;;; RHYTHM VARIABLE CREATION
;;;; ===========================================================================

(defun make-rhythm-variablesv (n domain &optional (random? nil) (voice-index 0))
  "Create N Screamer rhythm variables, each drawn from DOMAIN.
DOMAIN: list of atomic durations (positive=note, negative=rest, 0=grace).
RANDOM?: nil -> a-member-ofv, t -> a-random-member-ofv.
VOICE-INDEX: used to build variable names d-vi-k for debug readability.

When DOMAIN is a singleton, use VARIABLIZE instead of A-MEMBER-OFV.
A-MEMBER-OFV calls (value-of v) at the end and returns the concrete value
for singleton domains, breaking noticer registration and force functions.
VARIABLIZE creates a pre-bound Screamer variable that participates in the
constraint network without adding search alternatives - same pattern used
for locked voices."
  (if (null (rest domain))
      (let ((val (first domain)))
        (loop for k from 0 below n collect
              (let ((var (s::make-variable (format nil "d-~A-~A" voice-index k))))
                (s::assert! (s::equalv var val))
                var)))
      (loop for k from 0 below n collect
            (funcall (if random? #'s::a-random-member-ofv #'s::a-member-ofv)
                     domain
                     (format nil "d-~A-~A" voice-index k)))))


;;;; ===========================================================================
;;;; ONSET VARIABLE DERIVATION
;;;; ===========================================================================

(defun rhythm-vars->onset-vars (rhythm-vars &optional (start 0))
  "Derive onset constraint variables from rhythm duration variables.
START = time of the first slot onset (default 0).
Returns N+1 onset-vars: N slot onsets + 1 measure-end position."
  (let ((abs-vars (mapcar #'(lambda (r)(s::funcallv #'abs r)) rhythm-vars)))
    (butlast 
     (om?::dx->xv start abs-vars))))




;;;; ===========================================================================
;;;; BUILT-IN RHYTHM CONSTRAINTS (internal)
;;;; ===========================================================================

(defun compute-n-measures (rhythm-domain-specs metric-domain-spec)
  "Compute the maximum number of measures that could ever be needed.
= ceiling( max-per-voice(n-notes * max-abs-dur)  /  min-ts-measure-duration )
Computed per voice then maximised across voices -- avoids cross-voice overestimate
when voices have very different note counts or duration domains."
  (let* ((max-voice-dur
          (reduce #'max
                  (mapcar #'(lambda (rd)
                              (* (getf rd :n-notes)
                                 (reduce #'max (mapcar #'abs (getf rd :domain)))))
                          rhythm-domain-specs)))
         (ts-domain  (getf metric-domain-spec :domain))
         (min-ts-dur (reduce #'min (mapcar #'ts-measure-duration ts-domain))))
    (ceiling max-voice-dur min-ts-dur)))




;;;; ===========================================================================
;;;; ONSET-ORDERED PITCH VARIABLE REGISTRATION
;;;; ===========================================================================

(defun make-absent-pvar ()
  (let ((v (s::make-variable)))
    (s::assert! (s::equalv v -1))
    v))




;;;; ===========================================================================
;;;; POST-SEARCH UTILITIES
;;;; ===========================================================================

(defun rhythm-solution->onsets (rhythm-solution &optional (start 0))
  "Convert a rhythm solution (list of rationals) to onset positions."
  (let ((onsets (list start)))
    (dolist (dur (butlast rhythm-solution))
      (push (+ (first onsets) (abs dur)) onsets))
    (nreverse onsets)))


;;;; ===========================================================================
;;;; PART 1 -- TIME SIGNATURE UTILITIES
;;;; ===========================================================================

(defun ts-beat-unit (time-sig)
  "Return the beat unit of TIME-SIG: (num denom) -> 1/denom.
  (ts-beat-unit '(3 8)) -> 1/8
  (ts-beat-unit '(4 4)) -> 1/4"
  (/ 1 (second time-sig)))

(defun ts-measure-duration (time-sig)
  "Return the total duration of TIME-SIG: (num denom) -> num/denom.
  (ts-measure-duration '(3 8)) -> 3/8
  (ts-measure-duration '(4 4)) -> 1"
  (/ (first time-sig) (second time-sig)))

(defun beat-spans (ts beat-spec)
  "Return list of (beat-start . beat-duration) pairs covering one measure of TS.
BEAT-SPEC: nil -> auto (1/denom per TS), rational -> uniform override, list -> non-uniform.
Non-uniform example: (2/8 3/8) for (5 8) grouped 2+3."
  (let ((m-dur (ts-measure-duration ts)))
    (if (consp beat-spec)
        (mapcar #'cons (butlast (dx->x 0 beat-spec)) beat-spec)
        (let ((bu (or beat-spec (ts-beat-unit ts))))
          (loop for b = 0 then (+ b bu) while (< b m-dur)
                collect (cons b bu))))))

(defun resolve-beat-spec (ts beat-map)
  "Look up beat-spec for TS in BEAT-MAP alist. Returns nil (auto) when not found."
  (when beat-map (cdr (assoc ts beat-map :test #'equal))))


;;;; ===========================================================================
;;;; PART 2 -- METRIC VARIABLE BUNDLES
;;;; ===========================================================================

(defun make-metric-var-bundle (metric-domain random? &optional (measure-index 0))
  "Create one metric Screamer variable plus derived propagation variables.
Returns a plist (:ts ts :top top :bottom bot :dur dur).

ts     -- binds to a (num denom) list from METRIC-DOMAIN
top    -- (firstv ts)   -> numerator   (bidirectional propagation)
bottom -- (secondv ts)  -> denominator (bidirectional propagation)
dur    -- (/v top bot)  -> measure duration as a rational Screamer variable"
  (let* ((ts-var  (if (null (rest metric-domain))
                      (let ((var (s::make-variable (format nil "t-~A" measure-index))))
                        (s::assert! (s::equalv var (first metric-domain)))
                         var)
                      (if random?
                          (s::a-random-member-ofv metric-domain (format nil "t-~A" measure-index))
                          (s::a-member-ofv metric-domain (format nil "t-~A" measure-index)))))
         (top-var  (om?::firstv  ts-var))
         (bot-var  (om?::secondv ts-var))
         (dur-var  (s::/v top-var bot-var)))
    (list :ts ts-var :top top-var :bottom bot-var :dur dur-var)))

(defun metric-bundle-ts  (b) (getf b :ts))
(defun metric-bundle-top (b) (getf b :top))
(defun metric-bundle-bot (b) (getf b :bottom))
(defun metric-bundle-dur (b) (getf b :dur))



;;;; ===========================================================================
;;;; PART 3 -- METRIC CONSTRAINT HELPERS
;;;; ===========================================================================

(defun build-metric-domain (metric-bundles)
  "Build the metric constraint domain from METRIC-BUNDLES.
Returns one (top-var bot-var) list per bundle (numerator and denominator vars)."
  (mapcar #'(lambda (b) (list (metric-bundle-top b) (metric-bundle-bot b)))
          metric-bundles))

(defun apply-metric-constraint (cs-spec metric-bundles)
  "Apply one metric constraint spec (fn input-mode) to METRIC-BUNDLES."
  (let* ((fn         (first  cs-spec))
         (input-mode (second cs-spec))
         (domain     (build-metric-domain metric-bundles)))
    (apply-contv-2 fn "list" (if (equal input-mode "list") "off" input-mode) domain)))


;;;; ===========================================================================
;;;; PART 4 -- DOMAIN CONSTRUCTORS
;;;; ===========================================================================

(defun rhythm-domain (n-variables domain)
  "Create a rhythm domain spec for one voice.
N-VARIABLES: number of rhythm slots (notes + rests).
DOMAIN:      list of rational values. Positive = note, negative = rest.
             e.g. '(1/4 1/8 -1/8 1/2)
Returns: (:n-notes N :domain DOMAIN)"
  (list :n-notes n-variables :domain domain))

(defun pitch-domain (domain)
  "Create a pitch domain spec for one voice.
DOMAIN: list of pitch values (MIDI note numbers). Do NOT include -1 --
        rest slots receive -1 automatically via rest-link-post!.
        e.g. '(60 62 64 67)
Returns: (:domain DOMAIN)"
  (list :domain domain))

(defun metric-domain (domain)
  "Create a metric domain spec.
DOMAIN: list of (num denom) time signature lists.
        NEVER use fractions: '(3/4) reads as '(1) in Lisp -- always (num denom).
        e.g. '((4 4)) for fixed 4/4, '((2 4)(3 4)(4 4)) for variable meter.
The number of measures is computed automatically by SCREAMER-SCORE-2 as the
maximum that could be needed: ceiling(max-n-notes * max-dur / min-ts-dur).
Extra measures are trimmed in decoding.
Returns: (:domain DOMAIN)"
  (list :domain domain))

(defun metric-domain-full (specs)
  "Create a METRIC-DOMAIN-SPEC with per-TS beat overrides.
SPECS: list of (ts beat-spec) pairs.
  ts:        (num denom) -- NEVER use fractions.
  beat-spec: nil (auto = 1/denom), rational (uniform, e.g. 3/8), or list (non-uniform).

Examples:
  (metric-domain-full '(((4 4) nil) ((6 8) 3/8)))
  (metric-domain-full '(((5 8) (2/8 3/8)) ((9 8) (3/8 2/8 2/8 2/8 2/8))))
Returns a METRIC-DOMAIN-SPEC instance."
  (make-instance 'metric-domain-spec
    :ts-domain (mapcar #'first specs)
    :beat-map  (loop for (ts beat-spec) in specs
                     when beat-spec collect (cons ts beat-spec))))

(defparameter *velocity-map*
  '((ppp . 20) (pp . 40) (p . 55) (mp . 60) (mf . 85) (f . 100) (ff . 115) (fff . 127))
  "Maximum MIDI velocity of each OM velocity-symbol range (symbolic notation).")

(defun parse-velocity (d)
  "Coerce a velocity symbol or integer to a MIDI velocity (0-127).
Integers are clamped to [0, 127]."
  (if (integerp d)
      (max 0 (min 127 d))
      (or (cdr (assoc (symbol-name d) *velocity-map*
                      :key  #'symbol-name
                      :test #'string-equal))
          (error "Unknown velocity symbol ~S. Use one of ~S or an integer 0-127."
                 d (mapcar #'car *velocity-map*)))))

(defun velocity-domain (domain)
  "Create a velocity domain spec for one voice.
DOMAIN: list of velocity symbols and/or MIDI velocity integers (0-127).
        Do NOT include -1 -- rest slots receive -1 automatically via rest-link-post!.
        Symbols: ppp pp p mp mf f ff fff (converted to OpenMusic representative velocities).
        Integers: used directly (clamped to 0-127).
        Mixed: '(p mp mf 96 f) is valid.
Returns: (:domain DOMAIN)"
  (list :domain (mapcar #'parse-velocity domain)))


;;;; ===========================================================================
;;;; PART 5 -- CONSTRAINT CONSTRUCTOR
;;;; ===========================================================================

(defparameter *variable-type-options*
  '("pitch" "pc" "dur" "onset" "vel"
    "pitch-pc" "pitch-dur" "pitch-onset" "pitch-vel"
    "pc-dur" "pc-onset" "pc-vel"
    "dur-onset" "dur-vel"
    "onset-vel"
    "pitch-pc-dur" "pitch-pc-onset" "pitch-pc-vel"
    "pitch-dur-onset" "pitch-dur-vel" "pitch-onset-vel"
    "pc-dur-onset" "pc-dur-vel" "pc-onset-vel"
    "dur-onset-vel"
    "pitch-pc-dur-onset" "pitch-pc-dur-vel" "pitch-pc-onset-vel"
    "pitch-dur-onset-vel" "pc-dur-onset-vel"
    "pitch-pc-dur-onset-vel")
  "Canonical variable-type strings for one-voice / measures / measure constraints.
Canonical part order: pitch -> pc -> dur -> onset -> vel.
31 combinations (5 single-part + 10 + 10 + 5 + 1 multi-part).")

(defun split-variable-type (variable-type)
  "Split 'pitch-dur-onset' into ('pitch' 'dur' 'onset'). Returns list of part
strings in declared order. Each part must be one of pitch / dur / onset / vel."
  (loop with len = (length variable-type)
        for start = 0 then (1+ pos)
        for pos = (position #\- variable-type :start start)
        collect (subseq variable-type start (or pos len))
        while pos))

(defun extract-part-vars (part notes)
  "Return list of Screamer-vars from NOTES for one PART of variable-type.
PART is one of pitch / pc / dur / onset / vel. For chord notes, the pitch
and pc slots return lists per slot (one sublist of N vars per chord)."
  (cond
    ((string-equal part "pitch")
     (mapcar #'(lambda (n)
                 (etypecase n
                   (screamer-note  (screamer-note-pitch n))
                   (screamer-chord (screamer-chord-pitch n))))
             notes))
    ((string-equal part "pc")
     (mapcar #'(lambda (n)
                 (etypecase n
                   (screamer-note  (screamer-note-pc-slot n))
                   (screamer-chord (screamer-chord-pc-slot n))))
             notes))
    ((string-equal part "dur")    (rhythms-of notes))
    ((string-equal part "onset")  (onsets-of notes))
    ((string-equal part "vel")    (vels-of notes))
    (t (error "Unknown variable-type part ~S. Expected pitch / pc / dur / onset / vel." part))))

(defun extract-vars-by-type (variable-type notes)
  "Build the constraint payload for VARIABLE-TYPE from NOTES.
Single-part (e.g. 'pitch')   -> flat list of vars.
Multi-part  (e.g. 'pitch-dur') -> list of N sublists, one per part, in
                                  declared order (pitch / dur / onset / vel)."
  (let* ((parts (split-variable-type variable-type))
         (cols  (mapcar #'(lambda (p) (extract-part-vars p notes)) parts)))
    (if (null (rest parts)) (first cols) cols)))

(defun cols->slot-rows (cols)
  "Transpose COLS (list of N parallel var lists) into a list of slot tuples.
Each slot tuple is a list of N vars, one per part, in declared order."
  (apply #'mapcar #'list cols))

(defun apply-contv-with-variable-type (fn input variable-type notes)
  "Extract vars per VARIABLE-TYPE from NOTES, then dispatch over INPUT mode.
Single-part: behaves as the legacy notes->vars path -- flat var list, all
 INPUT modes work via apply-contv-2 windowing.
Multi-part:
  list      -> fn called once with cols (list of N sublists) as a single arg.
  n-inputs  -> windows of slot-tuples; arity = (length lambda-list) of fn.
               Each call: (apply fn tuple_i ... tuple_{i+arity-1}).
  car-cdr   -> (funcall fn tuple_i (rest-tuples)) for descending tails.
  growing   -> (funcall fn (slot-tuples-prefix-of-length-i)) i = 1..N."
  (let* ((parts (split-variable-type variable-type))
         (cols  (mapcar #'(lambda (p) (extract-part-vars p notes)) parts)))
    (cond
      ;; Single-part: identical pipeline to legacy.
      ((null (rest parts))
       (apply-contv-2 fn "list"
                      (if (string-equal input "list") "off" input)
                      (first cols)))
      ;; Multi-part list: cols passed as single arg.
      ((string-equal input "list")
       (apply-contv-2 fn "list" "off" cols))
      ;; Multi-part windowed/structural: operate on slot-tuples (rows).
      (t
       (apply-contv-2 fn "list" input (cols->slot-rows cols))))))


(defun make-screamer-score-constraint (&key constraint
                                            bpf
                                            approx
                                            range
                                            (sampling "x-axis")
                                            voices
                                            type
                                            variable-type
                                            (input "list")
                                            percentage
                                            (rests "include")
                                            (index nil)
                                            (measure nil)
                                            (measures nil)
                                            (cs-mode "propagation"))
  "Create a screamer-score-2 constraint spec plist. All arguments are keyword.

:CONSTRAINT     lambda. Signature depends on VARIABLE-TYPE and INPUT.
                Single-part variable-type (e.g. \"pitch\"): receives a flat list
                of Screamer vars; INPUT n-inputs / car-cdr / growing window it
                exactly like the legacy notes path.
                Multi-part variable-type (e.g. \"pitch-dur\"): for INPUT \"list\"
                receives one arg = list of N sublists (one per part, in declared
                order); for n-inputs / car-cdr / growing receives slot-tuples.
                For :type \"metric\", receives a list of screamer-time-signatures.
:BPF            BPF object (x/y points) for profile constraints.  [TODO]
:VOICES         which voices to apply to.
                integer    -> apply to that single voice.
                '(0 1 2)   -> apply to voices 0, 1, 2 independently (one-voice type).
                nil        -> all voices.
                Not used for :type \"metric\".
:TYPE           \"one-voice\" | \"metric\" | \"measures\".
:VARIABLE-TYPE  one of *VARIABLE-TYPE-OPTIONS* (15 combinations of pitch / dur /
                onset / vel). Mandatory for :type \"one-voice\" and \"measures\".
                Determines which Screamer-vars the engine extracts from the
                note structs before applying the predicate.
:INPUT          \"list\" | \"n-inputs\" | \"car-cdr\" | \"growing\".
                Default: \"list\".
:PERCENTAGE     nil (always apply) or integer (apply to N% of cases).  [TODO]
:INDEX          list of note positions (0-indexed among notes only) e.g. '(0 10).
                When non-nil, :rests is automatically set to \"exclude\".
                fn receives the extracted vars at requested positions.
                nil (default) -- positional index not used.
:RESTS          \"include\" (default) -- fn receives all notes including rests.
                \"exclude\" -- fn receives only non-rest notes.
:CS-MODE        \"propagation\" (default) -- fn uses Screamer ops; applied before search.
                \"backtrack\" -- fn fires when vars become ground; receives concrete values."
  (let ((compiled-fn
         (when (functionp constraint)
           (if (string-equal cs-mode "backtrack")
               (compile-screamer-backtrack-constraint-2 constraint)
               (compile-screamer-constraint-2 constraint)))))
    (when (and (or (string-equal type "one-voice") (string-equal type "measures"))
               (not (and (stringp variable-type)
                         (member variable-type *variable-type-options*
                                 :test #'string-equal))))
      (error "MAKE-SCREAMER-SCORE-CONSTRAINT: :variable-type ~S is required for :type ~S~%~
              and must be one of ~S."
             variable-type type *variable-type-options*))
    (list :constraint    (or compiled-fn constraint)
          :bpf           bpf
          :approx        approx
          :range         range
          :sampling      sampling
          :voices        voices
          :type          type
          :variable-type variable-type
          :input         input
          :percentage    percentage
          :rests         rests
          :index         index
          :measure       measure
          :measures      measures
          :cs-mode       cs-mode)))


;;;; ===========================================================================
;;;; PART 6 -- FORCE FUNCTION PARSING
;;;; ===========================================================================

(screamer::defun parse-force-function (spec)
  "Parse a force-function spec string or list into a Screamer ordering argument."
  (cond
    ((null spec)
     (s::static-ordering #'s::linear-force))
    ((and (stringp spec) (string-equal spec "static-ordering linear-force"))
     (s::static-ordering #'s::linear-force))
    ((and (stringp spec) (string-equal spec "static-ordering divide-and-conquer-force"))
     (s::static-ordering #'s::divide-and-conquer-force))
    ((and (stringp spec) (string-equal spec "static-ordering random-force"))
     (s::static-ordering #'s::random-force))
    ((and (listp spec) (string-equal (first spec) "reorder"))
     (s::reorder
      (let ((of (second spec)))
        (cond ((null of)                                #'s::domain-size)
              ((functionp of)                           of)
              ((string-equal of "domain-size")          #'s::domain-size)
              ((string-equal of "range-size")           #'s::range-size)
              ((string-equal of "score-position")       #'s::score-position)
              ((string-equal of "onset-position")        #'s::onset-position)
              (t                                        #'s::domain-size)))
      (let ((rf (third spec)))
        (cond ((null rf)                                #'(lambda (x) (declare (ignore x)) nil))
              ((functionp rf)                           rf)
              ((string-equal rf "(< x 1e-6)")           #'(lambda (x) (< x 1e-6)))
              (t                                        #'(lambda (x) (declare (ignore x)) nil))))
      (let ((ord (fourth spec)))
        (cond ((null ord)                               #'<)
              ((functionp ord)                          ord)
              ((string-equal ord ">")                   #'>)
              (t                                        #'<)))
      (let ((ff (fifth spec)))
        (cond ((null ff)                                #'s::linear-force)
              ((string-equal ff "linear-force")         #'s::linear-force)
              ((string-equal ff "divide-and-conquer-force") #'s::divide-and-conquer-force)
              ((string-equal ff "random-force")         #'s::random-force)
              (t                                        #'s::linear-force)))))
    (t
     (s::static-ordering #'s::linear-force))))


;;;; ===========================================================================
;;;; PART 7 -- CONSTRAINT APPLICATION DISPATCH
;;;; ===========================================================================

(defun collect-flat-constraints (cs-or-list)
  "Flatten a (possibly nested) list of constraint plists, preserving each plist intact.
A constraint plist is identified by its first element being EQ to :CONSTRAINT.
NIL entries are silently dropped."
  (cond ((null cs-or-list) nil)
        ((and (listp cs-or-list) (eq (first cs-or-list) :constraint))
         (list cs-or-list))
        ((listp cs-or-list)
         (mapcan #'(lambda (x) (if x (collect-flat-constraints x) nil))
                 cs-or-list))
        (t nil)))

(defun resolve-voice-indices (voices n-voices)
  "Resolve :voices spec to a list of voice-index groups (one-voice semantics).
  nil or \"all\"   -> ((0) (1) ... (n-1))  -- each voice independently
  '(0 1 2)        -> ((0) (1) (2))        -- same, explicit
  '((0 1) (2 3))  -> ((0 1) (2 3))        -- pre-grouped"
  (cond
    ((or (null voices) (and (stringp voices) (string-equal voices "all")))
     (loop for i below n-voices collect (list i)))
    ((and (listp voices) (listp (first voices)))
     voices)
    ((listp voices)
     (mapcar #'list voices))
    (t (list (list voices)))))


(defun apply-one-voice-cs! (cs notes)
  "Apply CS receiving Screamer-vars extracted from NOTES per :variable-type.
Dispatches over :input mode via apply-contv-with-variable-type."
  (let* ((fn            (getf cs :constraint))
         (input         (getf cs :input))
         (variable-type (getf cs :variable-type)))
    (apply-contv-with-variable-type fn input variable-type notes)))

(defun build-filtered-notes (notes rests-mode)
  "Return contiguous prefix of NOTES with bound rhythm.
- rests-mode 'include' (or NIL): all bound-rhythm notes
- rests-mode 'exclude': only non-rest bound notes (rhythm >= 0)
Stops at first slot with unbound rhythm."
  (let ((acc '())
        (exclude? (and rests-mode (string-equal rests-mode "exclude"))))
    (block done
      (dolist (n notes)
        (let ((r (screamer-note-rhythm n)))
          (cond
            ((not (s::bound? r)) (return-from done))
            ((and exclude? (minusp (s::value-of r))) nil)   ; skip rest
            (t (push n acc))))))
    (nreverse acc)))

(defun funcall-rec-notes (fn input variable-type notes rests-mode)
  "Apply FN to filtered notes for n-inputs or growing mode.
Called from p-chain-node noticers each time a new note commits.
Vars are extracted from filtered notes per VARIABLE-TYPE before dispatch."
  (let* ((filtered (build-filtered-notes notes rests-mode))
         (n        (length filtered)))
    (cond
      ((string-equal input "n-inputs")
       (let ((w (length (function-lambda-list fn))))
         (if (>= n w)
             (apply-contv-with-variable-type
                fn "n-inputs" variable-type
                (last filtered w))
             :not-ready)))
      ((string-equal input "growing")
       (if (> n 0)
           (apply-contv-with-variable-type
              fn "list" variable-type filtered)
           :not-ready)))))

(defun apply-one-voice-cs-exclude-rests! (cs notes)
  "Apply CS with :rests \"exclude\". Constructs a p-chain from notes and
attaches noticers on chain nodes (phase 2 -- chain-build binds the node,
firing the noticer afterwards).
  list/car-cdr    -- noticer on the LAST node, applied once (applied flag).
  n-inputs/growing -- noticer on EACH node, applied per growth (last-len dedup)."
  (let* ((fn            (getf cs :constraint))
         (input         (getf cs :input))
         (variable-type (getf cs :variable-type))
         (rests-mode    (or (getf cs :rests) "include"))
         (r-vars        (rhythms-of notes))
         (p-vars        (pitches-of notes))
         (p-chain       (s::build-chain r-vars p-vars))
         (chain-head    (first p-chain))
         (p-nodes       (second p-chain)))
    (cond
      ((or (string-equal input "list") (string-equal input "car-cdr"))
       (let ((applied   nil)
             (last-node (car (last p-nodes))))
         (s::attach-noticer!
           #'(lambda ()
               (when (and (not applied)
                          (s::bound? last-node)
                          (alexandria::proper-list-p (s::deep-value-of chain-head)))
                 (let ((filtered (build-filtered-notes notes rests-mode)))
                   (when filtered
                     (s::local (setq applied t))
                     (apply-contv-with-variable-type
                        fn input variable-type filtered)))))
           last-node)))
      (t
       (let ((last-len 0))
         (mapc #'(lambda (node)
                   (s::attach-noticer!
                     #'(lambda ()
                         (when (s::bound? node)
                           (let* ((filtered (build-filtered-notes notes rests-mode))
                                  (n (length filtered)))
                             (when (> n last-len)
                               (s::local (setq last-len n))
                               (funcall-rec-notes fn input variable-type notes rests-mode)))))
                     node))
               p-nodes))))))

(defun index-funcall-notes (fn variable-type index-list notes)
  "Return :not-ready if any indexed position is not yet a committed note.
Otherwise apply FN to the vars (extracted per VARIABLE-TYPE) at each index
position. Counting uses build-filtered-notes 'exclude' (rests skipped)."
  (let* ((filtered (build-filtered-notes notes "exclude")))
    (cond
      ((<= (length filtered) (apply #'max index-list))
       :not-ready)
      (t
       (let* ((selected (mapcar #'(lambda (i) (nth i filtered)) index-list))
              (parts    (split-variable-type variable-type))
              (cols     (mapcar #'(lambda (p) (extract-part-vars p selected))
                                parts)))
         (cond
           ((null (rest parts)) (apply fn (first cols)))
           (t                   (funcall fn cols))))))))

(defun apply-one-voice-cs-index! (cs notes)
  "Apply :index constraint. NOTES is the list of screamer-notes for the voice.
Constructs a p-chain for phase-2 noticing; each attached node attempts FN
when all positions in the index have committed notes.
Single-part variable-type: fn called via APPLY (multi-arg, one var per index).
Multi-part variable-type:  fn called via FUNCALL with cols list (single arg)."
  (let* ((fn            (getf cs :constraint))
         (variable-type (getf cs :variable-type))
         (raw-index     (getf cs :index))
         (groups        (if (every #'numberp raw-index) (list raw-index) raw-index))
         (r-vars        (rhythms-of notes))
         (p-vars        (pitches-of notes))
         (p-nodes       (second (s::build-chain r-vars p-vars))))
    (dolist (index-list groups)
      (let ((applied nil)
            (index-list index-list))
        (mapc #'(lambda (node)
                  (s::attach-noticer!
                    #'(lambda ()
                        (when (and (not applied) (s::bound? node))
                          (let ((result (index-funcall-notes
                                          fn variable-type index-list notes)))
                            (unless (eq result :not-ready)
                              (s::local (setq applied t))))))
                    node))
              p-nodes)))))

;;;; ===========================================================================
;;;; MEASURES CONSTRAINT SUPPORT
;;;; ===========================================================================

(defun build-measure-chains (notes ts-onset-vars n-measures)
  "Build N-MEASURES lazy cons chains of screamer-notes -- one per sounding event.
Chain m contains notes whose sound is in measure m. Each chain element is a
NOTE (not a tuple). Nodes are pre-allocated (backtrackable); noticer attached
em (rhythm onset) binda node[m][k] uma vez ambos committed.
Returns (values chain-heads nodes-by-measure)."
  (let* ((n-slots (length notes))
         (nodes   (loop for m from 0 below n-measures
                        collect (loop for k from 0 to n-slots
                                      collect (if (= k n-slots)
                                                  nil
                                                  (s::make-variable))))))
    (loop for k from 0 below n-slots
          for note in notes
          do (let ((k k)
                   (note note))
               (s::attach-noticer!
                 #'(lambda ()
                     (when (and (s::bound? (screamer-note-rhythm note))
                                (s::bound? (screamer-note-onset note)))
                       (let* ((onset    (s::value-of (screamer-note-onset note)))
                              (dur      (s::value-of (screamer-note-rhythm note)))
                              (note-end (+ onset (abs dur))))
                         (loop for m from 0 below n-measures
                               do (let* ((node-mk  (nth k      (nth m nodes)))
                                         (node-mk1 (nth (1+ k) (nth m nodes)))
                                         (ms       (s::value-of (nth m       ts-onset-vars)))
                                         (me       (s::value-of (nth (1+ m)  ts-onset-vars))))
                                    (when (and (not (s::bound? node-mk))
                                               (not (s::variable? ms))
                                               (not (s::variable? me)))
                                      (cond
                                        ((and (< onset me) (> note-end ms))
                                         (s::assert!-equalv node-mk (cons note node-mk1)))
                                        ((>= onset me)
                                         (s::assert!-equalv node-mk nil))
                                        (t
                                         (s::assert!-equalv node-mk node-mk1)))))))))
                 (list (screamer-note-rhythm note) (screamer-note-onset note)))))
    (values (mapcar #'car nodes) nodes)))

(defun build-measure-notes-filtered (m-chain-head rests-mode)
  "Extract notes from completed measure chain M-CHAIN-HEAD.
Filter rests if RESTS-MODE is \"exclude\"."
  (let ((notes (s::chain-value m-chain-head)))
    (if (string-equal rests-mode "exclude")
        (remove-if #'(lambda (n)
                       (let ((rv (screamer-note-rhythm n)))
                         (and rv (s::bound? rv) (minusp (s::value-of rv)))))
                   notes)
        notes)))

(defun build-measure-notes-union (relevant-chains rests-mode)
  "Union de filtered notes acrosso relevant measure chains (preserva onset order)."
  (apply #'append
         (mapcar #'(lambda (mh)
                     (build-measure-notes-filtered mh rests-mode))
                 relevant-chains)))

(defun chains-deep-bound-p (relevant-chains)
  "T if every chain in RELEVANT-CHAINS resolves to a proper list."
  (every #'(lambda (mh)
             (alexandria::proper-list-p (s::deep-value-of mh)))
         relevant-chains))

(defvar *ss2-measure-debug* nil
  "When T, apply-one-voice-cs-measure! prints noticer firings.")

(defun apply-one-voice-cs-measure! (cs m-chains m-nodes)
  "Apply :measure CS over screamer-note chains. Noticers on chain NODES
(phase 2) -- chain-build binds nodes via assert!-equalv, firing the noticer
AFTERWARDS.
  list/car-cdr    -- noticer on each node, applied once via applied flag.
  n-inputs/growing -- noticer on each node, applied per growth (last-len dedup)."
  (let* ((fn            (getf cs :constraint))
         (variable-type (getf cs :variable-type))
         (rests-mode    (or (getf cs :rests) "include"))
         (input         (or (getf cs :input) "list"))
         (raw-ms        (getf cs :measure))
         (groups        (mapcar #'(lambda (spec)
                                    (if (numberp spec) (list spec) spec))
                                raw-ms)))
    (when *ss2-measure-debug*
      (format om::*om-stream*
              "~%[measure-cs SETUP] input=~S rests=~S groups=~S~%"
              input rests-mode groups))
    (dolist (m-indices groups)
      (let* ((relevant-chains (mapcar #'(lambda (mi) (nth mi m-chains)) m-indices))
             (relevant-node-lists (mapcar #'(lambda (mi) (nth mi m-nodes)) m-indices)))
        (cond
          ((or (string-equal input "list") (string-equal input "car-cdr"))
           (let ((applied nil))
             (dolist (nodes-of-measure relevant-node-lists)
               (dolist (node nodes-of-measure)
                 (when (s::variable? node)  ; skip trailing nil
                   (s::attach-noticer!
                    #'(lambda ()
                        (when (and (not applied)
                                   (chains-deep-bound-p relevant-chains))
                          (let ((notes (build-measure-notes-union
                                         relevant-chains rests-mode)))
                            (when *ss2-measure-debug*
                              (format om::*om-stream*
                                      "[measure-cs list/car-cdr APPLY] group=~S notes-len=~A~%"
                                      m-indices (length notes)))
                            (s::local (setq applied t))
                            (apply-contv-with-variable-type
                               fn input variable-type notes))))
                    node))))))
          (t
           (let ((last-len 0))
             (dolist (nodes-of-measure relevant-node-lists)
               (dolist (node nodes-of-measure)
                 (when (s::variable? node)  ; skip trailing nil
                   (s::attach-noticer!
                    #'(lambda ()
                        (let* ((notes (build-measure-notes-union
                                        relevant-chains rests-mode))
                               (n     (length notes)))
                          (when *ss2-measure-debug*
                            (format om::*om-stream*
                                    "[measure-cs ~A FIRE] group=~S last-len=~A n=~A grow?=~A~%"
                                    input m-indices last-len n (> n last-len)))
                          (when (> n last-len)
                            (let ((old-len last-len))
                              (s::local (setq last-len n))
                              (cond
                                ((string-equal input "n-inputs")
                                 (mapc #'(lambda (note)
                                           (apply-contv-with-variable-type
                                              fn "n-inputs" variable-type
                                              (list note)))
                                       (subseq notes old-len n)))
                                ((string-equal input "growing")
                                 (loop for i from (1+ old-len) to n
                                       do (apply-contv-with-variable-type
                                             fn "list" variable-type
                                             (subseq notes 0 i)))))))))
                    node)))))))))))

(defun apply-measures-cs! (cs voice-notes-per-v measure-chains-per-v)
  "Apply :measures (plural) constraint across multiple (measure-spec, voice) pairs.
:measures and :voices are parallel lists. Each pair produces one extracted-vars
argument to fn. Vars are extracted per :variable-type from the notes of each pair.
Single-part variable-type: each pair contributes a flat var list as one fn arg.
Multi-part variable-type:  each pair contributes a list of N sublists as one arg.
Chains contem screamer-notes."
  (let* ((fn            (getf cs :constraint))
         (variable-type (getf cs :variable-type))
         (rests-mode    (or (getf cs :rests) "include"))
         (raw-ms        (getf cs :measures))
         (raw-vs        (getf cs :voices))
         (ms-specs      (mapcar #'(lambda (m) (if (listp m) m (list m))) raw-ms))
         (v-indices     raw-vs)
         (pairs         (mapcar #'cons ms-specs v-indices))
         (all-chains    (remove-duplicates
                          (loop for pair in pairs
                                append (mapcar #'(lambda (mi)
                                                   (nth mi (nth (cdr pair) measure-chains-per-v)))
                                               (car pair)))))
         (unique-vis    (remove-duplicates v-indices))
         (all-pos-vars  (apply #'append
                               (mapcar #'(lambda (vi)
                                           (let* ((notes (nth vi voice-notes-per-v))
                                                  (rs    (rhythms-of notes))
                                                  (ps    (pitches-of notes))
                                                  (chain (s::build-chain rs ps)))
                                             (second chain)))
                                       unique-vis))))
    (dolist (pv all-pos-vars)
      (s::attach-noticer!
        #'(lambda ()
            (when (s::bound? pv)
              (when (every #'(lambda (ch)
                               (alexandria::proper-list-p (s::deep-value-of ch)))
                           all-chains)
                (let ((event-arg-lists
                        (mapcar #'(lambda (pair)
                                    (let ((notes
                                            (apply #'append
                                                   (mapcar #'(lambda (mi)
                                                               (build-measure-notes-filtered
                                                                 (nth mi (nth (cdr pair) measure-chains-per-v))
                                                                 rests-mode))
                                                           (car pair)))))
                                      (extract-vars-by-type variable-type notes)))
                                pairs)))
                  (apply fn event-arg-lists)))))
        pv))))

(defun apply-screamer-score-2-constraint (cs voice-notes-per-v metric-bundles)
  "Apply one screamer-score-2 constraint plist upfront using screamer-notes.
Only called for include-cs (:rests nil or \"include\"). Exclude-cs are handled
separately in the engine body via per-voice notes.

Dispatch:
  type = \"metric\"    -> apply-metric-constraint on metric bundles.
  type = \"one-voice\" -> pass notes to apply-one-voice-cs! or filter by :index.
  type = \"profile\"   -> sample bpf, scale to per-voice domain, apply bounds."
  (let* ((voices   (getf cs :voices))
         (type     (getf cs :type))
         (n-voices (length voice-notes-per-v))
         (v-groups (resolve-voice-indices voices n-voices)))
    (cond
      ((string-equal type "metric")
       (apply-metric-constraint (list (getf cs :constraint) (getf cs :input)) metric-bundles))
      ((string-equal type "profile")
       (apply-cs-profile cs voice-notes-per-v))
      ((string-equal type "one-voice")
       (dolist (vg v-groups)
         (let* ((vi            (first vg))
                (notes         (nth vi voice-notes-per-v))
                (index         (getf cs :index))
                (variable-type (getf cs :variable-type)))
           (when notes
             (if index
                 (let ((groups (if (every #'numberp index) (list index) index)))
                   (dolist (group groups)
                     (let* ((selected (mapcar #'(lambda (i) (nth i notes)) group))
                            (parts    (split-variable-type variable-type))
                            (cols     (mapcar #'(lambda (p) (extract-part-vars p selected))
                                              parts)))
                       (cond
                         ((null (rest parts)) (apply (getf cs :constraint) (first cols)))
                         (t                   (funcall (getf cs :constraint) cols))))))
                 (apply-one-voice-cs! cs notes)))))))))


;;;; ===========================================================================
;;;; PROFILE CONSTRAINT
;;;;
;;;; Sample a SCREAMER-BPF (or BPF-LIB) to N points, where N = number of
;;;; variables of the chosen attribute in each target voice. Scale samples
;;;; to the per-voice (or pooled) attribute domain and apply per-variable
;;;; bounds (>=v var (- target approx)) (<=v var (+ target approx)).
;;;;
;;;; Range modes:
;;;;   "voice-range" -- per voice, use that voice's own attribute domain.
;;;;   "all"         -- union (deduped, sorted) of all target voices' domains.
;;;;   (lo hi)       -- per voice, filter domain to values in [lo,hi].
;;;;
;;;; Rests:
;;;;   "exclude" (default for pitch/dur) wraps each bound assertion in
;;;;     (impliesv (>=v rhythm 0) ...) so rest slots are not constrained.
;;;;   "include" applies bounds unconditionally (caller's responsibility).
;;;; ===========================================================================

(defun profile-var-domain (var)
  "Read VAR's enumerated domain, sort ascending. Errors on unbounded domain."
  (let ((dom (screamer::variable-enumerated-domain var)))
    (if (eq dom t)
        (error "MK-CONSTRAINT-PROFILE: variable ~S has unbounded domain. ~
Profile requires a bounded enumerated domain (a-member-ofv at variable creation)." var)
        (sort (copy-list dom) #'<))))


(defun profile-attribute-domain (variable-type vars)
  "Return the per-attribute domain shared by VARS (homogeneous), sorted asc.
For pitch: drop the rest sentinel (any negative value, e.g. -0.5).
For dur: drop negative values (rests). For onset / vel: keep all values."
  (when vars
    (let ((dom (profile-var-domain (first vars))))
      (cond
        ((string-equal variable-type "pitch") (remove-if #'minusp dom))
        ((string-equal variable-type "dur")   (remove-if #'minusp dom))
        (t dom)))))


(defun profile-scale-to-indices (samples target-min target-max)
  "Linearly scale SAMPLES from their (min,max) into [TARGET-MIN, TARGET-MAX],
round to nearest integer, clamp to bounds. If samples are constant, return
the midpoint integer for every entry."
  (let* ((s-min (reduce #'min samples))
         (s-max (reduce #'max samples)))
    (cond
      ((= s-min s-max)
       (let ((mid (round (/ (+ target-min target-max) 2))))
         (make-list (length samples) :initial-element mid)))
      (t
       (let ((slope (/ (- target-max target-min) (- s-max s-min))))
         (mapcar #'(lambda (s)
                     (max target-min
                          (min target-max
                               (round (+ target-min
                                         (* slope (- s s-min)))))))
                 samples))))))


(defun profile-broadcast-bpfs (bpfs n-target)
  "If BPFS has 1 element, broadcast to N-TARGET copies. If it already has
N-TARGET elements, return as-is. Otherwise error."
  (cond
    ((= (length bpfs) 1)        (make-list n-target :initial-element (first bpfs)))
    ((= (length bpfs) n-target) bpfs)
    (t (error "MK-CONSTRAINT-PROFILE: BPF-LIB has ~A BPFs but constraint targets ~A voices.
Provide one BPF per voice, or a single BPF to broadcast to all."
              (length bpfs) n-target))))


(defun profile-apply-bounds (vars approx targets rhythms)
  "Per (var, target) pair, assert bounds. When RHYTHMS is non-nil, the
assertion is wrapped in (impliesv (>=v r 0) ...) so the bound only fires
on non-rest slots."
  (loop for var in vars
        for target in targets
        for rhythm in (or rhythms (make-list (length vars) :initial-element nil))
        do (let ((bounds-form (s::andv
                                (s::>=v var (- target approx))
                                (s::<=v var (+ target approx)))))
             (s::assert!
               (if rhythm
                   (?::impliesv (s::>=v rhythm 0) bounds-form)
                   bounds-form)))))


(defun profile-compute-onsets (r-vars)
  "Given a list of bound rhythm vars (locked voice), return cumulative onsets
starting at 0. Uses absolute value so rest slots also occupy time."
  (let ((acc 0))
    (loop for r in r-vars
          collect acc
          do (incf acc (abs (s::value-of r))))))


(defun profile-onsets->bpf-positions (onsets bpf)
  "Map ONSETS (cumulative time, starting at 0) to positions in BPF's x-range.
Total time is normalised to [bpf-xmin, bpf-xmax]. Last onset = end of voice
maps to bpf-xmax."
  (let* ((xs    (sbpf-x-points bpf))
         (xmin  (first xs))
         (xmax  (car (last xs)))
         (xrange(- xmax xmin))
         (total (or (car (last onsets)) 0)))
    (cond
      ((zerop total)
       (make-list (length onsets) :initial-element xmin))
      (t
       (mapcar (lambda (o)
                 (+ xmin (* xrange (/ o total))))
               onsets)))))


(defun profile-sample-bpf (bpf n sampling-mode r-vars vi)
  "Sample BPF in N points using the chosen mode.
   index  -- y-list by integer index, ignores x-points spacing.
   x-axis -- BPF over its declared x-axis (default).
   time   -- requires all R-VARS bound (locked voice); sample at proportional
             onset positions; signals an error if any rhythm is unbound."
  (cond
    ((string-equal sampling-mode "index")
     (sbpf-sample-by-index bpf n))
    ((string-equal sampling-mode "x-axis")
     (sbpf-sample bpf n))
    ((string-equal sampling-mode "time")
     (unless (every #'s::bound? r-vars)
       (error "MK-CONSTRAINT-PROFILE: :sampling \"time\" requires locked rhythms.~%~
Voice ~A has unbound rhythm variables. Either pass a locked voice ~
(MK-SCREAMER-VOICE) for that voice, or use :sampling \"index\" / \"x-axis\"." vi))
     (let* ((onsets    (profile-compute-onsets r-vars))
            (positions (profile-onsets->bpf-positions onsets bpf)))
       (sbpf-sample-at-positions bpf positions)))
    (t
     (error "MK-CONSTRAINT-PROFILE: :sampling must be \"index\", \"x-axis\", or \"time\". Got ~S." sampling-mode))))


(defun apply-cs-profile (cs voice-notes-per-v)
  "Profile constraint dispatch. Reads :bpf, :variable-type, :voices,
:approx, :range, :rests, :sampling from CS, then applies bounds per voice."
  (let* ((bpf-input    (getf cs :bpf))
         (variable-type(getf cs :variable-type))
         (voices       (getf cs :voices))
         (approx       (getf cs :approx))
         (range        (getf cs :range))
         (rests-mode   (or (getf cs :rests) "exclude"))
         (sampling     (or (getf cs :sampling) "x-axis"))
         (n-voices     (length voice-notes-per-v))
         (v-groups     (resolve-voice-indices voices n-voices))
         (target-vis   (mapcar #'first v-groups))
         (n-target     (length target-vis))
         (parts        (split-variable-type variable-type)))
    (when (rest parts)
      (error "MK-CONSTRAINT-PROFILE: VARIABLE-TYPE must be single-part. Got ~S." variable-type))
    (when (string-equal variable-type "pc")
      (error "MK-CONSTRAINT-PROFILE: VARIABLE-TYPE \"pc\" is not supported. ~
A BPF defines a curve over real values; pc is mod-12 (no octave information), ~
so curves do not map naturally. Use \"pitch\" instead."))
    (unless bpf-input
      (error "MK-CONSTRAINT-PROFILE: :BPF is required."))
    (unless (numberp approx)
      (error "MK-CONSTRAINT-PROFILE: :APPROX must be a number. Got ~S." approx))
    (let* ((bpf-lib (coerce-to-screamer-bpf-lib bpf-input))
           (bpfs    (profile-broadcast-bpfs (sbpf-list bpf-lib) n-target))
           (per-voice-vars
             (loop for vi in target-vis
                   collect (let* ((notes (nth vi voice-notes-per-v))
                                  (vars  (extract-part-vars variable-type notes)))
                             (when (some #'consp vars)
                               (error "MK-CONSTRAINT-PROFILE: chord-domain pitch (list-of-lists) ~
not supported in profile yet. Voice ~A has chord pitches." vi))
                             vars)))
           (per-voice-domains
             (mapcar #'(lambda (vars)
                         (profile-attribute-domain variable-type vars))
                     per-voice-vars))
           (effective-domains
             (cond
               ((and (stringp range) (string-equal range "voice-range"))
                per-voice-domains)
               ((and (stringp range) (string-equal range "all"))
                (let ((union (sort (copy-list
                                     (remove-duplicates
                                       (apply #'append per-voice-domains)
                                       :test #'=))
                                   #'<)))
                  (make-list n-target :initial-element union)))
               ((listp range)
                (unless (and (= (length range) 2)
                             (numberp (first range))
                             (numberp (second range)))
                  (error "MK-CONSTRAINT-PROFILE: :RANGE list must be (min max). Got ~S." range))
                (let ((lo (first range)) (hi (second range)))
                  (mapcar #'(lambda (dom)
                              (remove-if-not
                                #'(lambda (v) (and (>= v lo) (<= v hi)))
                                dom))
                          per-voice-domains)))
               (t
                (error "MK-CONSTRAINT-PROFILE: :RANGE must be \"voice-range\", \"all\", or a list (min max). Got ~S." range))))
           (guard-rests? (and (string-equal rests-mode "exclude")
                              (or (string-equal variable-type "pitch")
                                  (string-equal variable-type "dur")))))
      (loop for vi in target-vis
            for bpf in bpfs
            for vars in per-voice-vars
            for dom  in effective-domains
            do (let ((notes (nth vi voice-notes-per-v)))
                 (cond
                   ((null vars)
                    (format om::*om-stream* "~&[PROFILE] voice ~A has no vars for variable-type ~S; skipped.~%"
                            vi variable-type))
                   ((null dom)
                    (format om::*om-stream* "~&[PROFILE] voice ~A: effective domain is empty after :range filter; skipped.~%" vi))
                   (t
                    (let* ((n         (length vars))
                           (r-vars    (rhythms-of notes))
                           (samples   (profile-sample-bpf bpf n sampling r-vars vi))
                           (positions (profile-scale-to-indices samples 0 (1- (length dom))))
                           (targets   (mapcar #'(lambda (i) (nth i dom)) positions))
                           (rhythms   (when guard-rests? r-vars)))
                      (profile-apply-bounds vars approx targets rhythms)))))))))




(defun setup-no-consecutive-rests! (r-vars-per-v)
  "Assert no two consecutive rests per voice (unlocked voices only)."
  (dolist (r-vars r-vars-per-v)
    (loop for (r1 r2) on r-vars while r2
          do (s::assert! (?::impliesv (s::<v r1 0) (s::>=v r2 0))))))

(defun build-grid-for-measure (m-start ts pos-durs beat-spec)
  "Compute valid onset positions within [m-start, m-start+ts-dur).
BEAT-SPEC: nil (auto), rational (uniform), or list (non-uniform) -- see BEAT-SPANS."
  (let* ((spans       (beat-spans ts beat-spec))
         (measure-end (+ m-start (ts-measure-duration ts)))
         (positions   nil))
    (flet ((valid-residual-p (residual)
             (or (zerop residual)
                 (some #'(lambda (d) (zerop (mod residual d))) pos-durs))))
      (dolist (span spans)
        (let* ((beat-start (+ m-start (car span)))
               (beat-dur   (cdr span))
               (beat-end   (min (+ beat-start beat-dur) measure-end)))
          (push beat-start positions)
          (dolist (d pos-durs)
            (loop for pos = (+ beat-start d) then (+ pos d)
                  while (< pos beat-end)
                  do (when (valid-residual-p (- beat-end pos))
                       (push pos positions)))))))
    (push measure-end positions)
    (sort (remove-duplicates positions :test #'=) #'<)))


(defun setup-metric-grid! (all-ts-vars ts-onset-vars o-vars-per-v beat-map pos-durs)
  (let* ((beats-per-measure
          (mapcar #'(lambda (ts-var ts-onset)
                      (s::funcallv #'(lambda (s o)
                                       (build-grid-for-measure o s pos-durs
                                                               (resolve-beat-spec s beat-map)))
                                   ts-var ts-onset))
                  all-ts-vars ts-onset-vars))
         (all-ts-beats (apply #'s::funcallv #'append beats-per-measure)))
    (dolist (o-list o-vars-per-v)
      (dolist (o o-list)
        (let ((o o))
          (s::attach-noticer!
            #'(lambda ()
                (when (and (s::bound? o) (s::bound? all-ts-beats))
                  (unless (member (s::value-of o) (s::value-of all-ts-beats) :test #'=)
                    (s::fail))))
            (list o all-ts-beats)))))))

(defun fill-ss2-var->onset-hash (all-ts-vars ts-onset-vars o-vars-per-v r-vars-per-v p-vars-per-v v-vars-per-v)
 (clrhash s::*ss2-var->onset*)

;; ts-vars: ts-var[i] -> ts-onset-var[i] (onset of measure i).
;; ts-onset-vars has n+1 elements (dx->xv); loop stops at all-ts-vars (n).
 (loop for ts in all-ts-vars
       for ov in ts-onset-vars
       do (setf (gethash ts s::*ss2-var->onset*) ov))

;; d/p/v vars: all three share the same onset var per slot.
(loop for r-list in r-vars-per-v
      for o-list in o-vars-per-v
      do (loop for r in r-list
      for o in o-list
      do (setf (gethash r s::*ss2-var->onset*) o)))

;; p-vars-per-v[vi] is either a flat list of pitch vars (mono voice) or
;; a list of sublists (chord voice, one sublist of N pitch vars per slot).
;; All chord-notes in a slot share the slot's onset.
(loop for p-list in p-vars-per-v
      for o-list in o-vars-per-v
      do (loop for entry in p-list
               for o     in o-list
               do (cond
                    ((listp entry)
                     (dolist (p entry)
                       (setf (gethash p s::*ss2-var->onset*) o)))
                    (t
                     (setf (gethash entry s::*ss2-var->onset*) o)))))

(loop for v-list in v-vars-per-v
      for o-list in o-vars-per-v
      do (loop for v in v-list
               for o in o-list
               do (setf (gethash v s::*ss2-var->onset*) o))))

(defvar *screamer-score-vars-debug* nil)
(setf *screamer-score-vars-debug* nil)

;;;; ===========================================================================
;;;; PART 8 -- SCREAMER-SCORE-2
;;;; ===========================================================================

(defun screamer-score-2-engine (tempo metric-domain-spec rhythm-domain-specs pitch-domain-specs
                                &key velocity-domain-specs
                                     (random? t) ordering-force-functions constraints
                                     (count-failures-timed? nil)
                                     (metric-grid nil) (beat-unit nil) (no-consecutive-rests t))
  "Flat onset-ordered metric + rhythm + pitch search. Pure Common Lisp, REPL-testable.

ARCHITECTURE (flat one-value):
  All variable types (timesig, duration, pitch, velocity) are bound in a
  single ONE-VALUE search driven by ONSET-POSITION. Variables are bound in
  chronological onset order across all voices simultaneously:
    ts-vars   -> onset = start of their measure (negative priority, always first).
    r-vars    -> onset = cumulative abs-duration of preceding slots in same voice.
    p/v-vars  -> onset = same as r-var at same slot (bound after by list order).

TEMPO:                integer BPM. Stored in result, not used in the search.
METRIC-DOMAIN-SPEC:   output of (metric-domain '((num denom) ...)).
                      Number of measures computed automatically (upper bound).
RHYTHM-DOMAIN-SPECS:  list of outputs of (rhythm-domain n domain), one per voice.
PITCH-DOMAIN-SPECS:   list of outputs of (pitch-domain domain), one per voice.

KEYWORD ARGS:

  :RANDOM?   nil -> a-member-ofv (deterministic order); t -> a-random-member-ofv.

  :ORDERING-FORCE-FUNCTIONS
             nil -> default: (reorder onset-position nil #'< linear-force).
             Single spec string or list:
               \"static-ordering linear-force\"
               \"static-ordering divide-and-conquer-force\"
               \"static-ordering random-force\"
               '(\"reorder\" ordering-fn restraint-fn order-fn force-fn)
             ordering-fn: \"onset-position\" | \"domain-size\" | \"range-size\" | \"score-position\"

  :CONSTRAINTS
             list of plists from MAKE-SCREAMER-SCORE-CONSTRAINT.
             Constraint dispatch:
               :type \"metric\"    -> applied upfront to ts-vars (propagation before search).
               :type \"one-voice\" -> applied upfront to Screamer vars.
               :type \"metric\"    -> applied upfront to ts-vars.
             :cs-mode \"propagation\" (default): fn uses Screamer ops; applied before search.
             :cs-mode \"backtrack\": fn receives concrete values; fires via attach-noticer!.

  :METRIC-GRID
             nil (default) -> no grid constraint.
             t -> constrain all onset vars to beat positions in their measure.
             Auto-activated when METRIC-DOMAIN-SPEC contains :beat-map (i.e. from metric-domain-full).
             Uses per-ts-var noticer: fires when ts[i] becomes bound; applies impliesv to onset vars.

  :BEAT-UNIT
             nil (default) -> derive from time signature denominator (1/denom).
             rational -> override beat unit for all measures (e.g. 1/8).

RETURNS: plist or NIL if no solution.
  :timesig  -> list of (num denom) time signatures: ((4 4) (3 4) ...)
  :dur      -> per-voice rhythm rationals: ((v0-s0 v0-s1 ...) ...)
  :pitch    -> per-voice pitches: ((v0-p0 ...) ...) -- -1 at rest slots
  :vel      -> per-voice velocities or NIL if no velocity specified"
  (let* ((metric-ts-domain  (getf metric-domain-spec :domain))
         ;; beat-map: alist ((ts . beat-spec) ...) -- nil when no per-TS or global override.
         ;; Per-TS entries (from metric-domain-full class) take priority; global beat-unit fills rest.
         (beat-map          (let ((per-ts (getf metric-domain-spec :beat-map)))
                              (when (or per-ts beat-unit)
                                (loop for ts in metric-ts-domain
                                      collect (cons ts
                                                    (or (cdr (assoc ts per-ts :test #'equal))
                                                        beat-unit))))))
         (metric-grid       (or metric-grid (not (null (getf metric-domain-spec :beat-map)))))
         (n-measures-init   (compute-n-measures rhythm-domain-specs metric-domain-spec))
         (all-bundles  (loop for k from 0 below n-measures-init
                             collect (make-metric-var-bundle metric-ts-domain random? k)))
         (all-ts-vars  (mapcar #'metric-bundle-ts all-bundles))
         (ts-onset-vars (om?::dx->xv 0 (mapcar #'(lambda (bundle)
                                   (let ((total-dur (getf bundle :dur)))
                                    total-dur))
                                all-bundles)))
         (r-vars-per-v
          (loop for rd in rhythm-domain-specs for vi from 0 collect
                (if (getf rd :locked)
                    (loop for val in (getf rd :domain) for k from 0 collect
                          (let ((var (s::make-variable (format nil "d-~A-~A" vi k))))
                            (s::assert! (s::equalv var val))
                            var))
                    (make-rhythm-variablesv (getf rd :n-notes) (getf rd :domain) random? vi))))
         (o-vars-per-v
          (mapcar #'rhythm-vars->onset-vars r-vars-per-v))
         (p-vars-per-v
          (loop for r-vars in r-vars-per-v for pd-spec in pitch-domain-specs for vi from 0 collect
                (cond
                  ((null pd-spec)
                   nil)
                  ((getf pd-spec :locked)
                   (loop for val in (getf pd-spec :domain) for k from 0 collect
                         (let ((var (s::make-variable (format nil "p-~A-~A" vi k))))
                           (s::assert! (s::equalv var val))
                           var)))
                  ((chord-pitch-spec-p pd-spec)
                   ;; Polyphonic voice. Build a sublist of N pitch variables per
                   ;; slot (N from notes-per-chord). Rest-link is bidirectional
                   ;; over the whole chord: dur < 0 forces every chord-pitch to
                   ;; -1; dur >= 0 forbids -1 in every chord-pitch.
                   (let* ((cd   (getf pd-spec :domain))
                          (pool (chord-domain-domain cd))
                          (npc  (chord-domain-notes-per-chord cd)))
                     (loop for r in r-vars for k from 0 collect
                           (let* ((n (resolve-notes-per-chord npc k))
                                  (full-domain (cons -0.5 pool))
                                  (chord-pitches
                                   (loop for j from 0 below n collect
                                         (funcall (if random?
                                                      #'s::a-random-member-ofv
                                                      #'s::a-member-ofv)
                                                  full-domain
                                                  (format nil "p-~A-~A-~A" vi k j)))))
                             (s::assert! (?::impliesv (s::minuspv r)
                                                      (apply #'?::andv
                                                             (mapcar #'(lambda (p) (s::equalv p -0.5))
                                                                     chord-pitches))))
                             (s::assert! (?::impliesv (s::>=v r 0)
                                                      (apply #'?::andv
                                                             (mapcar #'(lambda (p) (s::notv (s::equalv p -0.5)))
                                                                     chord-pitches))))
                             chord-pitches))))
                  (t
                   (let ((pd (getf pd-spec :domain)))
                     (loop for r in r-vars for k from 0
                           collect
                           (let* ((full-domain (cons -0.5 pd))
                                  (p (funcall (if random?
                                                  #'s::a-random-member-ofv
                                                  #'s::a-member-ofv)
                                              full-domain
                                              (format nil "p-~A-~A" vi k))))
                             (s::assert! (?::impliesv (s::minuspv r) (s::equalv p -0.5)))
                             (s::assert! (?::impliesv (s::>=v r 0) (s::notv (s::equalv p -0.5))))
                             p)))))))

         (v-vars-per-v
          (loop for r-vars in r-vars-per-v for vd-spec in velocity-domain-specs for vi from 0 collect
                (cond
                  ((null vd-spec) nil)
                  ((getf vd-spec :locked)
                   (loop for val in (getf vd-spec :domain) for k from 0 collect
                         (let ((var (s::make-variable (format nil "v-~A-~A" vi k))))
                           (s::assert! (s::equalv var val))
                           var)))
                  (t
                   (let ((vd (getf vd-spec :domain)))
                     (loop for r in r-vars for k from 0 collect
                           (let* ((full-domain (cons -1 vd))
                                  (v (funcall (if random?
                                                  #'s::a-random-member-ofv
                                                  #'s::a-member-ofv)
                                              full-domain
                                              (format nil "v-~A-~A" vi k))))
                             (s::assert! (?::impliesv (s::minuspv r) (s::=v v -1)))
                             (s::assert! (?::impliesv (s::>=v r 0) (s::>=v v 0)))
                             v)))))))

         (flat-cs              (collect-flat-constraints constraints))
         (metric-cs            (remove-if-not #'(lambda (cs) (string-equal (getf cs :type) "metric"))    flat-cs))
         (profile-cs           (remove-if-not #'(lambda (cs) (string-equal (getf cs :type) "profile"))   flat-cs))
         (one-voice-cs         (remove-if-not #'(lambda (cs) (string-equal (getf cs :type) "one-voice")) flat-cs))
         (plural-meas-cs       (remove-if-not #'(lambda (cs) (string-equal (getf cs :type) "measures"))  flat-cs))
         (measures-cs          (remove-if-not #'(lambda (cs) (getf cs :measure)) one-voice-cs))
         (non-meas-cs          (remove-if     #'(lambda (cs) (getf cs :measure)) one-voice-cs))
         (exclude-cs           (remove-if-not #'(lambda (cs) (string-equal (getf cs :rests) "exclude")) non-meas-cs))
         (include-cs           (remove-if     #'(lambda (cs) (string-equal (getf cs :rests) "exclude")) non-meas-cs))
         ;; Build voice-notes-per-v: per-voice list of screamer-note OR
         ;; screamer-chord structs. Required for include-cs, exclude-cs,
         ;; measures and downstream decode-to-poly. Dispatch by pitch-spec
         ;; type: chord-pitch-spec-p triggers make-voice-chords (sublist
         ;; per slot); otherwise the standard monophonic make-voice-notes.
         (voice-notes-per-v
          (loop for vi from 0 below (length r-vars-per-v)
                for pd-spec = (nth vi pitch-domain-specs)
                collect
                (if (chord-pitch-spec-p pd-spec)
                    (make-voice-chords
                      (length (nth vi r-vars-per-v))
                      (nth vi p-vars-per-v)
                      (nth vi r-vars-per-v)
                      (nth vi o-vars-per-v)
                      (when velocity-domain-specs (nth vi v-vars-per-v))
                      vi)
                    (make-voice-notes
                      (length (nth vi r-vars-per-v))
                      (nth vi p-vars-per-v)
                      (nth vi r-vars-per-v)
                      (nth vi o-vars-per-v)
                      (when velocity-domain-specs (nth vi v-vars-per-v))
                      vi))))
         (measure-chain-data
          (when (or measures-cs plural-meas-cs)
            (loop for vi from 0 below (length r-vars-per-v)
                  collect (multiple-value-list
                           (build-measure-chains
                             (nth vi voice-notes-per-v)
                             ts-onset-vars n-measures-init)))))
         (measure-chains-per-v (mapcar #'first  measure-chain-data))
         (measure-nodes-per-v  (mapcar #'second measure-chain-data)))
                            
    (when no-consecutive-rests
      (setup-no-consecutive-rests!
        (loop for r-vars in r-vars-per-v
              for rd in rhythm-domain-specs
              unless (getf rd :locked) collect r-vars)))

    (fill-ss2-var->onset-hash all-ts-vars
                              ts-onset-vars
                              o-vars-per-v
                              r-vars-per-v
                              p-vars-per-v
                              v-vars-per-v)

    (let ((locked-seq (getf metric-domain-spec :locked-seq)))
      (when locked-seq
        (let ((last-ts (car (last locked-seq))))
          (loop for bundle in all-bundles
                for i from 0
                do (s::assert! (s::equalv (metric-bundle-ts bundle)
                                          (if (< i (length locked-seq))
                                              (nth i locked-seq)
                                              last-ts)))))))

    (dolist (cs metric-cs)
      (apply-screamer-score-2-constraint cs voice-notes-per-v all-bundles))
    (dolist (cs profile-cs)
      (apply-screamer-score-2-constraint cs voice-notes-per-v nil))
    (dolist (cs include-cs)
      (apply-screamer-score-2-constraint cs voice-notes-per-v nil))
    (when voice-notes-per-v
      (dolist (cs exclude-cs)
        (let* ((voices   (getf cs :voices))
               (v-groups (resolve-voice-indices voices (length r-vars-per-v))))
          (dolist (vg v-groups)
            (let* ((vi    (first vg))
                   (notes (nth vi voice-notes-per-v)))
              (if (getf cs :index)
                  (apply-one-voice-cs-index! cs notes)
                  (apply-one-voice-cs-exclude-rests! cs notes))))))
      (dolist (cs measures-cs)
        (let* ((voices   (getf cs :voices))
               (v-groups (resolve-voice-indices voices (length r-vars-per-v))))
          (dolist (vg v-groups)
            (let* ((vi       (first vg))
                   (m-chains (nth vi measure-chains-per-v))
                   (m-nodes  (nth vi measure-nodes-per-v)))
              (apply-one-voice-cs-measure! cs m-chains m-nodes)))))
      (dolist (cs plural-meas-cs)
        (apply-measures-cs! cs voice-notes-per-v measure-chains-per-v)))

    (when metric-grid
      (let ((pos-durs (sort (remove-duplicates
                              (remove 0 (mapcar #'abs
                                                (apply #'append
                                                       (mapcar #'(lambda (rd) (getf rd :domain))
                                                               rhythm-domain-specs))))
                              :test #'=)
                            #'<)))
        (unless (every #'(lambda (d) (= 1 (logcount (denominator d)))) pos-durs)
          (setup-metric-grid! all-ts-vars ts-onset-vars o-vars-per-v beat-map pos-durs))))

    (let* ((max-measure-dur (apply #'max
                                   (mapcar #'(lambda (ts) (/ (car ts) (cadr ts)))
                                           metric-ts-domain)))
           (max-n-notes     (apply #'max
                                   (mapcar #'(lambda (rd) (getf rd :n-notes))
                                           rhythm-domain-specs)))
           (max-abs-dur     (apply #'max
                                   (apply #'append
                                          (mapcar #'(lambda (rd) (mapcar #'abs (getf rd :domain)))
                                                  rhythm-domain-specs))))
           (r-offset        (* n-measures-init max-measure-dur)))
      (setf screamer::*ss2-r-phase-onset-offset* r-offset)
      (setf screamer::*ss2-phase2-onset-offset*
            (+ r-offset (* max-n-notes max-abs-dur))))

(when *screamer-score-vars-debug*   

  (loop for ts-var in all-ts-vars
        for ts-o in ts-onset-vars
        do (let ((ts-var ts-var) (ts-o ts-o))
             (s::attach-noticer!
               #'(lambda ()
                   (when (s::bound? ts-var)
                     (format *om-stream* "~%[TS] ~A  onset=~A~%"
                             (s::value-of ts-var) (s::value-of ts-o))
                     ;(format *om-stream* "TRAIL LENGHT: ~A~%" (length screamer::*trail*))
                             ))
               ts-var)))

(loop for o-vars in o-vars-per-v
        for vi from 0
        do (loop for o in o-vars
                 for k from 0
                 do (let ((o o) (vi vi) (k k))
                      (s::attach-noticer!
                        #'(lambda ()
                            (when (and (s::bound? o) (s::deep-bound? all-ts-vars))
                              (format *om-stream* "~%[O v~A s~A] o=~A~%"
                                      vi k (s::value-of o))
                              ;(format *om-stream* "TRAIL LENGHT: ~A~%" (length screamer::*trail*))
                                      )
                             (when (and (s::variable? o) (not (s::bound? o)) (s::enumerated-domain-p o))
                              (format *om-stream* "~%[O v~A s~A] o=~A~%" vi k o))
                                      )
                        o))))
                        
  (loop for r-vars in r-vars-per-v
        for o-vars in o-vars-per-v
        for vi from 0
        do (loop for r in r-vars
                 for o in o-vars
                 for k from 0
                 do (let ((r r) (o o) (vi vi) (k k))
                      (s::attach-noticer!
                        #'(lambda ()
                            (when (and (s::bound? r) (s::deep-bound? all-ts-vars))
                              (format *om-stream* "~%[R v~A s~A] r=~A  onset=~A~%"
                                      vi k (s::value-of r) (s::value-of o))
                              ;(format *om-stream* "TRAIL LENGHT: ~A~%" (length screamer::*trail*))
                                      ))
                        r))))

  ;; P noticers. Each entry in P-VARS-PER-V[vi] is either a single
  ;; pitch var (mono voice) or a sublist of N pitch vars (chord voice).
  ;; The chord branch attaches one noticer per chord-pitch with an
  ;; extra "n<j>" tag in the log label so it is clear which voice/slot
  ;; and chord-position fired.
  (loop for p-vars in p-vars-per-v
        for o-vars in o-vars-per-v
        for vi from 0
        do (loop for entry in p-vars
                 for o     in o-vars
                 for k     from 0
                 do (cond
                      ((listp entry)
                       (loop for p in entry
                             for j from 0
                             do (let ((p p) (o o) (vi vi) (k k) (j j))
                                  (s::attach-noticer!
                                    #'(lambda ()
                                        (when (and (s::bound? p) (s::deep-bound? r-vars-per-v))
                                          (format *om-stream* "~%[P v~A s~A n~A] p=~A  onset=~A~%"
                                                  vi k j (s::value-of p) (s::value-of o))))
                                    p))))
                      (t
                       (let ((p entry) (o o) (vi vi) (k k))
                         (s::attach-noticer!
                           #'(lambda ()
                               (when (and (s::bound? p) (s::deep-bound? r-vars-per-v))
                                 (format *om-stream* "~%[P v~A s~A] p=~A  onset=~A~%"
                                         vi k (s::value-of p) (s::value-of o))))
                           p))))))
     )

    ;; score-solution: forces vars + retorna substituted notes (flat) + signatures.
    ;; Notes/signatures wrapping todas as Screamer-vars dos slots (pitch, rhythm,
    ;; onset, vel, ts, top, bottom, etc.) ja substituidas para valores concretos.
    (let* ((signatures      (make-time-signatures all-ts-vars ts-onset-vars))
           (voice-lengths   (mapcar #'length voice-notes-per-v))
           (all-notes       (apply #'append voice-notes-per-v))
           (force-fn        (parse-force-function ordering-force-functions))
           (raw-result
            (if count-failures-timed?
                (s::count-scs-failures-timed
                 (s::possibly?
                  (multiple-value-list
                   (score-solution all-notes force-fn signatures))))
                (s::possibly?
                 (multiple-value-list
                  (score-solution all-notes force-fn signatures))))))

      (clrhash s::*ss2-var->onset*)

      (when (and *screamer-score-vars-debug* raw-result)
        (format *om-stream*
                "~%SCREAMER-SCORE-2 RESULT: tempo=~A notes=~A sigs=~A~%"
                tempo (first raw-result) (second raw-result)))

      (when raw-result
        ;; Re-split substituted-notes per voice usando voice-lengths.
        (let* ((substituted-notes (first raw-result))
               (substituted-sigs  (second raw-result))
               (notes-per-v (loop with offset = 0
                                  for len in voice-lengths
                                  collect (subseq substituted-notes offset (+ offset len))
                                  do (incf offset len))))
          (list :tempo       tempo
                :notes-per-v notes-per-v
                :signatures  substituted-sigs))))))


;;;; ===========================================================================
;;;; PART 9 -- DECODE UTILITIES
;;;; ===========================================================================

(defun trim-meters-to-duration (meters total-dur)
  "Return only the prefix of METERS whose cumulative duration first reaches TOTAL-DUR.
Discards extra measures created by the maximum-n-measures upper bound strategy."
  (loop with acc = 0
        for ts in meters
        while (< acc total-dur)
        collect ts
        do (incf acc (ts-measure-duration ts))))

(defun decode-solution (result)
  "Decode the output of SCREAMER-SCORE-2 into a REPL-inspectable plist.

RESULT: output of SCREAMER-SCORE-2 (plist with :tempo :timesig :dur :pitch :vel), or NIL.

Plist keys:
  :timesig  -- list of time signatures: ((4 4) (3 4) ...)
  :dur      -- per-voice rhythms: ((v0-s0 ...) (v1-s0 ...) ...)
  :pitch    -- per-voice pitches: ((v0-p0 ...) ...) -- -1 at rest slots
  :vel      -- per-voice velocities or NIL if no velocity specified

Returns:
  (:tempo TEMPO :meters (...) :voices ((:voice 0 :rhythms ... :onsets ... :pitches ... :vels ...)
                                        (:voice 1 ...) ...))
or NIL if RESULT is NIL."
  (when result
    (let* ((tempo         (getf result :tempo))
           (meters-raw     (getf result :timesig))
           (rhythms-per-v (getf result :dur))
           (pitches-per-v (getf result :pitch))
           (vels-per-v    (getf result :vel))
           ;; n-measures-init is an upper bound, so the timesig list may have more measures than needed.
           (total-dur     (reduce #'max
                                  (mapcar #'(lambda (durs)
                                              (reduce #'+ (mapcar #'abs durs)))
                                          rhythms-per-v)))
           (meters        (trim-meters-to-duration meters-raw total-dur)))
      (list :tempo  tempo
            :meters meters
            :voices
            (loop for i       from 0
                  for durs    in rhythms-per-v
                  for pitches in pitches-per-v
                  for vels    in (or vels-per-v (make-list (length rhythms-per-v)))
                  collect
                  (list :voice   i
                        :rhythms durs
                        :onsets  (rhythm-solution->onsets durs 0)
                        :pitches pitches
                        :vels    vels))))))
