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

(in-package :screamer)


;;;; ===========================================================================
;;;; ONSET-ORDERED SEARCH -- GLOBAL STATE
;;;; ===========================================================================

(defvar *ss2-var->onset* (make-hash-table :test #'eq)
  "Hash table mapping every Screamer search variable to its onset Screamer variable.
Covers all types: timesig (\"t-k\"), duration (\"d-vi-k\"), pitch (\"p-vi-k\"), velocity (\"v-vi-k\").
  ts-var[i]    -> ts-onset-var[i]   (onset of measure i)
  r-var[vi][j] -> o-var[vi][j]      (onset of slot j in voice vi)
  p-var[vi][j] -> o-var[vi][j]      (shared with r-var at same slot)
  v-var[vi][j] -> o-var[vi][j]      (shared with r-var at same slot)
Populated by SCREAMER-SCORE-2-ENGINE before ONE-VALUE; cleared after.")

(defvar *ss2-r-phase-onset-offset* 0
  "Offset added to onset-position for duration (\"d\") variables (phase 2).
Set by SCREAMER-SCORE-2-ENGINE before ONE-VALUE to n-measures * max-measure-dur,
ensuring all r-vars are ordered after all ts-vars in the search.")

(defvar *ss2-phase2-onset-offset* 0
  "Offset added to onset-position for pitch (\"p\") and velocity (\"v\") variables (phase 3).
Set by SCREAMER-SCORE-2-ENGINE before ONE-VALUE to
*ss2-r-phase-onset-offset* + max-n-notes * max-abs-dur,
ensuring all content vars (p/v) are ordered after all rhythm vars in the search.")

(defun variable-phase-r? (x)
  "T if X is a phase-2 rhythm variable (duration): name starts with \"d\"."
  (eql 0 (search "d" (variable-name x))))

(defun variable-phase-2? (x)
  "T if X is a phase-3 content variable (pitch or velocity): name starts with \"p\" or \"v\"."
  (let ((name (variable-name x)))
    (or (eql 0 (search "p" name))
        (eql 0 (search "v" name)))))

;;;; ===========================================================================
;;;; ONSET-POSITION -- COST FUNCTION FOR ONSET-ORDERED SEARCH
;;;; ===========================================================================

(defun onset-position (x)
  "Cost function for onset-ordered search across all variable types (t/d/p/v).

Returns the concrete onset value of variable X, or NIL if the onset is not
yet determined (onset Screamer variable is still unbound).

Use with: (reorder #'onset-position nil #'< #'linear-force)

HOW IT WORKS
  *ss2-var->onset* maps every search variable directly to its onset variable.
  gethash lookup is O(1). The onset variable itself is a Screamer variable
  derived via dx->xv (+v chain), so it becomes bound as predecessors become bound.

ORDERING (three phases, separated by global offsets)
  - ts[i]:    onset = measure i start                      -> phase 1 (no offset)
  - r[vi][j]: onset = slot j start + *ss2-r-phase-onset-offset*   -> phase 2
  - p[vi][j], v[vi][j]: same slot onset + *ss2-phase2-onset-offset* -> phase 3"
  (let* ((onset-var (gethash x *ss2-var->onset*))
         (onset-val (value-of onset-var)))
    (when (not (variable? onset-val))
      (cond ((variable-phase-2? x)
             (+ onset-val *ss2-phase2-onset-offset*))
            ((variable-phase-r? x)
             (+ onset-val *ss2-r-phase-onset-offset*))
            (t onset-val)))))

;;;; ===========================================================================
;;;; TIMED FAILURE COUNTER
;;;; ===========================================================================

(defmacro-compile-time count-scs-failures-timed (&body body)
  "Like COUNT-SCS-FAILURES but also reports elapsed wall-clock time."
  (let ((values (gensym "VALUES-"))
        (start  (gensym "START-")))
   `(let ((failure-count 0)
          (,start (get-internal-real-time)))
      (when-failing ((incf failure-count)
       (when (integerp (/ failure-count 1000000))
             (format om::*om-stream*
              "Number of failures: ~:d.~%" failure-count)))
        (let* ((,values   (multiple-value-list (progn ,@body)))
               (elapsed   (/ (- (get-internal-real-time) ,start)
                             internal-time-units-per-second))
               (minutes   (floor elapsed 60))
               (secs-rem  (- elapsed (* minutes 60)))
               (whole-s   (floor secs-rem))
               (ms        (round (* (- secs-rem whole-s) 1000))))
          (format om::*om-stream* 
"~%-------------------------------------~%
Failures    =    ~:d~%
Elapsed     =    ~2,'0d:~2,'0d:~3,'0d~%
-------------------------------------~%"
                  failure-count minutes whole-s ms)
          (values-list ,values))))))

;;;; ===========================================================================
;;;; UTILS
;;;; ===========================================================================

(defun minuspv (x)
 (<v x 0))
 
 (defun pluspv (x)
 (>v x 0)) 
 
(defun zeropv (x)
 (=v x 0))

(defun nullv (x)
 (equalv x nil))

;;;; ===========================================================================
;;;; LAZY CONS CHAIN
;;;; ===========================================================================

(defun chain-value (x)
  "Resolve lazy cons chain X to a proper list, stopping at first unbound tail.
Each element is resolved once via VALUE-OF."
  (let ((v (value-of x)))
    (cond
      ((null v)      nil)
      ((variable? v) nil)
      ((consp v)     (cons (value-of (car v)) (chain-value (cdr v))))
      (t             nil))))

(defun chain-length (x)
  "Count elements in lazy cons chain X, stopping at first unbound tail."
  (let ((v (value-of x)))
    (cond
      ((null v)      0)
      ((variable? v) 0)
      ((consp v)     (1+ (chain-length (cdr v))))
      (t             0))))

(defun nth-chain (n x)
  "Return the nth element of lazy cons chain X, or NIL if out of range."
  (let ((v (value-of x)))
    (cond
      ((null v)      nil)
      ((variable? v) nil)
      ((consp v)     (if (zerop n)
                         (car v)
                         (nth-chain (- n 1) (cdr v))))
      (t             nil))))

(defun build-chain (r-vars domain-elements)
  (let ((position-vars nil))
    (labels ((build (rvs elems)
               (if (null rvs)
                   nil
                   (let* ((rvar    (car rvs))
                          (elem    (car elems))
                          (result  (make-variable))
                          (tail    (build (cdr rvs) (cdr elems)))
                          (noticer #'(lambda ()
                                       (when (and (bound? rvar) (not (bound? result)))
                                         (if (>= (value-of rvar) 0)
                                             (assert!-equalv result (cons elem tail))
                                             (assert!-equalv result tail))))))
                     (push result position-vars)
                     (attach-noticer! noticer rvar)
                     result))))
      (let ((container (build r-vars domain-elements)))
        (list container position-vars)))))
