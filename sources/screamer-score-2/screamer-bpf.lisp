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
;;;; SCREAMER-BPF / SCREAMER-BPF-LIB
;;;;
;;;; Lightweight BPF containers used by SCREAMER-SCORE-2 profile constraints.
;;;; Independent from OM bpf / bpf-lib classes; coerce-to-screamer-bpf and
;;;; coerce-to-screamer-bpf-lib accept either screamer-* objects or OM objects
;;;; (OM bpfs are detected via OM:BPF-P / OM:BPF-LIST when OM is loaded).
;;;;
;;;; Storage: two parallel lists X-POINTS and Y-POINTS (no point-list, no
;;;; decimals, no UI fields). X-points must be strictly ascending; lengths
;;;; must match. Validation runs in INITIALIZE-INSTANCE :AFTER.
;;;; ===========================================================================


(defclass screamer-bpf ()
  ((x-points :initarg :x-points :accessor sbpf-x-points :type list)
   (y-points :initarg :y-points :accessor sbpf-y-points :type list))
  (:documentation
   "Break-point function for SCREAMER-SCORE-2. X-POINTS strictly ascending,
Y-POINTS same length. No interpolation precision setting (results returned as
real numbers; rounding is the caller's choice)."))


(defclass screamer-bpf-lib ()
  ((bpf-list :initarg :bpf-list :accessor sbpf-list :type list))
  (:documentation
   "Container for a list of SCREAMER-BPF instances. Following OM bpf-lib
convention, individual BPFs may have different x-ranges and lengths;
sampling normalises per-BPF."))


(defmethod screamer-bpf-p ((self screamer-bpf)) t)
(defmethod screamer-bpf-p ((self t))            nil)

(defmethod screamer-bpf-lib-p ((self screamer-bpf-lib)) t)
(defmethod screamer-bpf-lib-p ((self t))                nil)


(defmethod initialize-instance :after ((self screamer-bpf) &key)
  (let ((xs (sbpf-x-points self))
        (ys (sbpf-y-points self)))
    (unless (and (listp xs) (listp ys))
      (error "SCREAMER-BPF: X-POINTS and Y-POINTS must be lists. Got ~S, ~S." xs ys))
    (unless (= (length xs) (length ys))
      (error "SCREAMER-BPF: X-POINTS (~A) and Y-POINTS (~A) length mismatch."
             (length xs) (length ys)))
    (when (< (length xs) 2)
      (error "SCREAMER-BPF: requires at least 2 points. Got ~A." (length xs)))
    (loop for (a b) on xs while b
          unless (< a b)
            do (error "SCREAMER-BPF: X-POINTS must be strictly ascending. ~
                       Found ~A followed by ~A in ~S." a b xs))))


(defmethod initialize-instance :after ((self screamer-bpf-lib) &key)
  (let ((bpfs (sbpf-list self)))
    (unless (and (listp bpfs) bpfs)
      (error "SCREAMER-BPF-LIB: BPF-LIST must be a non-empty list of SCREAMER-BPF instances. Got ~S." bpfs))
    (dolist (b bpfs)
      (unless (screamer-bpf-p b)
        (error "SCREAMER-BPF-LIB: BPF-LIST element ~S is not a SCREAMER-BPF." b)))))


;;;; ===========================================================================
;;;; CONSTRUCTORS
;;;; ===========================================================================

(defun mk-screamer-bpf (x-points y-points)
  "Construct a SCREAMER-BPF from parallel X-POINTS and Y-POINTS lists.
X-POINTS must be strictly ascending; both lists must be the same length and
contain at least 2 elements."
  (make-instance 'screamer-bpf :x-points x-points :y-points y-points))


(defun mk-screamer-bpf-lib (&rest bpfs)
  "Construct a SCREAMER-BPF-LIB from one or more SCREAMER-BPF instances.
Each argument may also be an OM BPF; it will be coerced via
COERCE-TO-SCREAMER-BPF."
  (make-instance 'screamer-bpf-lib
                 :bpf-list (mapcar #'coerce-to-screamer-bpf bpfs)))


;;;; ===========================================================================
;;;; LINEAR INTERPOLATION (matches OM:LINEAR-INTERPOL / OM:INTERPOLE)
;;;; ===========================================================================

(defun sbpf-linear-interpol (x1 x2 y1 y2 x)
  "Linear interpolation: y at X on segment ((X1 Y1)-(X2 Y2)). Degenerate
segment X1=X2 returns Y1. Matches the formula in
OM:basicproject/functions/functions.lisp:LINEAR-INTERPOL."
  (if (= x1 x2) y1
      (+ y1
         (* (- y2 y1)
            (/ (- x x1)
               (- x2 x1))))))


(defun sbpf-interpole (xs ys x-min x-max n)
  "Sample N y-values on the BPF defined by parallel XS / YS, between X-MIN
and X-MAX. Step = (x-max - x-min) / (n - 1) for N >= 2. For N = 1 returns
the value at the midpoint of [x-min, x-max]. Walks segments in order.

Behaviour matches OM:INTERPOLE so output is bit-equivalent for valid
inputs (xs strictly ascending, x-min/x-max within range, n >= 1)."
  (cond
    ((= n 1)
     (let ((x-mid (+ x-min (/ (- x-max x-min) 2))))
       (list (sbpf-interpole-one xs ys x-mid))))
    (t
     (let ((step (/ (- x-max x-min) (1- (coerce n 'double-float)))))
       (loop with x  = (pop xs)
             with xx = (pop xs)
             with y  = (pop ys)
             with yy = (pop ys)
             with x-index = x-min
             with s-index = 0
             while (and xx (< s-index n))
             if (and (>= x-index x) (<= x-index xx))
               collect (sbpf-linear-interpol x xx y yy x-index)
               and do (setf x-index (+ x-min (* (incf s-index) step)))
             else
               do (setf x  xx
                        xx (pop xs)
                        y  yy
                        yy (pop ys)))))))


(defun sbpf-interpole-one (xs ys x)
  "Single-point linear interpolation. Locates the segment of (XS,YS)
containing X and returns the linear-interpolated y."
  (loop for (x1 x2) on xs
        for (y1 y2) on ys
        while x2
        when (and (>= x x1) (<= x x2))
          return (sbpf-linear-interpol x1 x2 y1 y2 x)
        finally (return (if (<= x (first xs))
                            (first ys)
                            (car (last ys))))))


;;;; ===========================================================================
;;;; SAMPLING
;;;; ===========================================================================

(defmethod sbpf-sample ((bpf screamer-bpf) (n integer) &optional xmin xmax)
  "Return a list of N y-values sampled from BPF between XMIN and XMAX
(defaults: BPF's own x-range). Linear interpolation between adjacent
points. N must be a positive integer."
  (when (< n 1)
    (error "SBPF-SAMPLE: N must be >= 1. Got ~A." n))
  (let* ((xs (sbpf-x-points bpf))
         (ys (sbpf-y-points bpf))
         (x-min (or xmin (first xs)))
         (x-max (or xmax (car (last xs)))))
    (when (< x-min (first xs))
      (setf x-min (first xs)))
    (when (> x-max (car (last xs)))
      (setf x-max (car (last xs))))
    (sbpf-interpole xs ys x-min x-max n)))


(defmethod sbpf-sample ((lib screamer-bpf-lib) (n integer) &optional xmin xmax)
  "Sample each BPF in LIB independently. Returns a list of N-element y-lists,
one sub-list per BPF (in declaration order)."
  (mapcar #'(lambda (b) (sbpf-sample b n xmin xmax))
          (sbpf-list lib)))


(defun sbpf-sample-by-index (bpf n)
  "Classic-style sampling: treat y-points as an indexed sequence with implicit
x = (0 1 ... len-y - 1) and return N evenly-spaced y-samples. Ignores BPF's
declared x-points entirely. Matches the classic SCREAMER-SCORE behaviour
of (om-sample (y-points bpf) N) on the y-list."
  (when (< n 1)
    (error "SBPF-SAMPLE-BY-INDEX: N must be >= 1. Got ~A." n))
  (let* ((ys     (sbpf-y-points bpf))
         (len-y  (length ys))
         (xs-impl(loop for i below len-y collect i)))
    (sbpf-interpole xs-impl ys 0 (1- len-y) n)))


(defun sbpf-sample-at-positions (bpf positions)
  "Sample BPF at each X position in POSITIONS (one y-value per position).
POSITIONS must lie within the BPF's x-range; out-of-range values are clamped
to the nearest endpoint y-value."
  (let* ((xs (sbpf-x-points bpf))
         (ys (sbpf-y-points bpf)))
    (mapcar (lambda (x) (sbpf-interpole-one xs ys x))
            positions)))


;;;; ===========================================================================
;;;; COERCION
;;;;
;;;; Accept input from user as: screamer-bpf, screamer-bpf-lib, OM bpf,
;;;; OM bpf-lib, or a 2-element list (x-points y-points). The conversion
;;;; is one-shot at constraint-creation time.
;;;; ===========================================================================

(defgeneric coerce-to-screamer-bpf (obj)
  (:documentation
   "Coerce OBJ to a SCREAMER-BPF. Accepts SCREAMER-BPF (returned as-is) or
an OM BPF (read via OM:X-POINTS / OM:Y-POINTS). Errors on other types."))


(defmethod coerce-to-screamer-bpf ((obj screamer-bpf))
  obj)


(defmethod coerce-to-screamer-bpf ((obj t))
  (cond
    ;; OM bpf detection: OM:BPF-P returns T for OM bpf instances.
    ((and (find-package :om)
          (find-symbol "BPF-P" :om)
          (fboundp (find-symbol "BPF-P" :om))
          (funcall (find-symbol "BPF-P" :om) obj))
     (mk-screamer-bpf
       (funcall (find-symbol "X-POINTS" :om) obj)
       (funcall (find-symbol "Y-POINTS" :om) obj)))
    (t
     (error "COERCE-TO-SCREAMER-BPF: cannot coerce ~S to SCREAMER-BPF.
Accepted: SCREAMER-BPF instance or OM BPF instance." obj))))


(defgeneric coerce-to-screamer-bpf-lib (obj)
  (:documentation
   "Coerce OBJ to a SCREAMER-BPF-LIB. Accepts SCREAMER-BPF-LIB (returned
as-is), SCREAMER-BPF (wrapped as 1-element lib), OM BPF-LIB, OM BPF
(wrapped as 1-element lib)."))


(defmethod coerce-to-screamer-bpf-lib ((obj screamer-bpf-lib))
  obj)


(defmethod coerce-to-screamer-bpf-lib ((obj screamer-bpf))
  (make-instance 'screamer-bpf-lib :bpf-list (list obj)))


(defmethod coerce-to-screamer-bpf-lib ((obj t))
  (cond
    ;; OM bpf-lib detection
    ((and (find-package :om)
          (find-class (find-symbol "BPF-LIB" :om) nil)
          (typep obj (find-class (find-symbol "BPF-LIB" :om))))
     (let ((om-list (funcall (find-symbol "BPF-LIST" :om) obj)))
       (make-instance 'screamer-bpf-lib
                      :bpf-list (mapcar #'coerce-to-screamer-bpf om-list))))
    ;; OM bpf as single -> wrap
    ((and (find-package :om)
          (find-symbol "BPF-P" :om)
          (fboundp (find-symbol "BPF-P" :om))
          (funcall (find-symbol "BPF-P" :om) obj))
     (make-instance 'screamer-bpf-lib
                    :bpf-list (list (coerce-to-screamer-bpf obj))))
    (t
     (error "COERCE-TO-SCREAMER-BPF-LIB: cannot coerce ~S to SCREAMER-BPF-LIB.
Accepted: SCREAMER-BPF-LIB, SCREAMER-BPF, OM BPF-LIB, or OM BPF." obj))))
