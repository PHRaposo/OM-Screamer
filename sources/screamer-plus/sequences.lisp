;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;
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

(in-package :screamer+)

;;; ============================================================================
;;; COUNTING CONSTRAINTS
;;; ============================================================================
;;; Simon White's incremental counting with LOCAL for backtrackable state.
;;; More efficient than the declarative sumv+reifyv approach for large inputs.

(defmacro-compile-time at-leastv (n f &rest x)
  `(at-leastv-internal ,n ,f ,@x))

(defun at-leastv-internal (n f &rest x)
  (declare (integer n))
  (let* ((z (a-booleanv))
         (countup 0)
         (noes 0)
         (shortest (apply #'min (mapcar #'length x)))
         (known-list (make-list shortest :initial-element nil)))
    (declare (integer countup noes shortest))
    (do* ((cdrs x (mapcar #'cdr cdrs))
          (cars (mapcar #'car cdrs) (mapcar #'car cdrs))
          (c 0 (1+ c)))
         ((some #'null cdrs) t)
      (let ((cars cars) (c c) temp)
        (attach-noticer!
         #'(lambda ()
             (when (every #'bound? cars)
               (setq temp (apply f cars))
               (when (bound? temp)
                 (when (null (nth c known-list))
                   (local (setf (nth c known-list) t)))
                 (if (equal (value-of temp) t)
                     (progn
                       (local (incf countup))
                       (when (>= countup n)
                         (assert!-true z)))
                     (local (incf noes)))
                 ;; Not enough unknowns left
                 (when (and (< (- shortest noes) n)
                            (not (known?-false z)))
                   (assert!-false z))
                 ;; Short cut: exactly enough unknowns remain
                 (when (and (= (- shortest noes) n)
                            (known?-true z))
                   (do* ((cdrs2 x (mapcar #'cdr cdrs2))
                         (cars2 (mapcar #'car cdrs2) (mapcar #'car cdrs2))
                         (q 0 (1+ q)))
                        ((some #'null cdrs2) t)
                     (when (null (nth q known-list))
                       (assert! (apply f cars2))))))))
         cars)))
    ;; z noticer: when z becomes true and remaining unknowns = n
    (attach-noticer!
     #'(lambda ()
         (when (and (= (- shortest noes) n)
                    (known?-true z))
           (do* ((cdrs x (mapcar #'cdr cdrs))
                 (cars (mapcar #'car cdrs) (mapcar #'car cdrs))
                 (q 0 (1+ q)))
                ((some #'null cdrs) t)
             (when (null (nth q known-list))
               (assert! (apply f cars))))))
     z)
    z))

(defmacro-compile-time at-mostv (n f &rest x)
  `(at-mostv-internal ,n ,f ,@x))

(defun at-mostv-internal (n f &rest x)
  (declare (integer n))
  (let* ((z (a-booleanv))
         (countup 0)
         (noes 0)
         (shortest (apply #'min (mapcar #'length x)))
         (known-list (make-list shortest :initial-element nil)))
    (declare (integer countup noes shortest))
    (do* ((cdrs x (mapcar #'cdr cdrs))
          (cars (mapcar #'car cdrs) (mapcar #'car cdrs))
          (c 0 (1+ c)))
         ((some #'null cdrs) t)
      (let ((cars cars) (c c) temp)
        (attach-noticer!
         #'(lambda ()
             (when (every #'bound? cars)
               (setq temp (apply f cars))
               (when (bound? temp)
                 (when (null (nth c known-list))
                   (local (setf (nth c known-list) t)))
                 (if (equal (value-of temp) t)
                     (progn
                       (local (setq countup (1+ countup)))
                       (when (> countup n)
                         (assert! (notv z))))
                     (local (setq noes (1+ noes))))
                 ;; Enough noes: at-most is guaranteed
                 (when (and (<= (- shortest noes) n)
                            (not (known?-true z)))
                   (assert! z))
                 ;; Short cut: reached n, force remaining false
                 (when (and (= countup n)
                            (known?-true z))
                   (do* ((cdrs2 x (mapcar #'cdr cdrs2))
                         (cars2 (mapcar #'car cdrs2) (mapcar #'car cdrs2))
                         (q 0 (1+ q)))
                        ((some #'null cdrs2) t)
                     (when (null (nth q known-list))
                       (assert! (notv (apply f cars2)))))))))
         cars)))
    (attach-noticer!
     #'(lambda ()
         (when (bound? z)
           (when (and (= countup n)
                      (known?-true z))
             (do* ((cdrs x (mapcar #'cdr cdrs))
                   (cars (mapcar #'car cdrs) (mapcar #'car cdrs))
                   (q 0 (1+ q)))
                  ((some #'null cdrs) t)
               (when (null (nth q known-list))
                 (assert! (notv (apply f cars))))))))
     z)
    z))

(defmacro-compile-time exactlyv (n f &rest x)
  `(exactlyv-internal ,n ,f ,@x))

(defun exactlyv-internal (n f &rest x)
  (declare (integer n))
  (let* ((z (a-booleanv))
         (countup 0)
         (noes 0)
         (shortest (apply #'min (mapcar #'length x)))
         (known-list (make-list shortest :initial-element nil)))
    (do* ((cdrs x (mapcar #'cdr cdrs))
          (cars (mapcar #'car cdrs) (mapcar #'car cdrs))
          (c 0 (1+ c)))
         ((some #'null cdrs) t)
      (let ((cars cars) (c c) temp)
        (attach-noticer!
         #'(lambda ()
             (when (every #'bound? cars)
               (setq temp (apply f cars))
               (when (bound? temp)
                 (when (null (nth c known-list))
                   (local (setf (nth c known-list) t)))
                 (if (equal (value-of temp) t)
                     (progn
                       (local (setq countup (1+ countup)))
                       ;; Reached n: assert true
                       (when (>= countup n) (assert!-true z))
                       ;; Exceeded n: assert false
                       (when (> countup n) (assert!-false z)))
                     (local (setq noes (1+ noes))))
                 ;; Not enough unknowns for at-least
                 (when (and (< (- shortest noes) n)
                            (not (known?-false z)))
                   (assert!-false z))
                 ;; Enough noes: at-most guaranteed
                 (when (and (<= (- shortest noes) n)
                            (not (known?-true z)))
                   (assert!-true z))
                 ;; Short cut: exactly enough unknowns, force true
                 (when (and (= (- shortest noes) n)
                            (known?-true z))
                   (do* ((cdrs2 x (mapcar #'cdr cdrs2))
                         (cars2 (mapcar #'car cdrs2) (mapcar #'car cdrs2))
                         (q 0 (1+ q)))
                        ((some #'null cdrs2) t)
                     (when (null (nth q known-list))
                       (assert! (apply f cars2)))))
                 ;; Short cut: reached n, force remaining false
                 (when (and (>= countup n)
                            (known?-true z))
                   (do* ((cdrs2 x (mapcar #'cdr cdrs2))
                         (cars2 (mapcar #'car cdrs2) (mapcar #'car cdrs2))
                         (q 0 (1+ q)))
                        ((some #'null cdrs2) t)
                     (when (null (nth q known-list))
                       (assert! (notv (apply f cars2)))))))))
         cars)))
    (attach-noticer!
     #'(lambda ()
         (when (bound? z)
           (when (and (= countup n)
                      (known?-true z))
             (do* ((cdrs x (mapcar #'cdr cdrs))
                   (cars (mapcar #'car cdrs) (mapcar #'car cdrs))
                   (q 0 (1+ q)))
                  ((some #'null cdrs) t)
               (when (null (nth q known-list))
                 (assert! (notv (apply f cars))))))))
     z)
    z))

;;; ============================================================================
;;; QUANTIFIERS
;;; ============================================================================
;;; Based on Simon White's originals with named rules and cleaner formatting.
;;; These use attach-noticer! with bidirectional propagation:
;;; forward (list -> z) and backward (z -> list elements).

(defun everyv (f v)
  "Boolean variable: T iff F applied to every element of V is true."
  (let ((z (a-booleanv))
        (v (value-of v)))
    (attach-noticer!
     #'(lambda ()
         (when (bound? v)
           (let ((val nil))
             (dolist (x (value-of v))
               (push (funcall f x) val))
             (assert!-equalv z (apply #'andv val)))))
     v)
    (attach-noticer!
     #'(lambda ()
         (when (and (known?-true z) (bound? v))
           (dolist (x (value-of v))
             (assert! (funcall f x)))))
     z)
    z))

(defun somev (f v)
  "Boolean variable: T iff F applied to some element of V is true."
  (let ((z (a-booleanv))
        (v (value-of v)))
    (attach-noticer!
     #'(lambda ()
         (when (bound? v)
           (let ((disj nil))
             (dolist (x (value-of v))
               (push (funcall f x) disj))
             (assert! (equalv z (apply #'orv disj))))))
     v)
    (attach-noticer!
     #'(lambda ()
         (when (and (known?-false z) (bound? v))
           (dolist (x (value-of v))
             (assert! (notv (funcall f x))))))
     z)
    z))

(defun noteveryv (f v)
  "Boolean variable: T iff F applied to some element of V is false."
  (let ((z (a-booleanv))
        (v (value-of v)))
    (attach-noticer!
     #'(lambda ()
         (when (bound? v)
           (let ((conj nil))
             (dolist (x (value-of v))
               (push (funcall f x) conj))
             (assert! (equalv (notv z) (apply #'andv conj))))))
     v)
    (attach-noticer!
     #'(lambda ()
         (when (and (known?-false z) (bound? v))
           (dolist (x (value-of v))
             (assert! (funcall f x)))))
     z)
    z))

(defun notanyv (f v)
  "Boolean variable: T iff F applied to every element of V is false."
  (let ((z (a-booleanv))
        (v (value-of v)))
    (attach-noticer!
     #'(lambda ()
         (when (bound? v)
           (let ((val nil))
             (dolist (x (value-of v))
               (push (notv (funcall f x)) val))
             (assert! (equalv z (apply #'andv val))))))
     v)
    (attach-noticer!
     #'(lambda ()
         (when (and (known?-true z) (bound? v))
           (dolist (x (value-of v))
             (assert! (notv (funcall f x))))))
     z)
    z))

(defun subseqv (sequence start &optional end)
(let ((sequence (value-of sequence))
      (start (value-of start))
      (end (value-of end)))
 (if (or (variable? sequence) (variable? start) (variable? end))
         (let ((z (applyv #'subseq (append (list sequence start)
                                           (when end (list end))))))
           z)
         (apply #'subseq (append (list sequence start)
                                 (when end (list end)))))))

(defun countv (item sequence &key from-end start end key test test-not)
(let* ((sequence (value-of sequence))
       (arguments (append (list item sequence)
                          (when from-end (list :from-end from-end))
                          (when start (list :start start))
                          (when end (list :end end))
                          (when key (list :key key))
                          (when test (list :test test))
                          (when test-not (list :test-not test-not)))))
    (when (some #'identity (mapcar #'variable? (cdr arguments)))
        (error "The current implementation does not allow any optional argument~%~
        of COUNTV to be an unbound variable."))
    (if (deep-bound? arguments)
        (apply #'count arguments)
        (let ((z (applyv #'count arguments)))
         z))))

(defun remove-duplicatesv (sequence &key from-end test test-not start end key)
(let* ((sequence (value-of sequence))
       (arguments (append (list sequence)
                          (when from-end (list :from-end from-end))
                          (when test (list :test test))
                          (when test-not (list :test-not test-not))
                          (when start (list :start start))
                          (when end (list :end end))
                          (when key (list :key key)))))
    (when (some #'identity (mapcar #'variable? (cdr arguments)))
        (error "The current implementation does not allow any optional argument~%~
        of REMOVE-DUPLICATESV to be an unbound variable."))
    (if (deep-bound? sequence)
        (apply #'remove-duplicates arguments)
        (let ((z (applyv #'remove-duplicates arguments)))
         (cond ((variable? sequence)
                (assert! (everyv (lambda (el) (memberv el z)) sequence)))
               (t (map nil (lambda (el) (assert! (memberv el z))) sequence)))
         (value-of z)))))

