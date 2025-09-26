;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;
;;;; Screamer-Plus: A modernized constraint logic programming library for Common Lisp
;;;;
;;;; Screamer-Plus is an extension of constraint propagation in Screamer,
;;;; built upon a fundamental redesign of the core functions `funcallv` and `applyv`
;;;; introduced in version 4.0.1 of Screamer.
;;;;
;;;; This new foundation enables automatic constraint propagation, eliminating the
;;;; need for manual noticers and simplifying function/macro definitions.
;;;; As a result, many of the macros and functions originally found in Screamer-Plus
;;;; (by Simon White) — such as `CARV`, `CDRV`, `IFV`, and others — have been
;;;; entirely rewritten or reimagined with cleaner semantics and greater efficiency.
;;;;
;;;; Some function names and general ideas are inspired by the original Screamer-Plus
;;;; by Simon White, but all code in this package is original unless otherwise noted.”
;;;;
;;;; Contributions, feedback, and extensions are welcome.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Copyright (c) 2025 Paulo Henrique Raposo
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

(eval-when (:compile-toplevel :load-toplevel :execute)

 #-screamer-extensible-types
 (error "Screamer-Plus requires the Screamer feature 'screamer-extensible-types'.
Enable this extension and include the following non number types in *nonnumber-types*:
LIST, CONS, ARRAY, STRING and SYMBOL.")

(unless (subsetp '(LIST CONS ARRAY STRING SYMBOL) screamer::*nonnumber-types*)
 (error "Screamer-Plus requires the following NON NUMBER VARIABLE TYPES:
  LIST, CONS, ARRAY, STRING and SYMBOL.")))

(in-package :screamer)

(eval-when (:compile-toplevel :load-toplevel :execute) (setf *screamer?* t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline objectp)))
(defun-compile-time objectp (var)
  "Determines whether a variable is a standard CLOS object or not"
  (typep var 'standard-object))

(defun-compile-time slot-names-of (obj)
 (mapcar #'(lambda(x) (slot-value x 'CLOS::NAME))
        (clos::class-slots (class-of obj))))

(defun equal-trees? (voice1 voice2)
 (and (om::voice-p voice1)
      (om::voice-p voice2)
      (equal (cadr (om::tree voice1))
             (cadr (om::tree voice2)))))

(defun slot-equal (x y slot)
 (the boolean
  (let ((xb (slot-boundp x slot))
        (yb (slot-boundp y slot)))
    (cond ((and (not xb) (not yb)) t)
          ((not (eql xb yb)) nil)
          (t (let ((x (value-of (slot-value x slot)))
                   (y (value-of (slot-value y slot))))
              (typecase x
                (vector      (and (vectorp y) (equalp x y)))
                (array       (and (arrayp y) (equalp x y)))
                (cons        (and (consp y) (equal x y)))
                (string      (and (stringp y) (string= x y)))
                (hash-table  (and (hash-table-p y) (equalp x y)))
                (number      (and (numberp y) (eql x y)))
                (symbol      (and (symbolp y) (eq x y)))
                (t (eql x y)))))))))

(defun standard-object-equal (x y)
 (declare (standard-object x y))
 (or (equal-trees? x y) ;; OM-VOICE
     (and (typep y (type-of x))
           (every (lambda (slot)
                   (slot-equal x y slot))
                  (slot-names-of x)))))

(defun generic-equal (x y)
;; note: Should find a better name for this.
  "Compares two objects for equality considering their types and structures."
 (the boolean
  (typecase x
    (symbol      (and (symbolp y) (eq x y)))
    (number      (and (numberp y) (eql x y)))
    (vector      (and (vectorp y) (equalp x y)))
    (array       (and (arrayp y) (equalp x y)))
    (cons        (and (consp y) (equal x y)))
    (string      (and (stringp y) (string= x y)))
    (hash-table  (and (hash-table-p y) (equalp x y)))
    (standard-object  (standard-object-equal x y))
    (t           (eql x y)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (setf *screamer?* nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; END OF PATCH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

