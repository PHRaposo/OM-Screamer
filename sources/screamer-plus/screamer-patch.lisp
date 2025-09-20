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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline objectp)))
(defun-compile-time objectp (var)
  "Determines whether a variable is a standard CLOS object or not"
  (typep var 'standard-object))

(defun-compile-time slot-names-of (obj)
 (mapcar #'(lambda(x) (slot-value x 'CLOS::NAME))
        (clos::class-slots (class-of obj))))

(defun-compile-time copy-standard-object (obj)
  "Shallow copy of a CLOS object (standard-object)."
  (let* ((class (class-of obj))
         (copy (make-instance class)))
    (dolist (slot (slot-names-of obj))
     (when (slot-boundp obj slot)
      (setf (slot-value copy slot) (slot-value obj slot))))
    copy))

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
  (and (typep y (class-of x))
       (every (lambda (slot)
               (slot-equal x y slot))
              (slot-names-of x))))

(defun generic-equal (x y)
;; note: Should find a better name for this.
  "Compares two objects for equality considering their types and structures."
 (the boolean
  (typecase x
    (vector      (and (vectorp y) (equalp x y)))
    (array       (and (arrayp y) (equalp x y)))
    (cons        (and (consp y) (equal x y)))
    (string      (and (stringp y) (string= x y)))
    (hash-table  (and (hash-table-p y) (equalp x y)))
    (number      (and (numberp y) (eql x y)))
    (symbol      (and (symbolp y) (eq x y)))
    (standard-object (standard-object-equal x y))
    (t           (eql x y)))))

 (defun variables-in (x)
  (the list
   (let ((x (value-of x)))
       ;; Note: (value-of x) is required to dereference a variable 
       ;; whose value has been unified (using EQUALV) with other 
       ;; data types, such as cons cells, lists, and so on.
       (typecase x
         (cons (append (variables-in (car x))
                       (variables-in (cdr x))))
         (string nil)
         ;; (simple-vector (apply #'append (map 'list #'variables-in x)))
         (sequence (apply #'append (map 'list #'variables-in x)))
         (array (flet ((mappend-arr (arr f)
                         (let (coll)
                           (dotimes (idx (array-total-size arr))
                             (alexandria::appendf coll (funcall f (row-major-aref arr idx))))
                           coll)))
                  (mappend-arr x #'variables-in)))
         (hash-table (let (coll)
                       (maphash (lambda (k v)
                                  (declare (ignore k))
                                  (alexandria::appendf coll (variables-in v)))
                                x)
                       coll))
         (standard-object (let (coll)
                           (dolist (slot (slot-names-of x))
                            (when (slot-boundp x slot)
                               (alexandria:appendf coll (variables-in (slot-value x slot)))))
                           coll))
         (variable (list x))
         (otherwise nil)))))

(defun apply-substitution (x)
  "If X is a SEQUENCE or HASH-TABLE, returns a freshly consed
copy of the tree with all variables dereferenced.
Otherwise returns the value of X."
  (let ((x (value-of x)))
    (etypecase x
      (cons (if (null (cdr (last x)))
                ;; If terminates with nil (ie normal list)
                ;; use mapcar to not consume stack
                (mapcar #'apply-substitution x)
                ;; Otherwise recurse on the car and cdr
                (cons (apply-substitution (car x))
                      (apply-substitution (cdr x)))))
      (string x)
      (simple-vector (map 'vector #'apply-substitution x))
      (sequence (let ((copy (copy-seq x)))
                  (dotimes (idx (length x))
                    (setf (elt copy idx)
                          (apply-substitution (elt x idx))))
                  copy))
      (array (let ((arr (alexandria::copy-array x)))
               (dotimes (idx (array-total-size arr))
                 (setf (row-major-aref arr idx)
                       (apply-substitution (row-major-aref arr idx))))
               arr))
      (hash-table
       (let ((x (alexandria::copy-hash-table x)))
         (maphash (lambda (k v) (setf (gethash k x) (apply-substitution v))) x)
         x))
       (standard-object (let ((copy (copy-standard-object x)))
                              (dolist (slot (slot-names-of x))
                                (when (slot-boundp x slot)
                                    (setf (slot-value copy slot)
                                          (apply-substitution (slot-value x slot)))))
                              copy))
      (t x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; END OF PATCH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

