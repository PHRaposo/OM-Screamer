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

(in-package :screamer+)

(defun slot-valuev (objvar slotname)
  (if (and (bound? objvar)
           (bound? slotname))
      (slot-value (value-of objvar) (value-of slotname))
      (let ((z (funcallv #'slot-value objvar slotname)))
       z)))

(defun make-instancev (class &rest initargs)
 (if (and (bound? class)
          (every #'bound? initargs))
      (apply #'make-instance (cons class initargs))
      (let ((z (applyv #'make-instance (cons class initargs))))
       z)))

(defun classpv (obj name)
 (if (and (bound? obj)
          (bound? name))
      (eq (class-name (class-of (value-of obj))) (value-of name))
      (let ((z (a-booleanv)))
        (assert! (eqv z (equalv (funcallv #'(lambda(x)
                                            (class-name (class-of x))) (value-of obj))
                                (value-of name))))
        z)))

(defun class-namev (obj)
"Redesigned from original Screamer Plus, by Simon White.

If OBJ is bound, returns its class name.

If OBJ is unbound, returns a variable that will be constrained
to the class name of OBJ once it becomes bound."
 (if (bound? obj)
     (class-name (value-of obj))
      (let ((z (make-variable)))  
      (attach-noticer!
       #'(lambda()
           (when (and (not (bound? obj))
                      (screamer::enumerated-domain-p obj))
              (assert! (memberv
                        z
                       (mapcar #'(lambda (x)
                              (and (objectp x) (class-name x)))
                       (variable-enumerated-domain obj)))))
           (when (bound? obj)
              (make-equal z (class-name (value-of obj)))))
       obj) 
      z)))

(defun class-ofv (obj)
"Redesigned from original Screamer Plus, by Simon White.

If OBJ is bound, returns its class.

If OBJ is unbound, returns a variable that will be constrained
to the class of OBJ once it becomes bound."
 (if (bound? obj)
     (class-of (value-of obj))
      (let ((z (make-variable)))  
      (attach-noticer!
       #'(lambda()
           (when (and (not (bound? obj))
                      (screamer::enumerated-domain-p obj))
              (assert! (memberv
                        z
                       (mapcar #'(lambda (x)
                              (and (objectp x) (class-of x)))
                       (variable-enumerated-domain obj)))))
           (when (bound? obj)
              (make-equal z (class-of (value-of obj)))))
       obj) 
      z)))

(defun slot-exists-pv (obj slotname)
 (if (and (bound? obj)
          (bound? slotname))
     (if (slot-exists-p (value-of obj) (value-of slotname)) t)
     (let ((z (funcallv #'(lambda (o n) (if (slot-exists-p o n) t)) (value-of obj) (value-of slotname))))
      z)))

;; IN PROGRESS
;; note: the following functions was not tested yet

(defun slot-names-ofv (obj)
 (if (bound? obj)
     (slot-names-of (value-of obj))
     (let ((z (funcallv #'slot-names-of obj)))
      z)))

(defun slot-boundpv (obj slotname)
 (if (and (bound? obj)
          (bound? slotname))
     (slot-boundp (value-of obj) (value-of slotname))
     (let ((z (funcallv #'slot-boundp (value-of obj) (value-of slotname))))
      z)))

(defun reconcile-objectsv (objvar1 objvar2)
  (let ((slots1 (slot-names-ofv objvar1))
        (slots2 (slot-names-ofv objvar2)))
    (assert! (equalv slots1 slots2))
    (everyv (lambda (s)
              (equalv (slot-valuev objvar1 s)
                      (slot-valuev objvar2 s)))
            slots1)))

(defun reconcile (var1 var2)
  (cond 
   ((equal var1 var2) t)
   ((and (objectp var1) (objectp var2))
    (reconcile-objectsv var1 var2))
   (t (reconcile-objectsv var1 var2))))
