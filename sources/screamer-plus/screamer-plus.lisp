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

(defvar *screamer+-version* "0.2.0" ;(asdf:component-version (asdf:find-system :screamer-plus))
  "The version of Screamer-Plus which is loaded.")

(defmacro define-screamer-plus-package (defined-package-name &body options)
  "Convenience wrapper around DEFPACKAGE. Passes its argument directly
to DEFPACKAGE, and automatically injects two additional options:

    \(:shadowing-import-from :screamer :defun :multiple-value-bind :y-or-n-p)
    \(:use :cl :screamer :screamer+)"
  `(defpackage ,defined-package-name
     ,@options
     (:shadowing-import-from :screamer :defun :multiple-value-bind :y-or-n-p)
     (:use :cl :screamer :screamer+)))

(defvar *maximum-domain-size* 100
 "Specifies the maximum number of elements permitted in variable enumerated domain.")

(defun slot-names-of (obj)
 (mapcar #'(lambda(x) (slot-value x 'CLOS::NAME))
        (clos::class-slots (class-of obj))))

(defun objectp (var)
  "Determines whether a variable is a standard CLOS object or not"
  (typep var 'standard-object))

(defun eqv (x y)
  "Original from Screamer-Plus, by Simon White.
  A simple version of eqv for atoms. PROPAGATION PROPERTIES: as for funcallv."
  (funcallv #'eq x y))

(defun impliesv (p q)
  "Original from Screamer-Plus, by Simon White.
  This was not included in the standard SCREAMER distribution.
  The following generates the truth table for implication:
  > (setq p (a-booleanv))
  [3558 Boolean]
  > (setq q (a-booleanv))
  [3559 Boolean]
  > (setq r (a-booleanv))
  [3560 Boolean]
  > (assert! (equalv r (impliesv p q)))
  NIL]
  > (all-values (solution (list p '=> q 'is r) (static-ordering #'linear-force)))
  ((T => T IS T) (T => NIL IS NIL) (NIL => T IS T) (NIL => NIL IS T))
  PROPAGATION PROPERTIES: as for other logical functions."
  (orv (notv p) q))

(defmacro-compile-time carefully (&body forms)
 "Redesigned from original Screamer-Plus.
  Evaluates FORMS, returning its value or NIL on error, emitting a warning."
  `(handler-case
       (progn ,@forms)
     (error (e)
       (warn "~s failed: ~a" ',forms e)
       nil)))

(defmacro-compile-time carefully-evaluate (form)
  "Redesigned from original Screamer-Plus.
   Evaluates FORM, returning its value or NIL on error, emitting a warning."
  `(handler-case
       ,form
     (error (e)
       (warn "~s failed: ~a" ',form e)
       nil)))

(defmacro-compile-time ifv (condition then &optional else)
  "If CONDITION is KNOWN? to be NOT EQUAL NIL, THEN is returned. Note that the
 value of CONDITION does not need to be a BOOLEAN.

  If CONDITION is KNOWN? to be false, ELSE is returned.

  Otherwise returns a variable Z which is constrained to be the value of THEN
  if CONDITION later becomes bound to not NIL, or the value of ELSE if CONDITION
  later becomes bound to NIL.

  All arguments (CONDITION THEN ELSE) are evaluated at runtime, so recursive calls
  can cause infinite loops."
  (let ((g-cond (gensym "COND"))
        (g-then (gensym "THEN"))
        (g-else (gensym "ELSE")))
    `(let ((,g-cond ,condition)
           (,g-then ,then)
           (,g-else ,else))
       (cond ((known? (notv (equalv ,g-cond nil))) (value-of ,g-then))
             ((known?-false ,g-cond) (value-of ,g-else))
             (t (funcallv (lambda (cond then else)
                            (if cond then else))
                          ,g-cond ,g-then ,g-else))))))

(defmacro-compile-time ifv-rec (condition then &optional else)
 "Redesigned from original Screamer-Plus.

 This version keeps the semantics of original IFV and allows recursive calls.
 
 If condition is BOUND? and not NIL, a variable Z is returned which is
 constrained to be the value of THEN.
 
 If CONDITION is BOUND? and NIL, Z is constrained to be the value of ELSE.
 
 If CONDITION is not BOUND?, a variable Z is returned which is constrained
 to be the value of THEN if CONDITION later becomes bound to not NIL, or
 the value of ELSE if CONDITION later becomes bound to NIL.
 "
  (let ((gcond (gensym "COND"))
        (z (gensym "Z")))
    `(let ((,gcond ,condition))
      (if (known? (notv (equalv ,gcond nil)))
          ,then
          (let ((,z (make-variable)))
            (attach-noticer!
             #'(lambda ()
                (when (deep-bound? ,gcond)
                 (assert!-equalv ,z (funcallv (lambda (condition then else)
                                               (if condition then else))
                                                ,gcond ,then ,else))))
              ,gcond)
             (attach-noticer! #'(lambda nil ) ,z :dependencies (list ,gcond))
            ,z)))))

 (defmacro-compile-time condv (&rest clauses)
  "Screamer version of Common LISP COND macro."
  (let* ((last-clause (car (last clauses)))
         (final-clauses (if (and (consp last-clause) (eq (first last-clause) 't))
                             clauses
                             (append clauses '((t nil)))))
          (gsyms (mapcar (lambda (_) (declare (ignorable _)) (gensym "CONDV")) final-clauses))
          (bodies (mapcar (lambda (clause)
                            (let ((body (cdr clause)))
                              (if (or (null body)
                                      (and (= (length body) 1)
                                            (null (first body))))
                                   nil
                                 `(progn ,@body))))
                          final-clauses))
          (known-cond-clauses
            (mapcar (lambda (gsym clause body)
                     (unless (and (consp clause) (eq (first clause) 't))
                      `((known?-true ,gsym) ,body)))
                    gsyms final-clauses bodies))
          (funcallv-cond-clauses
            (mapcar (lambda (gsym body)
                      `(,gsym ,body))
                    gsyms bodies)))
      `(let ,(mapcar #'list gsyms (mapcar #'first final-clauses))
        (cond
          ,@(remove nil known-cond-clauses)
          (t (funcallv
              (lambda ,gsyms
                (cond ,@funcallv-cond-clauses))
              ,@gsyms))))))

(defun formatv (destination control-string &rest args)
"Redesigned from original Screamer-Plus."
 (applyv #'format (apply #'list destination control-string args)))

(defun reifyv (x)
"Redesigned from original Screamer-Plus."
(let ((x (value-of x)))
 (cond ((known?-true x) 1)
       ((known?-false x) 0)
       (t (funcallv #'(lambda (v) (if (eq v t) 1 0)) x)))))

(defun funcallinv (f inverse &rest el)
"Redesigned from original Screamer-Plus.

This function returns a variable Z such that (funcallv f EL) = Z and
(funcallv inverse Z) = EL, if INVERSE is provided. 

If INVERSE is nil, it behaves like funcallv.
"
  (let ((z (applyv f (value-of el))))
   (when inverse (assert! (equalv (value-of el) (list (funcallv inverse z)))))
    z))

(defun constraint-fn (f)                      
;; needs work: make CONSTRAINT-FN work at compile-time
"Redesigned from original Screamer-Plus, by Simon White.
This function takes a function which works only on bound arguments, and
returns a similar function which works with either bound arguments or
arguments which are constraint variables.
If a function already exists with the same name as the supplied function
appended with the suffix 'v', then this function is returned. Otherwise
a lambda function is constructed and returned.
"
 (let ((fn-name (third (multiple-value-list (function-lambda-expression f))))
       (cfn-name nil))
    (setf cfn-name (read-from-string (format nil "~av" fn-name) nil nil))
    (if (and (screamer::valid-function-name? fn-name)
             (fboundp cfn-name))
         (symbol-function cfn-name)
         (alexandria::curry (lambda (&rest args)
                             (value-of (applyv (value-of f) args)))))))

(defun sumv (listv)
 (let ((x (value-of listv)))
 (typecase x 
  (list (if (some #'variable? x)
            (apply #'+v x)
            (apply #'+ x)))
 (screamer::variable (funcallv #'(lambda (lst) (apply #'+ lst)) listv))
 (otherwise (error "The argument for SUMV must be a list or a LISTV" x)))))

;; compatibility note
;; This section contains or functions from original Screamer-Plus or
;; adaptations of them, keeped here for backward compatibility.
;; Some of them are deprecated.

(defmacro-compile-time make-equal (var value &optional (retval '(fail)))
"Redesigned from original Screamer-Plus."
  `(cond ((possibly? (equalv ,var ,value))
          (assert! (equalv ,var ,value))
           (values ,var))
         ((possibly? (equalv ,(deep-value-of var) ,(deep-value-of value)))
          (assert! (equalv ,(deep-value-of var) ,(deep-value-of value)))
          (values ,var))
        (t (warn "(make-equal ~s ~s) failed~%  ~s = ~s; ~s = ~s"
           (quote ,var) (quote ,value)
           (quote ,var) ,var
           (quote ,value) ,value)
           (values ,retval))))

(defun not-equalv (x y &key (full-propagation nil))
"Redesigned from original Screamer-Plus.
DEPRECATED. Use (notv (equalv ...)) instead."
 (declare (ignore full-propagation))
 (let ((z (a-booleanv)))  
  (assert! (equalv z (funcallv #'(lambda (a b) (not (equal a b))) x y)))
  z))

(defun funcallgv (f &rest x)
"DEPRECATED. Use funcallv instead."
 (apply #'funcallv (cons f x)))

(defmacro-compile-time setq-domains (vars vals &aux (res nil))
"Original from original Screamer-Plus."
 (dolist (var vars) (setq res (nconc (list var vals) res)))
 (cons 'setq res))

;; note: The following functions are nondeterministic generators
;; for subsets and partitions not included in the classic Screamer.
;; They were described in:
;; SCREAMER: A Portable Efficient Implementation of Nondeterministic
;; Common Lisp, by Jeffrey Mark Siskind and David Allen McAllester.

(defun a-subset-of (x)
 "Nondeterministically generates all possible subsets of X."
  (if (null (value-of x))
       nil
      (let ((y (a-subset-of (cdr x))))
        (either (cons (car x) y) y))))

(defun a-partition-of (x)
 "Nondeterministically generates all possible partitions of X."
  (if (null x)
       nil
      (let ((y (a-partition-of (cdr x))))
        (either (cons (list (car x)) y)
                (let ((z (a-member-of y)))
                  (cons (cons (car x) z)
                        (remove z y :test #'equal :count 1)))))))
