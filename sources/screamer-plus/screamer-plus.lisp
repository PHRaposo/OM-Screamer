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

(defvar *enumeration-limit* 100
 "Specifies the maximum number of elements permitted in variable enumerated domain.")

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


;;; Fixes from Simon White original:
;;;   - No (assert! (booleanpv c)): CL IF semantics, any non-NIL is truthy
;;;   - Uses (value-of c) instead of (equal (value-of c) t) in noticer
;;;   - Adds :dependencies on output variable
;;;   - Supports recursive calls (THEN/ELSE evaluated only when condition commits)

(defmacro-compile-time ifv (condition then &optional else)
  (let ((g-cond (gensym "COND"))
        (g-z (gensym "Z")))
    `(let ((,g-cond ,condition))
       (cond
         ;; Condition already bound: evaluate immediately
         ((bound? ,g-cond)
          (if (value-of ,g-cond) ,then ,else))
         ;; Condition known non-NIL through propagation
         ((known? (notv (equalv ,g-cond nil)))
          ,then)
         ;; Unknown: defer via noticer
         (t (let ((,g-z (make-variable)))
              (attach-noticer!
               #'(lambda ()
                   (when (bound? ,g-cond)
                     (if (value-of ,g-cond)
                         (assert!-equalv ,g-z ,then)
                         (assert!-equalv ,g-z ,else))))
               ,g-cond)
              (attach-noticer! #'(lambda nil) ,g-z :dependencies (list ,g-cond))
              ,g-z))))))

;;; CONDV - Constraint-aware COND.
;;; Same fixes as IFV: no booleanpv, CL IF semantics, :dependencies.

(defmacro-compile-time condv (&rest clauses)
  (let ((g-clauses (gensym "CLAUSES")))
    `(let ((,g-clauses (list ,@(mapcar (lambda (clause)
                                         `(list ,@(mapcar #'identity clause)))
                                       clauses))))
       (labels ((condv-helper (clauses)
                  (if (null clauses)
                      nil
                      (let* ((test (caar clauses))
                             (form (cadar clauses)))
                        (cond
                          ;; Literal T: always true
                          ((eq test t) form)
                          ;; Bound: evaluate immediately
                          ((bound? test)
                           (if (value-of test)
                               form
                               (condv-helper (cdr clauses))))
                          ;; Known non-NIL through propagation
                          ((known? (notv (equalv test nil)))
                           form)
                          ;; Unknown: defer
                          (t (let ((z (make-variable)))
                               (attach-noticer!
                                #'(lambda ()
                                    (when (bound? test)
                                      (if (value-of test)
                                          (assert!-equalv z form)
                                          (assert!-equalv z
                                            (condv-helper (cdr clauses))))))
                                test)
                               (attach-noticer! #'(lambda nil) z
                                                :dependencies (list test))
                               z)))))))
         (condv-helper ,g-clauses)))))

(defun formatv (destination control-string &rest args)
"Redesigned from original Screamer-Plus."
 (applyv #'format (apply #'list destination control-string args)))

(defun reifyv (b)
  "Convert boolean variable to integer: T->1, NIL->0."
  (cond
    ((known?-true b) 1)
    ((known?-false b) 0)
    (t (let ((z (an-integer-betweenv 0 1)))
         (attach-noticer!
          #'(lambda ()
              (when (known?-true b) (assert!-equalv z 1))
              (when (known?-false b) (assert!-equalv z 0)))
          b)
         z))))

(defun sumv (listv)
 (let ((x (value-of listv)))
 (typecase x 
  (list (if (some #'variable? x)
            (apply #'+v x)
            (apply #'+ x)))
 (screamer::variable (funcallv #'(lambda (lst) (apply #'+ lst)) listv))
 (otherwise (error "The argument for SUMV must be a list or a LISTV")))))

;; compatibility note
;; This section contains or functions from original Screamer-Plus or
;; adaptations of them, keeped here for backward compatibility.
;; Some of them are deprecated.

(defun funcross-product (f x y)
  "Compute cross-product: apply F to every pair from lists X and Y."
  (cond
    ((null x) nil)
    (t (append (funcross-pair f (car x) y) (funcross-product f (cdr x) y)))))

(defun funcross-pair (f x y)
  "Apply F to X paired with each element of Y."
  (mapcar #'(lambda (g) (funcall f (value-of x) g)) y))

(defun constraint-fn (f)
  "Takes a CL function and returns its constraint version (suffix 'v').
If no such function exists, returns a lambda wrapping applyv."
  (let ((fn-name (third (multiple-value-list (function-lambda-expression f))))
        (cfn-name nil))
    (setf cfn-name (read-from-string (format nil "~av" fn-name) nil nil))
    (if (and (SCREAMER::valid-function-name? fn-name)
             (fboundp cfn-name))
        (symbol-function cfn-name)
        (lambda (&rest args) (value-of (applyv (value-of f) args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: consv
;;;
;;; A function returning a variable constrained to be the cons of x and y 
;;; Since this function is so important I've tried to maximise the propagation
;;;
;;; Suppose z is constrained to be the cons of x and y; i.e., z == (cons x y)
;;; Then:
;;; - if y is bound at the time of function invocation, then z is immediately
;;;   bound to the cons of x and y (regardless of whether x is bound).
;;;
;;; - if z becomes bound, then the values of x and y are derived.
;;;
;;; - if x and y become bound, then z is derived
;;;
;;; - if z has an enumerated domain, then the appropriate enumerated domains
;;;   are propagated to x and y.
;;;
;;; - if x becomes bound and y has an enumerated domain, then the possible
;;;   values are propagated to z
;;;
;;; - if x has an enumerated domain and y has an enumerated domain, then
;;;   possible values for z are computed, subject to the size of the 
;;;   cross-product being less than *enumeration-limit*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun consv (x y)
  (if (and (bound? x) (bound? y))
    (cons (value-of x) (value-of y))
    (let ((z (make-variable)) mem)      
      (screamer::attach-noticer!
        #'(lambda()
            (when (bound? z)
              (assert! (equalv x (car (value-of z))))
              (assert! (equalv y (cdr (value-of z)))))
            (when (and (not (bound? z)) (enumerated-domain-p z))
              (assert! (memberv x (mapcar #'car (variable-enumerated-domain z))))
              (assert! (memberv y (mapcar #'cdr (variable-enumerated-domain z))))))
        z)
      (screamer::attach-noticer!
        #'(lambda()
            (when (and (bound? x) (not (bound? y)) (enumerated-domain-p y))
              (setq mem (mapcar #'(lambda(g) (cons (value-of x) g))
                          (variable-enumerated-domain y)))
              (assert! (memberv z mem)))
            (when (and (bound? x) (bound? y))
              (assert! (equalv z (cons (value-of x) (value-of y))))))
        x)     
      (if (bound? y)
        (assert! (equalv z (cons (value-of x) (value-of y))))
  (screamer::attach-noticer!
          #'(lambda()
              (when (and (not (bound? y)) (enumerated-domain-p y))
                (if (bound? x)
                  (assert! (memberv z (mapcar #'(lambda(g) (cons (value-of x) g))
                                        (variable-enumerated-domain y))))
                  (when (and
                          (enumerated-domain-p x)
                          (< (* (domain-size x) (domain-size y))
                            *enumeration-limit*))
                    (assert! (memberv z (funcross-product #'cons (variable-enumerated-domain x)
                                          (variable-enumerated-domain y)))))))
              (when (and (not (bound? x)) (bound? y) (enumerated-domain-p x))
                (assert! (memberv z (funcross-product #'cons (variable-enumerated-domain x)
                                      (list (value-of y))))))
              (when (and (bound? x) (bound? y))
                (assert! (equalv z (cons (value-of x) (value-of y))))))
          y))      
      z)))

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

(defmacro-compile-time setq-domains (vars vals &aux (res nil))
"Original from original Screamer-Plus."
 (dolist (var vars) (setq res (nconc (list var vals) res)))
 (cons 'setq res))

(defun not-equalv (x y &key (full-propagation nil))
"Redesigned from original Screamer-Plus.
DEPRECATED. Use (notv (equalv ...)) instead."
 (declare (ignore full-propagation))
 (let ((z (a-booleanv)))  
  (assert! (equalv z (funcallv #'(lambda (a b) (not (equal a b))) x y)))
  z))

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
