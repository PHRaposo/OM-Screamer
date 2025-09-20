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

(defun carv (x)
  (let ((x (value-of x)))
    (typecase x
      (list (car x))
      (screamer::variable
       (let ((z (funcallv #'car x)))
         z))
      (otherwise (error "Cannot take CARV of ~A~%" x)))))

(defun cdrv (x)
  (let ((x (value-of x)))
    (typecase x 
      (list (cdr x))
      (screamer::variable
       (let ((z (funcallv #'cdr x)))
         z))
      (otherwise (error "Cannot take CDRV of ~A~%" x)))))

(defun restv (x)
  (let ((x (value-of x)))
    (typecase x 
      (list (rest x))
      (screamer::variable
       (let ((z (funcallv #'rest x)))
         z))
      (otherwise (error "Cannot take RESTV of ~A~%" x)))))

 (defun consv (x y)
 (let* ((z (funcallv #'cons (value-of x) (value-of y)))
        (carv (carv z))
        (cdrv (cdrv z)))
  (assert! (equalv carv x))
  (assert! (equalv cdrv y))
   z))

(defun nthv (n lst)
  (let ((n (value-of n))
        (lst (value-of lst)))
    (cond ((or (variable? n) (variable? lst))
           (let ((z (funcallv #'nth n lst)))
            (assert! (listpv lst))
            (assert! (integerpv n))
            z))
          ((listp lst) (nth n lst))
          (t (error "Cannot take NTHV ~A of ~A.~%" n lst)))))

(defun firstv (x)
  (let ((x (value-of x)))
    (typecase x 
      (list (first x))
      (screamer::variable
        (let ((z (funcallv #'first x)))
          z))
      (otherwise (error "Cannot take FIRSTV of ~A.~%" x)))))

(defun secondv (x)
  (let ((x (value-of x)))
    (typecase x 
      (list (second x))
      (screamer::variable
        (let ((z (funcallv #'second x)))
          (assert! (listpv x))
          z))
      (otherwise (error "Cannot take SECONDV of ~A.~%" x)))))

(defun thirdv (x)
  (let ((x (value-of x)))
    (typecase x 
      (list (third x))
      (screamer::variable
        (let ((z (funcallv #'third x)))
          (assert! (listpv x))
          z))
      (otherwise (error "Cannot take THIRDV of ~A.~%" x)))))

(defun fourthv (x)
  (let ((x (value-of x)))
    (typecase x 
      (list (fourth x))
      (screamer::variable
        (let ((z (funcallv #'fourth x)))
          (assert! (listpv x))
          z))
      (otherwise (error "Cannot take FOURTHV of ~A.~%" x)))))

(defun fifthv (x)
  (let ((x (value-of x)))
    (typecase x 
      (list (fifth x))
      (screamer::variable
        (let ((z (funcallv #'fifth x)))
          (assert! (listpv x))
          z))
      (otherwise (error "Cannot take FIFTHV of ~A.~%" x)))))

(defun sixthv (x)
  (let ((x (value-of x)))
    (typecase x 
      (list (sixth x))
      (screamer::variable
        (let ((z (funcallv #'sixth x)))
          (assert! (listpv x))
          z))
      (otherwise (error "Cannot take SIXTHV of ~A.~%" x)))))

(defun seventhv (x)
  (let ((x (value-of x)))
    (typecase x 
      (list (seventh x))
      (screamer::variable
        (let ((z (funcallv #'seventh x)))
          (assert! (listpv x))
          z))
      (otherwise (error "Cannot take SEVENTHV of ~A.~%" x)))))

(defun eighthv (x)
  (let ((x (value-of x)))
    (typecase x 
      (list (eighth x))
      (screamer::variable
        (let ((z (funcallv #'eighth x)))
          (assert! (listpv x))
          z))
      (otherwise (error "Cannot take EIGHTHV of ~A.~%" x)))))

(defun ninthv (x)
  (let ((x (value-of x)))
    (typecase x 
      (list (ninth x))
      (screamer::variable
        (let ((z (funcallv #'ninth x)))
          (assert! (listpv x))
         z))
      (otherwise (error "Cannot take NINTHV of ~A.~%" x)))))

(defun tenthv (x)
  (let ((x (value-of x))) 
    (typecase x 
      (list (tenth x))
      (screamer::variable
        (let ((z (funcallv #'tenth x)))
          (assert! (listpv x))
          z))
      (otherwise (error "Cannot take TENTHV of ~A.~%" x)))))

(defun lengthv (x)
  (let ((x (value-of x)))
  (typecase x 
    (list (length x))
    (screamer::variable 
     (let ((z (funcallv #'length x)))
      z))
    (otherwise (error "Cannot take LENGTHV of ~A.~%" x)))))

(defun nthcdrv (n lst)
  (let ((n (value-of n))
        (lst (value-of lst)))
    (assert! (integerpv n))
    (cond ((or (variable? n) (variable? lst))
           (let ((z (funcallv #'nthcdr n lst)))
           (assert! (listpv lst))
           (assert! (integerpv n))
            z))
          ((listp lst) (nthcdr n lst))
          (t (error "Cannot take NTHCDRV ~A of ~A.~%" n lst)))))

(defun listv-equalv (x y)
"An enhanced version of EQUALV for lists that works with both logic variables and lists.

LISTV-EQUALV restricts its arguments to be A-LISTV.

- This function behaves like EQUALV when:
    - both arguments are ground lists, or
    - one argument is a ground list and the other is a variable.

- If one argument is a variable and the other is a list that contains variables, it returns
a boolean variable Z and sets up constraints to ensure that:
    - the variable has the same length as the list, and
    - each corresponding element in the variable and the list are equal.

- If both X and Y are variables, sets up constraints to ensure that their lengths are equal.
Then, if the lengths are known, it ensures that each corresponding element in both variables
are equal. Otherwise, when their lengths become known, restricts the variable Z to be true
if and only if all corresponding elements are equal. Returns the boolean variable Z.
"
  (let ((x (value-of x))
        (y (value-of y)))
    (cond
      ;; Both are ground lists or one is a ground list
      ;; and the other is a variable
      ((or (and (listp x) (listp y))
           (and (listp x) (deep-bound? x) (variable? y))
           (and (variable? x) (listp y) (deep-bound? y)))
       (equalv (deep-value-of x) (deep-value-of y)))

      ;; One is a list with variables, the other is a variable
      ((and (listp x) (screamer::contains-variables? x) (variable? y))
       (let ((len-x (length x))
             (len-y (lengthv y))
             (z (a-booleanv))
             (all-equalv '()))
         (assert! (listpv y))
         (assert! (=v len-x len-y))
         (attach-noticer! #'(lambda nil) z :dependencies (list x y))
         (dotimes (i len-x)
          (push (equalv (nth i x) (nthv i y)) all-equalv))
         (assert! (eqv z (apply #'andv (nreverse all-equalv))))
         z))

      ;; Symmetric case: variable and list with variables
      ((and (listp y) (screamer::contains-variables? y) (variable? x))
       (let ((len-x (lengthv x))
             (len-y (length y))
             (z (a-booleanv))
             (all-equalv '()))
         (assert! (listpv x))
         (assert! (=v len-x len-y))
         (attach-noticer! #'(lambda nil) z :dependencies (list x y))
         (dotimes (i len-y)
          (push (equalv (nthv i x) (nth i y)) all-equalv))
         (assert! (eqv z (apply #'andv (nreverse all-equalv))))
         z))

      ;; Both are variables
      ((and (variable? x) (variable? y))
       (let ((x-len (lengthv x))
             (y-len (lengthv y))
             (z (a-booleanv))
             (all-equalv '()))
         (assert! (listpv x))
         (assert! (listpv y))
         (assert! (=v x-len y-len))
         (attach-noticer! #'(lambda nil) z :dependencies (list x y))
         (cond ((or (bound? x-len) (bound? y-len))
                ;; The case where the length is known
                (dotimes (i (value-of x-len))
                 (push (equalv (nthv i x) (nthv i y)) all-equalv))
                (assert! (eqv z (apply #'andv (nreverse all-equalv))))
                z)
                ;; The case where the length is not yet known
             (t (dolist (variable (list x y))
                 (attach-noticer!
                  #'(lambda ()
                     (when (or (bound? x-len) (bound? y-len))
                      (let ((all-equalv '()))
                       (dotimes (i (value-of x-len))
                         (push (equalv (nthv i x) (nthv i y)) all-equalv))
                       (assert! (eqv z (apply #'andv (nreverse all-equalv)))))))
                  variable))
               z))))

      (t (error "Unhandled case in LISTV-EQUALV for arguments: ~A ~A" x y)))))

(defun make-listv (size &key (initial-element '(make-variable)))
 (if (and (variable? size)
          (not (bound? size)))
     (let ((z (funcallv #'(lambda (s)
                          (let ((listv '()))
                            (dotimes (c s)
                              (setq listv (nconc listv (list (eval initial-element)))))
                              listv))
                        (value-of size))))
        z)
    (let ((listv '()))
      (dotimes (c (value-of size))
        (setq listv (nconc listv (list (eval initial-element)))))
      listv)))

(defun mapcarv (function list &rest more-lists)
  (when (variable? function)
    (error "The current implementation does not allow the first argument~%~
    of MAPCARV to be an unbound variable."))
  (let* ((list (value-of list))
         (more-lists (mapcar #'value-of more-lists))
         (arguments (cons function (cons list more-lists)))
         (z (if (deep-bound? arguments)
                (apply #'mapcar arguments)
                (applyv #'mapcar arguments))))
    z))

(defun maplistv (function list &rest more-lists)
(when (variable? function)
    (error "The current implementation does not allow the first argument~%~
    of MAPLISTV to be an unbound variable."))
  (let* ((list (value-of list))
         (more-lists (mapcar #'value-of more-lists))
         (arguments (cons function (cons list more-lists)))
         (z (if (deep-bound? arguments)
                (apply #'maplist arguments)
                (applyv #'maplist arguments))))
    z))

(defun listv (&rest elements)
  (let* ((elements (mapcar #'value-of elements))
         (z (if (deep-bound? elements)
                (apply #'list elements)
                (applyv #'list elements))))
  z))

(defun appendv (&rest lists)
  (let* ((z (if (deep-bound? lists)
                (apply #'append lists)
                (applyv #'append lists))))
   z))
