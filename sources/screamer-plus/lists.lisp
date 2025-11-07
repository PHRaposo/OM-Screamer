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

(defun car-rule-up (x z)
 (when (and (variable? (deep-value-of x))
            (not (eq (variable-enumerated-domain x) t))
            (variable? (deep-value-of z)))        
      (if (not (eq (variable-enumerated-domain z) t))
          (let ((car-domain (remove-duplicates (mapcar #'car (variable-enumerated-domain x)) :test #'generic-equal)))
           (if (set-enumerated-domain!
                z (remove-if-not #'(lambda (element) (member element car-domain
                                                            :test #'generic-equal))
                                 (variable-enumerated-domain z)))
              (run-noticers z)))
          (let ((domain (mapcar #'car (variable-enumerated-domain x))))
           (restrict-enumerated-domain! z domain)))))

(defun car-rule-down (z x)
 (when (and (variable? (deep-value-of x))
            (variable? (deep-value-of z))
            (not (eq (variable-enumerated-domain z) t))
            (not (eq (variable-enumerated-domain x) t)))
      (let ((car-domain (variable-enumerated-domain z)))
        (if (set-enumerated-domain!
            x (remove-if-not #'(lambda (element) (member (car element) car-domain
                                                          :test #'generic-equal))
                            (variable-enumerated-domain x)))
          (run-noticers x)))))
     
(defun carv (x)
  (let ((x (value-of x)))
    (typecase x
      (list (car x))
      (screamer::variable
       (let ((z (funcallv #'car x)))
        (attach-noticer! #'(lambda () (car-rule-up x z)) x)
        (attach-noticer! #'(lambda () (car-rule-down z x)) z :dependencies (list x))
         z))
      (otherwise (error "Cannot take CARV of ~A~%" x)))))

(defun cdr-rule-up (x z)
  (when (and (variable? (deep-value-of x))
             (not (eq (variable-enumerated-domain x) t))
             (variable? (deep-value-of z)))
    (if (not (eq (variable-enumerated-domain z) t))
        (let ((cdr-domain (remove-duplicates (mapcar #'cdr (variable-enumerated-domain x)) :test #'generic-equal)))
          (if (set-enumerated-domain!
               z (remove-if-not #'(lambda (element) (member element cdr-domain
                                                            :test #'generic-equal))
                                (variable-enumerated-domain z)))
              (run-noticers z)))
        (let ((domain (mapcar #'cdr (variable-enumerated-domain x))))
          (restrict-enumerated-domain! z domain)))))

(defun cdr-rule-down (z x)
  (when (and (variable? (deep-value-of x))
             (variable? (deep-value-of z))
             (not (eq (variable-enumerated-domain z) t))
             (not (eq (variable-enumerated-domain x) t)))
    (let ((cdr-domain (variable-enumerated-domain z)))
      (if (set-enumerated-domain!
           x (remove-if-not #'(lambda (element) (member (cdr element) cdr-domain
                                                        :test #'generic-equal))
                            (variable-enumerated-domain x)))
          (run-noticers x)))))

(defun cdrv (x)
  (let ((x (value-of x)))
    (typecase x
      (list (cdr x))
      (screamer::variable
       (let ((z (funcallv #'cdr x)))
         (attach-noticer! #'(lambda () (cdr-rule-up x z)) x)
         (attach-noticer! #'(lambda () (cdr-rule-down z x)) z :dependencies (list x))
         z))
      (otherwise (error "Cannot take CDRV of ~A~%" x)))))

(defun restv (x) (cdrv x))

(defun cons-rule-up (x y z)
 (when (and (domain-size (list x y))
            (<= (domain-size (list x y)) *maximum-discretization-range*)
            (or (variable? (deep-value-of x))
                (variable? (deep-value-of y)))
            (not (eq (variable-enumerated-domain x) t))
            (not (eq (variable-enumerated-domain y) t))
            (variable? (deep-value-of z)))
    (if (not (eq (variable-enumerated-domain z) t))
        (let ((cons-domain (all-values (solution (cons x y) (static-ordering #'linear-force)))))
          (if (set-enumerated-domain!
               z (remove-if-not #'(lambda (element) (member element cons-domain
                                                            :test #'generic-equal))
                                (variable-enumerated-domain z)))
              (run-noticers z)))
        (let ((domain (all-values (solution (cons x y) (static-ordering #'linear-force)))))
          (restrict-enumerated-domain! z domain)))))

(defun cons-rule-down (z x y)
  (when (and (or (variable? (deep-value-of x))
                 (variable? (deep-value-of y)))
             (not (eq (variable-enumerated-domain z) t)))
   (when (variable? (deep-value-of x))
    (if (not (eq (variable-enumerated-domain x) t))
        (let ((car-z-domain (remove-duplicates (mapcar #'car (variable-enumerated-domain z))
                                            :test #'generic-equal)))
         (if (set-enumerated-domain!
              x (remove-if-not
                            #'(lambda (element)
                                (member element car-z-domain
                                        :test #'generic-equal))
                            (variable-enumerated-domain x)))
              (run-noticers x)))
        (let ((car-z-domain (mapcar #'car (variable-enumerated-domain z))))
          (restrict-enumerated-domain! x car-z-domain))))
    (when (variable? (deep-value-of y))
    (if (not (eq (variable-enumerated-domain y) t))
        (let ((cdr-z-domain (remove-duplicates (mapcar #'cdr (variable-enumerated-domain z))
                                            :test #'generic-equal)))
         (if (set-enumerated-domain!
              y (remove-if-not
                            #'(lambda (element)
                                (member element cdr-z-domain
                                        :test #'generic-equal))
                            (variable-enumerated-domain y)))
              (run-noticers y)))
        (let ((cdr-z-domain (mapcar #'cdr (variable-enumerated-domain z))))
         (restrict-enumerated-domain! y cdr-z-domain))))))

(defun consv2 (x y)
 (let* ((x (variablize x))
        (y (variablize y))
        (z (funcallv #'cons x y)))
    (attach-noticer! #'(lambda () (cons-rule-up x y z)) x)
    (attach-noticer! #'(lambda () (cons-rule-up x y z)) y)
    (attach-noticer! #'(lambda () (cons-rule-down z x y)) z :dependencies (list x y))
    z))

(defun consv-internal (x y)
 (cond ((atom x)
        (if (atom y)
            (consv2 x y)
            (consv2 x (consv (car y) (cdr y)))))
       ((atom y)
        (consv2 (consv (car x) (cdr x)) y))
       (t (consv2 (consv (car x) (cdr x))
                  (consv (car y) (cdr y))))))

(defun consv (x y)
 (consv-internal (value-of x) (value-of y)))

(defun listv-internal (x)
  (cond
    ((null x) nil)
    ((consp x) (consv2 (listv-internal (car x)) (listv-internal (cdr x))))
    (t x)))

(defun listv (&rest args)
 (listv-internal args))

(defun length-rule-up (x z)
  (when (and (variable? (deep-value-of x))
             (not (eq (variable-enumerated-domain x) t))
             (variable? (deep-value-of z)))
    (if (not (eq (variable-enumerated-domain z) t))
        (let ((length-domain (remove-duplicates (mapcar #'length (variable-enumerated-domain x)))))
          (if (set-enumerated-domain!
               z (remove-if-not #'(lambda (element) (member element length-domain :test #'=))
                                (variable-enumerated-domain z)))
              (run-noticers z)))
        (let ((domain (mapcar #'length (variable-enumerated-domain x))))
          (restrict-enumerated-domain! z domain)))))

(defun length-rule-down (z x)
  (when (and (variable? (deep-value-of x))
             (variable? (deep-value-of z))
             (not (eq (variable-enumerated-domain z) t))
             (not (eq (variable-enumerated-domain x) t)))
    (let ((length-domain (variable-enumerated-domain z)))
      (if (set-enumerated-domain!
           x (remove-if-not #'(lambda (element) (member (length element) length-domain :test #'=))
                            (variable-enumerated-domain x)))
          (run-noticers x)))))

(defun lengthv (x)
  (let ((x (value-of x)))
    (typecase x
      (list (length x))
      (screamer::variable
       (let ((z (funcallv #'length x)))
         (assert! (andv (integerpv z) 
                        (>=v z 0)))
         (attach-noticer! #'(lambda () (length-rule-up x z)) x)
         (attach-noticer! #'(lambda () (length-rule-down z x)) z :dependencies (list x))
         z))
      (otherwise (error "Cannot take LENGTHV of ~A~%" x)))))

(defun nth-rule-up (n x z)
  (when (and (domain-size (list n x))
             (<= (domain-size (list n x)) *maximum-discretization-range*)
             (or (variable? (deep-value-of n))
                 (variable? (deep-value-of x)))
             (variable? (deep-value-of z)))
    (let* ((x-domain (variable-enumerated-domain x))
           (n-domain (variable-enumerated-domain n))
           (new-z-domain (let ((results '()))
                          (dolist (x-element x-domain results)
                            (dolist (n-element n-domain)
                              (pushnew (nth n-element x-element) results))))))
      (if (not (eq (variable-enumerated-domain z) t))
          (when (set-enumerated-domain!
                   z (remove-if-not
                      #'(lambda (element)
                        (member element new-z-domain :test #'generic-equal))
                      (variable-enumerated-domain z)))
              (run-noticers z))
          (restrict-enumerated-domain! z new-z-domain)))))

(defun nth-rule-down (z n x)
  (when (and (or (variable? (value-of n))
                 (variable? (deep-value-of x)))
             (not (eq (variable-enumerated-domain z) t)))             
  (let* ((z-domain (variable-enumerated-domain z)))
   (when (and (variable? (value-of n))
              (not (eq (variable-enumerated-domain x) t)))
    (if (not (eq (variable-enumerated-domain n) t))
        (let ((new-n-domain (remove-if-not
                             #'(lambda (n-element)
                                 (some #'(lambda (x-element)
                                            (member (nth n-element x-element) z-domain :test #'generic-equal))
                                       (variable-enumerated-domain x)))
                             (variable-enumerated-domain n))))
          (when (set-enumerated-domain! n new-n-domain)
            (run-noticers n)))
        ;; Compute valid indices: all integers N such that (nth N x-element) member z-domain
        ;; for some x-element in x-domain
        (let ((x-domain (variable-enumerated-domain x))
              (valid-indices '()))
          (dolist (x-element x-domain)
            (dotimes (i (length x-element))
              (when (member (nth i x-element) z-domain :test #'generic-equal)
                (pushnew i valid-indices))))
          (when valid-indices
            (restrict-enumerated-domain! n valid-indices)))))
    (when (and (variable? (deep-value-of x))
                (not (eq (variable-enumerated-domain n) t)))
          (if (not (eq (variable-enumerated-domain x) t))
              (let ((new-x-domain (remove-if-not
                                   #'(lambda (x-element)
                                       (some #'(lambda (n-element)
                                                  (member (nth n-element x-element) z-domain :test #'generic-equal))
                                             (variable-enumerated-domain n)))
                                   (variable-enumerated-domain x))))
                (when (set-enumerated-domain! x new-x-domain)
                  (run-noticers x))))))))

(defun nthv (n x)
 (let ((x (value-of x))
       (n (value-of n)))
 (if (and (bound? n)
          (or (listp x) (deep-bound? x)))
      (nth (value-of n) (deep-value-of x))
      (let* ((n (variablize n))
             (x (typecase x
                 (screamer::variable x)
                 (list (apply #'listv x))
                 (t (error "Cannot take NTHV ~A of ~A.~%" n x))))
             (z (funcallv #'nth n x)))
       (unless (bound? n)
        (assert! (andv (>=v n 0)
                       (<=v n (-v (lengthv x) 1)))))
       (attach-noticer! #'(lambda () (nth-rule-up n x z)) n)
       (attach-noticer! #'(lambda () (nth-rule-up n x z)) x)
       (attach-noticer! #'(lambda () (nth-rule-down z n x)) z :dependencies (list n x))
       z))))

(defun firstv (x) (carv x))

(defun secondv (x) (nthv 1 x))

(defun thirdv (x) (nthv 2 x))

(defun fourthv (x) (nthv 3 x))

(defun fifthv (x) (nthv 4 x))

(defun sixthv (x) (nthv 5 x))

(defun seventhv (x) (nthv 6 x))

(defun eighthv (x) (nthv 7 x))

(defun ninthv (x) (nthv 8 x))

(defun tenthv (x) (nthv 9 x))

(defun nthcdr-rule-up (n x z)
  (when (and (domain-size (list n x))
             (<= (domain-size (list n x)) *maximum-discretization-range*)
             (or (variable? (deep-value-of n))
                 (variable? (deep-value-of x)))
             (variable? (deep-value-of z)))
    (let* ((x-domain (variable-enumerated-domain x))
           (n-domain (variable-enumerated-domain n))
           (new-z-domain
             (let ((results '()))
               (dolist (x-element x-domain results)
                 (dolist (n-element n-domain)
                   (pushnew (nthcdr n-element x-element) results :test #'generic-equal))))))
      (if (not (eq (variable-enumerated-domain z) t))
          (when (set-enumerated-domain!
                 z (remove-if-not
                    (lambda (element)
                      (member element new-z-domain :test #'generic-equal))
                    (variable-enumerated-domain z)))
            (run-noticers z))
          (restrict-enumerated-domain! z new-z-domain)))))

(defun nthcdr-rule-down (z n x)
  (when (and (or (variable? (value-of n))
                 (variable? (deep-value-of x)))
             (not (eq (variable-enumerated-domain z) t)))
    (let ((z-domain (variable-enumerated-domain z)))
      (when (and (variable? (value-of n))
                 (not (eq (variable-enumerated-domain x) t)))
        (if (not (eq (variable-enumerated-domain n) t))
            (let ((new-n-domain
                    (remove-if-not
                      #'(lambda (n-element)
                        (some #'(lambda (x-element)
                                (member (nthcdr n-element x-element) z-domain :test #'generic-equal))
                              (variable-enumerated-domain x)))
                      (variable-enumerated-domain n))))
              (when (set-enumerated-domain! n new-n-domain)
                (run-noticers n)))
            (restrict-enumerated-domain!
             n (mapcar #'(lambda (x-element)
                          (remove-if-not #'(lambda (n-element)
                                            (member (nthcdr n-element x-element) z-domain :test #'generic-equal))
                                         (variable-enumerated-domain n)))
                       (variable-enumerated-domain x)))))
      (when (and (variable? (deep-value-of x))
                 (not (eq (variable-enumerated-domain n) t)))
        (if (not (eq (variable-enumerated-domain x) t))
            (let ((new-x-domain
                    (remove-if-not #'(lambda (x-element)
                                      (some #'(lambda (n-element)
                                               (member (nthcdr n-element x-element) z-domain :test #'generic-equal))
                                            (variable-enumerated-domain n)))
                                   (variable-enumerated-domain x))))
              (when (set-enumerated-domain! x new-x-domain)
                (run-noticers x)))
            (restrict-enumerated-domain!
             x (mapcar #'(lambda (n-element)
                          (remove-if-not #'(lambda (x-element)
                                            (member (nthcdr n-element x-element) z-domain :test #'generic-equal))
                                         (variable-enumerated-domain x)))
                      (variable-enumerated-domain n))))))))

(defun nthcdrv (n x)
 (let ((x (value-of x))
       (n (value-of n)))
  (if (and (bound? n)
           (or (listp x) (deep-bound? x)))
      (nthcdr (value-of n) (deep-value-of x))
      (let* ((n (variablize n))
             (x (typecase x
                  (screamer::variable x)
                  (list (apply #'listv x))
                  (t (error "Cannot take NTHCDRV ~A of ~A.~%" n x))))
             (z (funcallv #'nthcdr n x)))
        (unless (bound? n)
          (assert! (andv (integerpv n)
                         (>=v n 0)
                         (<=v n (lengthv x)))))
        (attach-noticer! #'(lambda () (nthcdr-rule-up n x z)) n)
        (attach-noticer! #'(lambda () (nthcdr-rule-up n x z)) x)
        (attach-noticer! #'(lambda () (nthcdr-rule-down z n x)) z :dependencies (list n x))
        z))))

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

(defun appendv (&rest lists)
  (let* ((z (if (deep-bound? lists)
                (apply #'append lists)
                (applyv #'append lists))))
   z))
