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

(defun a-set-ofv (test)
 "Original from Screamer-Plus.
Models a set as a list
Each member of the set must produce t when the test is applied to it.
Test is a deterministic function which returns a boolean value."
  (let ((z (a-listv)))
    (assert! (everyv test z))
    z))

(defun subsetpv (x y &key (test #'equal))
"Original from Screamer-Plus
This function returns a boolean variable constrained to indicate
whether x is a subset of y."
 (let* ((x (value-of x))
        (y (value-of y))
        (z (funcallv #'subsetp x y :test test)))
    z))

(defun set-equalv (x y &key (test #'equal))
  (let ((z (a-booleanv)))
   (assert! (eqv z (andv (subsetpv x y :test test)
                         (subsetpv y x :test test))))
   z))

(defun same-elementsv (x)
  "Returns a variable constrained to have the same elements as x (order may differ)."
  (let ((x (value-of x))
        (z (make-variable)))
    (assert! (set-equalv z x))
    z))

(defun bag-equalv (x y &optional (test #'equal))
  "Returns a variable constrained to indicate whether the bags x and y are equal."
  (let* ((z (funcallv #'(lambda (x-list y-list)
                         (let ((all-elements (remove-duplicates (append x-list y-list) :test test)))
                           (every (lambda (e)
                                    (= (count e x-list :test test)
                                       (count e y-list :test test )))
                                  all-elements)))
              x y)))
    z))

(defun a-subset-ofv (x)
  (let* ((x (value-of x))
         (z (a-listv)))
   (cond ((and (listp x) (deep-bound? x))
          (assert! (memberv z (all-values (a-subset-of (deep-value-of x)))))
          z)
         (t (attach-noticer! 
             #'(lambda ()
                (when (and (deep-bound? x)
                           (<= (domain-size x) *maximum-domain-size*))
                 (assert! (memberv z (all-values (a-subset-of (apply-substitution x)))))))
               x)
            z))))

(defun intersectionv (list1 list2 &key key test test-not)
  (if (and (deep-bound? list1) (deep-bound? list2))
      (apply #'intersection (append (list list1 list2)
                 (when key      (list :key key))
                 (when test     (list :test test))
                 (when test-not (list :test-not test-not))))
      (let ((z (apply #'funcallv
                      #'intersection
                      (append (list list1 list2)
                              (when key      (list :key key))
                              (when test     (list :test test))
                              (when test-not (list :test-not test-not))))))
        z)))

(defun unionv (list1 list2 &key key test test-not)
 (if (and (deep-bound? list1) (deep-bound? list2))
     (apply #'union (append (list list1 list2)
                            (when key      (list :key key))
                            (when test     (list :test test))
                            (when test-not (list :test-not test-not))))
      (let ((z (apply #'funcallv
                      #'union
                      (append (list list1 list2)
                              (when key      (list :key key))
                              (when test     (list :test test))
                              (when test-not (list :test-not test-not))))))
        z)))
