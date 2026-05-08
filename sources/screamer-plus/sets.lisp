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
