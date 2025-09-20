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

(defun arefv (array &rest subscripts)
 (let ((array (value-of array))
       (subscripts (mapcar #'value-of subscripts)))
 (if (or (variable? array)
         (some #'variable? subscripts))
     (let ((z (applyv #'aref (cons array subscripts))))
      z)
     (apply #'aref (cons array subscripts)))))

(defun generate-initial-contents (dimensions variable-generator &optional arguments)
 (let ((contents '())) 
 (dotimes (n (car dimensions))
  (let ((inner-dimension '()))
   (dotimes (m (second dimensions))
    (push (third dimensions) inner-dimension))
  (if arguments
  (push (n-lists-of-variables (nreverse inner-dimension) variable-generator arguments) contents)
  (push (n-lists-of-variables (nreverse inner-dimension) variable-generator) contents))))
 (nreverse contents)))

(defun make-arrayv (dimensions &key element-type
                                    initial-element
                                    initial-contents
                                    adjustable
                                    fill-pointer
                                    displaced-to
                                    displaced-index-offset)
(let ((dimensions (value-of dimensions)))
 (if (variable? dimensions)
     (let ((z (applyv #'make-array
                       (append (list dimensions)
                               (when element-type (list :element-type (value-of element-type)))
                               (when initial-element (list :initial-element (value-of initial-element)))
                               (when initial-contents (list :initial-contents (value-of initial-contents)))
                               (when adjustable (list :adjustable (value-of adjustable)))
                               (when fill-pointer (list :fill-pointer (value-of fill-pointer)))
                               (when displaced-to (list :displaced-to (value-of displaced-to)))
                               (when displaced-index-offset (list :displaced-index-offset (value-of displaced-index-offset)))))))
      z)
     (apply #'make-array
         (append (list dimensions)
                 (when element-type (list :element-type (value-of element-type)))
                 (when initial-element (list :initial-element (value-of initial-element)))
                 (when initial-contents (list :initial-contents (value-of initial-contents)))
                 (when adjustable (list :adjustable (value-of adjustable)))
                 (when fill-pointer (list :fill-pointer (value-of fill-pointer)))
                 (when displaced-to (list :displaced-to (value-of displaced-to)))
                 (when displaced-index-offset (list :displaced-index-offset (value-of displaced-index-offset))))))))
