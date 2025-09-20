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

(defun mapv (result-type function sequence &rest more-sequences)
(let ((sequence (value-of sequence))
      (more-sequences (mapcar #'value-of more-sequences)))
(when (variable? result-type)
    (error "The current implementation does not allow the first argument~%~
    of MAPV to be an unbound variable."))
(when (variable? function)
    (error "The current implementation does not allow the second argument~%~
    of MAPV to be an unbound variable."))
 (if (and (deep-bound? sequence)
          (deep-bound? more-sequences))
     (apply #'map (list* result-type function sequence more-sequences))
     (let ((z (applyv #'map (list* result-type function sequence more-sequences))))
      z))))

(defun at-mostv (n fn sequence &rest more-sequences)
  (let* ((sequence (value-of sequence))
        (more-sequences (mapcar #'value-of more-sequences))
        (z (a-booleanv)) 
        (count-trues (sumv (mapcarv #'reifyv (apply #'mapv (list* 'list fn sequence more-sequences))))))
    (assert! (impliesv z (<=v count-trues n)))
    (assert! (impliesv (notv z) (>=v count-trues n)))
    (assert!-true z)    
    z))

(defun at-leastv (n fn sequence &rest more-sequences)
  (let* ((sequence (value-of sequence))
        (more-sequences (mapcar #'value-of more-sequences))
        (z (a-booleanv)) 
        (count-trues (sumv (mapcarv #'reifyv (apply #'mapv (list* 'list fn sequence more-sequences))))))
    (assert! (impliesv z (>=v count-trues n)))
    (assert! (impliesv (notv z) (<=v count-trues n)))
    (assert!-true z)
    z))

(defun exactlyv (n fn sequence &rest more-sequences)
  (let* ((sequence (value-of sequence))
        (more-sequences (mapcar #'value-of more-sequences))
        (z (a-booleanv)) 
        (count-trues (sumv (mapcarv #'reifyv (apply #'mapv (list* 'list fn sequence more-sequences))))))
    (assert! (impliesv z (=v count-trues n)))
    (assert! (impliesv (notv z) (/=v count-trues n)))
    (assert!-true z)
    z))

(defun everyv (predicate sequence &rest more-sequences)
(let ((sequence (value-of sequence))
      (more-sequences (mapcar #'value-of more-sequences)))
(when (variable? predicate)
    (error "The current implementation does not allow the first argument~%~
    of EVERYV to be an unbound variable."))
 (if (and (deep-bound? sequence)
          (deep-bound? more-sequences))
    (apply #'every (list* predicate sequence more-sequences))
    (let ((z (applyv #'(lambda (seq) (every predicate seq)) (list* sequence more-sequences))))
     z))))

(defun somev (predicate sequence &rest more-sequences)
(let ((sequence (value-of sequence))
      (more-sequences (mapcar #'value-of more-sequences)))
(when (variable? predicate)
    (error "The current implementation does not allow the first argument~%~
    of SOMEV to be an unbound variable."))
 (if (and (deep-bound? sequence)
          (deep-bound? more-sequences))
    (apply #'some (list* predicate sequence more-sequences))
    (let ((z (applyv #'(lambda (seq) (some predicate seq)) (list* sequence more-sequences))))
     z))))

(defun notanyv (predicate sequence &rest more-sequences)
(let ((sequence (value-of sequence))
      (more-sequences (mapcar #'value-of more-sequences)))
(when (variable? predicate)
    (error "The current implementation does not allow the first argument~%~
    of NOTANYV to be an unbound variable."))
 (if (and (deep-bound? sequence)
          (deep-bound? more-sequences))
    (apply #'notany (list* predicate sequence more-sequences))
    (let ((z (applyv #'(lambda (seq) (notany predicate seq)) (list* sequence more-sequences))))
     z))))

(defun noteveryv (predicate sequence &rest more-sequences)
(let ((sequence (value-of sequence))
      (more-sequences (mapcar #'value-of more-sequences)))
(when (variable? predicate)
    (error "The current implementation does not allow the first argument~%~
    of NOTEVERYV to be an unbound variable."))
 (if (and (deep-bound? sequence)
          (deep-bound? more-sequences))
    (apply #'notevery (list* predicate sequence more-sequences))
    (let ((z (applyv #'(lambda (seq) (notevery predicate seq)) (list* sequence more-sequences))))
      z))))

(defun subseqv (sequence start &optional end)
(let ((sequence (value-of sequence))
      (start (value-of start))
      (end (value-of end)))
 (if (or (variable? sequence) (variable? start) (variable? end))
         (let ((z (applyv #'subseq (append (list sequence start)
                                           (when end (list end))))))
           z)
         (apply #'subseq (append (list sequence start)
                                 (when end (list end)))))))

(defun countv (item sequence &key from-end start end key test test-not)
(let* ((sequence (value-of sequence))
       (arguments (append (list item sequence)
                          (when from-end (list :from-end from-end))
                          (when start (list :start start))
                          (when end (list :end end))
                          (when key (list :key key))
                          (when test (list :test test))
                          (when test-not (list :test-not test-not)))))
    (when (some #'identity (mapcar #'variable? (cdr arguments)))
        (error "The current implementation does not allow any optional argument~%~
        of COUNTV to be an unbound variable."))
    (if (deep-bound? arguments)
        (apply #'count arguments)
        (let ((z (applyv #'count arguments)))
         z))))

(defun remove-duplicatesv (sequence &key from-end test test-not start end key)
(let* ((sequence (value-of sequence))
       (arguments (append (list sequence)
                          (when from-end (list :from-end from-end))
                          (when test (list :test test))
                          (when test-not (list :test-not test-not))
                          (when start (list :start start))
                          (when end (list :end end))
                          (when key (list :key key)))))
    (when (some #'identity (mapcar #'variable? (cdr arguments)))
        (error "The current implementation does not allow any optional argument~%~
        of REMOVE-DUPLICATESV to be an unbound variable."))
    (if (deep-bound? sequence)
        (apply #'remove-duplicates arguments)
        (let ((z (applyv #'remove-duplicates arguments)))
         (cond ((variable? sequence)
                (assert! (everyv (lambda (el) (memberv el z)) sequence)))
               (t (map nil (lambda (el) (assert! (memberv el z))) sequence)))
         (value-of z)))))

;; compat

(defun members-ofv (x)
"DEPRECATED. Use remove-duplicatesv instead."
 (remove-duplicatesv x :test #'equal))
