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

(IN-PACKAGE :CL-USER)
 
(SCREAMER:DEFINE-SCREAMER-PACKAGE :screamer+
 (:use :cl)
 (:nicknames :? :screamer-plus)
 (:import-from :screamer
    :defun-compile-time
    :defmacro-compile-time
    :assert!-true
    :assert!-false
    :assert!-equalv
    :known?-true
    :known?-false
    :variable-enumerated-domain
    :variable-enumerated-antidomain
    :variable?
    :value-of
    :deep-bound?
    :deep-value-of
    :attach-noticer!)
 (:export
    :define-screamer-plus-package
    :impliesv
    :not-equalv
    :??
    :ifv
    :condv
    :make-equal
    :carv
    :cdrv
    :restv
    :consv
    :firstv
    :secondv
    :thirdv
    :fourthv
    :fifthv
    :sixthv
    :seventhv
    :eighthv
    :ninthv
    :tenthv
    :nthv
    :nthcdrv
    :subseqv
    :lengthv
    :appendv
    :make-listv
    :set-equalv
    :a-subset-of
    :subsetpv
    :a-subset-ofv
    :intersectionv
    :unionv
    :bag-equalv
    :make-arrayv
    :arefv
    :make-instancev
    :classpv
    :slot-valuev
    :class-ofv
    :class-namev
    :slot-exists-pv
    :reconcile
    :funcallinv
    :listv
    :mapcarv
    :maplistv
    :mapv
    :everyv
    :somev
    :noteveryv
    :notanyv
    :countv
    :remove-duplicatesv
    :at-leastv
    :at-mostv
    :exactlyv
    :constraint-fn
    :formatv
    :carefully
    :slot-names-of
    :objectp
    :eqv
    :funcallgv
    :setq-domains
    :listv-equalv
    :*maximum-domain-size*))
