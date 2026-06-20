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
    :enumerated-domain-p
    :variable?
    :value-of
    :deep-bound?
    :deep-value-of
    :attach-noticer!
    :generic-equal
    :variable?
    :set-enumerated-domain!
    :restrict-enumerated-domain!
    :restrict-enumerated-antidomain!
    :restrict-value!
    :run-noticers
    :variablize)
 (:export
    :impliesv
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
    :lengthv
    :appendv
    :make-listv
    :a-subset-of
    :a-partition-of
    :intersectionv
    :unionv
    :listv
    :mapcarv
    :maplistv
    :everyv
    :somev
    :noteveryv
    :notanyv
    :countv
    :remove-duplicatesv
    :at-leastv
    :at-mostv
    :exactlyv
    :formatv
    :carefully
    :eqv
    :setq-domains
    :not-equalv
    :listv-equalv
    :*enumeration-limit*
    :absv
    :modv
    :constraint-fn
    :set-equalv
    :subsetpv))
