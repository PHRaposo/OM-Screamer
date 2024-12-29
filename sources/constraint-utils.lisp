;; MIT License

;; Copyright (c) 2024 Paulo Henrique Raposo

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OM METHODS - UTILS

(in-package :om)

(defun om-variables-in (x)
 (cond ((null x) nil)
           ((atom x) 
             (if (equal 'screamer+::variable+ (type-of x))
                 x
                 nil))
           (t (remove nil (cons (om-variables-in (car x)) (om-variables-in (cdr x)))))))

(defmethod! variables-in ((domain-list list))
    :initvals '(nil)
    :indoc '( "list")
    :doc "
 Returns all screamer variables, removing rests."
    :icon 486
(om-variables-in domain-list))

(defmethod! contain-rests? ((domain-list list))
    :initvals '((60 nil nil (s::an-integerv)))
    :indoc '( "domain-list")
    :doc "Returns t if the list containts any rests (represented as null value <nil> in the screamer-score domains)."
    :icon 486
(not (null (position 'nil (flat domain-list)))))

(defmethod! pcset-equalv ((domain-list list) (pcset list))
    :initvals '((60 64 67) (0 4 7))
    :indoc '( "midic" "pcset list")
    :doc "Returns t if a list of midi values <input1> containts all the pitch-classes in pcset list <input2>."
    :icon 486
(all-membersv (m->pcv (flat (remove nil (flat domain-list)))) pcset))
