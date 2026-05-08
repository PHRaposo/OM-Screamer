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

(defun abs-rule (x z)
  (cond
    ;; x ainda e variavel
    ((variable? (value-of x))
     (if (variable? (value-of z))
         ;; Ambos variaveis
         (cond
           ((enumerated-domain-p x)
            ;; Ambos com domínio
            (if (enumerated-domain-p z)
                (let ((x-dom (variable-enumerated-domain x))
                      (z-dom (variable-enumerated-domain z)))
                  (when (some #'(lambda (zi)
                                  (not (or (member zi x-dom :test #'=)
                                           (member (- zi) x-dom :test #'=))))
                              z-dom)
                    (set-enumerated-domain!
                     z (remove-if-not
                        #'(lambda (zi)
                            (or (member zi x-dom :test #'=)
                                (member (- zi) x-dom :test #'=)))
                        z-dom))
                    (run-noticers z))
                  (when (some #'(lambda (xi) (not (member (abs xi) z-dom :test #'=)))
                              x-dom)
                    (set-enumerated-domain!
                     x (remove-if-not
                        #'(lambda (xi) (member (abs xi) z-dom :test #'=))
                        x-dom))
                    (run-noticers x)))
                ;; Somente X com domínio
                (let ((abs-domain '()))
                  (dolist (xi (variable-enumerated-domain x))
                    (pushnew (abs xi) abs-domain :test #'=))
                  (set-enumerated-domain! z abs-domain)
                  (run-noticers z))))
           ;; Somente Z com domínio
           ((enumerated-domain-p z)
            (let ((cands '()))
              (dolist (zi (variable-enumerated-domain z))
                (pushnew zi cands :test #'=)
                (pushnew (- zi) cands :test #'=))
              (set-enumerated-domain! x cands)
              (run-noticers x))))
         ;; x var, z numberp
         (cond
           ((enumerated-domain-p x)
            (when (some #'(lambda (xi) (/= (abs xi) (value-of z)))
                        (variable-enumerated-domain x))
              (set-enumerated-domain!
               x (remove-if-not
                  #'(lambda (xi) (= (abs xi) (value-of z)))
                  (variable-enumerated-domain x)))
              (run-noticers x)))
           ((zerop (value-of z))
            (screamer::restrict-value! x 0))
           (t
            (set-enumerated-domain!
             x (list (- (value-of z)) (value-of z)))
            (run-noticers x)))))
    ;; x numberp, z ainda variavel  -- SIMPLIFICADO
    ((variable? (value-of z))
     (screamer::restrict-value! z (abs (value-of x))))
    ;; Ambos numberp -- ground check
    (t
     (unless (= (abs (value-of x)) (value-of z)) (fail)))))

(defun absv (x)
  (let ((x (value-of x)))
    (typecase x
      (number (abs x))
      (screamer::variable
       (let ((z (make-variable)))
         (attach-noticer! #'(lambda () (abs-rule x z)) x)
         (attach-noticer! #'(lambda () (abs-rule x z)) z)
         z))
      (otherwise (error "Cannot take ABSV of ~A~%" x)))))

(defun exclude-value! (var value)
  (when (variable? (value-of var))
    (if (enumerated-domain-p var)
        (when (member value (variable-enumerated-domain var) :test #'=)
          (set-enumerated-domain!
           var (remove value (variable-enumerated-domain var) :test #'=))
          (run-noticers var))
        (unless (member value (variable-enumerated-antidomain var) :test #'=)
          (restrict-enumerated-antidomain! var (list value))))))

(defun mod-rule (n d z)
  (cond
    ;; d numberp (garantido != 0 pelo setup)
    ((numberp (value-of d))
     (cond
       ;; n e variavel
       ((variable? (value-of n))
        (if (variable? (value-of z))
            ;; n var, d num, z var
            (cond
              ((enumerated-domain-p n)
               (if (enumerated-domain-p z)
                   (let ((n-dom (variable-enumerated-domain n))
                         (z-dom (variable-enumerated-domain z)))
                     ;; Filter z: keep zi if some ni has (mod ni d-val) = zi
                     (when (some #'(lambda (zi)
                                     (not (some #'(lambda (ni)
                                                    (= (mod ni (value-of d)) zi))
                                                n-dom)))
                                 z-dom)
                       (set-enumerated-domain!
                        z (remove-if-not
                           #'(lambda (zi)
                               (some #'(lambda (ni)
                                         (= (mod ni (value-of d)) zi))
                                     n-dom))
                           z-dom))
                       (run-noticers z))
                     ;; Filter n: keep ni where (mod ni d-val) in z-dom
                     (when (some #'(lambda (ni)
                                     (not (member (mod ni (value-of d)) z-dom :test #'=)))
                                 n-dom)
                       (set-enumerated-domain!
                        n (remove-if-not
                           #'(lambda (ni)
                               (member (mod ni (value-of d)) z-dom :test #'=))
                           n-dom))
                       (run-noticers n)))
                   ;; Apenas n com domain: forward (estabelece z's dom)
                   (let ((mod-domain '()))
                     (dolist (ni (variable-enumerated-domain n))
                       (pushnew (mod ni (value-of d)) mod-domain :test #'=))
                     (set-enumerated-domain! z mod-domain)
                     (run-noticers z)))))
            ;; n var, d num, z numberp -- narrow n
            (when (enumerated-domain-p n)
              (when (some #'(lambda (ni)
                              (/= (mod ni (value-of d)) (value-of z)))
                          (variable-enumerated-domain n))
                (set-enumerated-domain!
                 n (remove-if-not
                    #'(lambda (ni)
                        (= (mod ni (value-of d)) (value-of z)))
                    (variable-enumerated-domain n)))
                (run-noticers n)))))
       ;; n numberp, d num, z var
       ((variable? (value-of z))
        (restrict-value! z (mod (value-of n) (value-of d))))
       ;; n numberp, d num, z numberp -- ground check
       (t
        (unless (= (mod (value-of n) (value-of d)) (value-of z)) (fail)))))
    ;; d variavel + n numberp (limited prop)
    ((numberp (value-of n))
     (when (numberp (value-of z))
       ;; n num, d var, z num -- narrow d
       (when (enumerated-domain-p d)
         (when (some #'(lambda (di)
                         (/= (mod (value-of n) di) (value-of z)))
                     (variable-enumerated-domain d))
           (set-enumerated-domain!
            d (remove-if-not
               #'(lambda (di)
                   (= (mod (value-of n) di) (value-of z)))
               (variable-enumerated-domain d)))
           (run-noticers d)))))))

(defun modv (n d)
  (let ((nv (value-of n))
        (dv (value-of d)))
    (when (and (numberp dv) (zerop dv)) (fail))
    (cond
      ((and (numberp nv) (numberp dv))
       (mod nv dv))
      (t
       (let ((n (variablize n))
             (d (variablize d))
             (z (make-variable)))
         (exclude-value! d 0)
         (attach-noticer! #'(lambda () (mod-rule n d z)) n)
         (attach-noticer! #'(lambda () (mod-rule n d z)) d)
         (attach-noticer! #'(lambda () (mod-rule n d z)) z)
         z)))))

#|
;;;; ===========================================================================
;;;; ABSV
;;;; ===========================================================================

(defun abs-rule-up (x z)
  (when (and (variable? (value-of x))
             (enumerated-domain-p x)
             (eq (variable-enumerated-domain z) t))
    (let ((abs-domain '()))
      (dolist (xi (variable-enumerated-domain x))
        (pushnew (abs xi) abs-domain :test #'=))
      (when (set-enumerated-domain! z abs-domain)
        (run-noticers z)))))

(defun absv (x)
  (let ((x (value-of x)))
    (typecase x
      (number (abs x))
      (screamer::variable
       (let ((z (funcallv #'abs x)))
         (attach-noticer! #'(lambda () (abs-rule-up x z)) x)
         z))
      (otherwise (error "Cannot take ABSV of ~A~%" x)))))

;;;; ===========================================================================
;;;; MODV
;;;; ===========================================================================

(defun mod-rule-up (n d z)
  (when (numberp (value-of d))
    (when (zerop (value-of d)) (fail))
    (when (and (variable? (value-of n))
               (enumerated-domain-p n)
               (eq (variable-enumerated-domain z) t))
      (let ((d-val (value-of d))
            (mod-domain '()))
        (dolist (ni (variable-enumerated-domain n))
          (pushnew (mod ni d-val) mod-domain :test #'=))
        (when (set-enumerated-domain! z mod-domain)
          (run-noticers z))))))

(defun modv (n d)
  (let ((nv (value-of n))
        (dv (value-of d)))
    (cond
      ((and (numberp nv) (numberp dv))
       (if (zerop dv) (fail) (mod nv dv)))
      (t
       (let* ((n (variablize n))
              (d (variablize d))
              (z (funcallv #'mod n d)))
         (attach-noticer! #'(lambda () (mod-rule-up n d z)) n)
         (attach-noticer! #'(lambda () (mod-rule-up n d z)) d)
         z)))))
|#