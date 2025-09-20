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

(IN-PACKAGE :CL-USER)
(SCREAMER+:DEFINE-SCREAMER-PLUS-PACKAGE :om-screamer
 (:nicknames :om?)
 (:use :cl)
 )
(IN-PACKAGE :om-screamer)

;; FROM OPENMUSIC

(defmacro while (test &body body)
  (let ((testlab (gensym))
        (toplab (gensym)))
    `(tagbody
       (go ,testlab)
      ,toplab
      (progn ,@body)
      ,testlab
      (when ,test (go ,toplab)))))

(defmacro newl (lst elem) `(push ,elem ,lst))

(defmacro nextl (lst &optional symb)
(if symb
  `(setq ,symb (pop ,lst))
  `(pop ,lst) ))
  
(cl::defun rev-flat (lst)  
(let ((l ()))
  (while lst
         (if (not (consp (car lst)))
           (newl l (nextl lst))
           (setq l (nconc (rev-flat (nextl lst)) l))))
  l ))

(cl::defun lo-flat (list) 
(cond ((atom list) list)
      ((atom (car list)) (cons (car list) (lo-flat (cdr list))))
      ((atom (caar list)) (apply 'append list))
      (t (cons (lo-flat (car list)) (lo-flat (cdr list))))))

(cl::defun flat-low (list) 
(lo-flat list))

(cl::defun flat-once (list)
(if (consp (car list))
  (apply 'append list) list))

(cl::defun flat-one (list)
(loop for item in list
      append (list! item)))

(cl::defun n-flat-one (list level)
(let ((rep list))
  (loop for i from 1 to level do
        (setf rep (flat-one rep)))
  rep))

(cl::defun flat (lst &optional (level nil))
    (cond
     ((null level) (nreverse (rev-flat lst)))
     ((= level 0) lst)
     ((and (integerp level) (plusp level))
      (n-flat-one lst level))
     (t lst)))

(cl::defun list! (thing) (if (listp thing) thing (list thing)))
