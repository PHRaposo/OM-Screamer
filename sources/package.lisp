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
(SCREAMER:DEFINE-SCREAMER-PACKAGE :om-screamer
 (:nicknames :om?)
 (:use :cl :screamer+)
 )
(IN-PACKAGE :om-screamer)

(in-package :om)

;; GLOBAL VARIABLES

(defvar *screamer-score-midi-approx* nil)
(setf *screamer-score-midi-approx* nil)

(defvar *screamer-score-debug* nil)
(setf  *screamer-score-debug* nil)

(defvar *print-screamer-score-failures?* nil)
 (setf *print-screamer-score-failures?* nil)

(defun set-print-score-fails (bool)
 (setf *print-screamer-score-failures?* bool))

(defvar *print-screamer-score-time?* nil)
 (setf *print-screamer-score-time?* nil)

(defun set-screamer-score-print-time (bool)
 (setf *print-screamer-score-time?* bool))

(defun set-debug (bool)
 (setf *screamer-score-debug* bool)
 (if *screamer-score-debug*
     (progn 
         (setf *print-screamer-score-failures?* t)
         (setf *print-screamer-score-time?* t)
         "SCREAMER-SCORE DEBUG: ON.")
	 (progn 
		 (setf *print-screamer-score-failures?* nil)
                (setf *print-screamer-score-time?* nil)
                "SCREAMER-SCORE DEBUG: OFF.")
 ))
