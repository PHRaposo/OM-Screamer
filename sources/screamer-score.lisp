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

(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCREAMER-SCORE

(defun update-pitches (voice-object solution)
 (make-instance 'voice
      :tree (tree voice-object)
      :tempo (tempo voice-object)
      :legato (legato voice-object)
      :ties (ties voice-object)
      :chords (om* solution 100)))

(defun test-solution (solution voices)
 (if (null solution)
     (progn (om-message-dialog "UNABLE TO FIND A SOLUTION." ) (om-beep) (om-abort))
  (make-instance 'poly :voices (mapcar #'update-pitches voices solution))))
  
(defun test-solution-2 (solution voices)
(if (null solution)
    (error "UNABLE TO FIND A SOLUTION.")

   (list (loop for pitches in solution
	   	       for voice in voices
		       collect (list pitches (ratios voice)))
		 (get-time-sig (car voices))
		 (tempo (car voices)))))
	
(defmethod! screamer-score ((poly-object poly)(domains t)(score-constraints t)
                             &key (force-function '("reorder" "score-position" "(declare (ignore x))" "<" "linear-force")) (random? t) (m-approx 2))
  :initvals '(nil nil nil ("reorder" "score-position" "(declare (ignore x))" "<" "linear-force") t 2)
  :indoc '("voice" "screamer-score-domain or list of domains"  "list of screamer-score-constraints" "ordering-force-functions" "t or nil" "integer")
  :menuins '((4 (("t" t) ("nil" nil))) (5 (("2" 2) ("4" 4) ("8" 8) ("16" 16))))
  :doc "SCREAMER-SCORE: this function returns a poly object replacing all pitches of open voices by the results of the search process.
<INPUT1>: poly object with any number of voices.
<INPUT2>: score-domain object (generated by screamer-score-domain) or list of domains objects.
<INPUT3>: score-constraint object (generated by constraint-one-voice, constraint-harmony, constraint-profile or constraint-measure) or list of constraints objects.
<KEY-FORCE-FUNCTION>: screamer ordering and force functions (generated by the force-function).
<KEY-RANDOM?>: t for a random solution or nil for a ordered solution.
<KEY-M-APPROX> Midi approximation (2 for semitone, 4 for quarter-tones, etc...)."
  :icon 487

 (setf *screamer-score-midi-approx* m-approx)
 (setf s::*all-screamer-score-variables* nil)
 ;(setf *screamer-score-backtrack* nil)
 
(let* ((all-domains (build-all-domains poly-object domains m-approx random?))
	    scs-time)
	   
(if *print-screamer-score-time?*
   (progn (setq scs-time (list (get-internal-run-time) (get-internal-real-time)))
        (print "Timing evaluation of screamer-score..."))
   (setq scs-time nil))

 (setf s::*all-screamer-score-variables* (flat (chords (var-domain all-domains))))

; ====> FOR DEBUG <======================================= ;
(if *screamer-score-debug*
(progn (print all-domains)
      (print "DOMAIN BUILD OK!")))

 #|
 (print s::*all-screamer-score-variables*)
 (mapcar #'(lambda (x)
	 (if (s::variable? x)
     (s::attach-noticer!
      #'(lambda()
   (when (s::bound? x)
   (print (position x s::*all-screamer-score-variables*))
   ))
      x)
 )) s::*all-screamer-score-variables*)
|#
; ====================================================== ;

    (if (screamer-score-constraint-p score-constraints)
       (apply-screamer-score-constraint score-constraints all-domains)
       (mapcar #'(lambda (constraint)
                  (apply-screamer-score-constraint constraint all-domains))
         (remove nil (flat score-constraints)))
    )

(if *screamer-score-debug*
 (progn (print "FUNCTIONS APPLIED: OK!")
	    (print "STORED BACKTRACK-CONSTRAINTS:")
		(print *screamer-score-backtrack*)
		))
 (let ((solution (screamer-score-solution ;(append (list *p-variables*)  (list (first all-domains)))
                   all-domains ;(first all-domains)
                   force-function)))
                  ;(print-screamer-score-solution (first all-domains) force-function voices)))<== FOR PRINT-VALUES

  (setf s::*all-screamer-score-variables* nil)
  (setf *screamer-score-midi-approx* nil)
   ;(setf *screamer-score-backtrack* nil)
  (if *print-screamer-score-time?* (print-scs-time scs-time))
  
  ;(if (null solution)
  ;    (progn (om-message-dialog "UNABLE TO FIND A SOLUTION." ) (om-beep) (om-abort))
;	  solution)

  (test-solution solution (voices poly-object))
  ;(if solution solution (progn (om-message-dialog "UNABLE TO FIND A SOLUTION." ) (om-beep) (om-abort))) <== FOR PRINT-VALUES
	  
	  
 )))


(defmethod screamer-score-2 ((pitch-dur list) (time-sig list) (tempo number) (domains t) (score-constraints t)
                              &key (force-function '("reorder" "score-position" "(declare (ignore x))" "<" "linear-force")) (random? t) (m-approx 2))
	
  (setf *screamer-score-midi-approx* m-approx)							   
  (setf s::*all-screamer-score-variables* nil)
  ;(setf *screamer-score-backtrack* nil)

 (let* ((screamer-poly (make-screamer-poly (mapcar #'(lambda (p-d) (make-screamer-voice (first p-d) nil (second p-d) time-sig tempo nil)) pitch-dur)))
 	   (all-domains (build-all-domains screamer-poly domains m-approx random?))
 	   )

  (setf s::*all-screamer-score-variables* (flat (chords (var-domain all-domains))))

     (if (screamer-score-constraint-p score-constraints)
        (apply-screamer-score-constraint score-constraints all-domains)
        (mapcar #'(lambda (constraint)
                   (apply-screamer-score-constraint constraint all-domains))
          (remove nil (flat score-constraints)))
     )

  (let ((solution (screamer-score-solution all-domains force-function)))

   (setf s::*all-screamer-score-variables* nil)
   (setf *screamer-score-midi-approx* nil)
   ;(setf *screamer-score-backtrack* nil)
   (test-solution-2 solution (voices screamer-poly))
  )))
  
  
   ;===> (funcall #'(lambda (x) (apply-score-cont <CONSTRAINTS> <BOUNDED-DOMAIN>)) <=== [ BACKTRACK CONSTRAINTS ]
	   
(defun screamer-score-solution (all-domains force)
 (let ((variables-domain (var-domain all-domains))
   	   ;(measures-domain (mes-domain all-domains))  <=== FOR [ BACKTRACK CONSTRAINTS ]
	   (force-function force))	   
 (if *print-screamer-score-failures?*
  (s::count-scs-failures
   (block one-value
   (s::for-effects
   (return-from one-value
	(first
   (s::solution (list-all-slots variables-domain)
   (cond ((equal force-function "static-ordering linear-force") (s::static-ordering #'s::linear-force))
         ((equal force-function "static-ordering divide-and-conquer-force") (s::static-ordering #'s::divide-and-conquer-force))
         ((equal force-function "static-ordering random-force") (s::static-ordering #'s::random-force))
             (t (s::reorder
                 (cond ((null (second force-function)) #'s::domain-size)
                           ((functionp (second force-function)) (second force-function))
                           ((equal (second force-function) "domain-size") #'s::domain-size)
                           ((equal (second force-function) "range-size") #'s::range-size)
                           ((equal (second force-function) "score-position") #'s::score-position)
                           (t #'s::domain-size))
                (cond ((null (third force-function)) #'(lambda (x) (declare (ignore x)) nil))
                          ((functionp (third force-function)) (third force-function))
                          ((equal (third force-function) "(< x 1e-6)") #'(lambda (x) (< x 1e-6)))
                          (t #'(lambda (x) (declare (ignore x)) nil)))
               (cond ((null (fourth force-function)) #'<)
                         ((functionp (fourth force-function)) (fourth force-function))
                         ((equal (fourth force-function) ">") #'>)
                         (t #'<))
              (cond ((null (fifth force-function)) #'s::linear-force)
                        ((equal (fifth force-function) "linear-force") #'s::linear-force)
                        ((equal (fifth force-function) "divide-and-conquer-force") #'s::divide-and-conquer-force)
                        ((equal (fifth force-function) "random-force") #'s::random-force)
                        (t #'s::linear-force)))))))))))		
	    (block one-value
	    (s::for-effects
	    (return-from one-value
	 	(first
	    (s::solution (list-all-slots variables-domain)
	    (cond ((equal force-function "static-ordering linear-force") (s::static-ordering #'s::linear-force))
	          ((equal force-function "static-ordering divide-and-conquer-force") (s::static-ordering #'s::divide-and-conquer-force))
	          ((equal force-function "static-ordering random-force") (s::static-ordering #'s::random-force))
	              (t (s::reorder
	                  (cond ((null (second force-function)) #'s::domain-size)
	                            ((functionp (second force-function)) (second force-function))
	                            ((equal (second force-function) "domain-size") #'s::domain-size)
	                            ((equal (second force-function) "range-size") #'s::range-size)
	                            ((equal (second force-function) "score-position") #'s::score-position)
	                            (t #'s::domain-size))
	                 (cond ((null (third force-function)) #'(lambda (x) (declare (ignore x)) nil))
	                           ((functionp (third force-function)) (third force-function))
	                           ((equal (third force-function) "(< x 1e-6)") #'(lambda (x) (< x 1e-6)))
	                           (t #'(lambda (x) (declare (ignore x)) nil)))
	                (cond ((null (fourth force-function)) #'<)
	                          ((functionp (fourth force-function)) (fourth force-function))
	                          ((equal (fourth force-function) ">") #'>)
	                          (t #'<))
	               (cond ((null (fifth force-function)) #'s::linear-force)
	                         ((equal (fifth force-function) "linear-force") #'s::linear-force)
	                         ((equal (fifth force-function) "divide-and-conquer-force") #'s::divide-and-conquer-force)
	                         ((equal (fifth force-function) "random-force") #'s::random-force)
	                         (t #'s::linear-force))
				))
			)
		   ) ;<== end solution
	   ))))
	);<==end if
	)) ;end-let-defun)
#|
(defun print-screamer-score-solution (domains force voices) ;<== FOR PRINT-VALUES
(let ((force-function force)
       (all-domains domains)
	   (voices voices))
 (s::count-scs-failures
 (s::print-values (make-instance 'poly :voices (mapcar #'update-pitches voices
  (s::solution all-domains
  (cond ((equal force-function "static-ordering linear-force") (s::static-ordering #'s::linear-force))
        ((equal force-function "static-ordering divide-and-conquer-force") (s::static-ordering #'s::divide-and-conquer-force))
        ((equal force-function "static-ordering random-force") (s::static-ordering #'s::random-force))
            (t (s::reorder
                (cond ((null (second force-function)) #'s::domain-size)
                          ((functionp (second force-function)) (second force-function))
                          ((equal (second force-function) "domain-size") #'s::domain-size)
                          ((equal (second force-function) "range-size") #'s::range-size)
                          ((equal (second force-function) "score-position") #'s::score-position)
                          (t #'s::domain-size))
               (cond ((null (third force-function)) #'(lambda (x) (declare (ignore x)) nil))
                         ((functionp (third force-function)) (third force-function))
                         ((equal (third force-function) "(< x 1e-6)") #'(lambda (x) (< x 1e-6)))
                         (t #'(lambda (x) (declare (ignore x)) nil)))
              (cond ((null (fourth force-function)) #'<)
                        ((functionp (fourth force-function)) (fourth force-function))
                        ((equal (fourth force-function) ">") #'>)
                        (t #'<))
             (cond ((null (fifth force-function)) #'s::linear-force)
                       ((equal (fifth force-function) "linear-force") #'s::linear-force)
                       ((equal (fifth force-function) "divide-and-conquer-force") #'s::divide-and-conquer-force)
                       ((equal (fifth force-function) "random-force") #'s::random-force)
                       (t #'s::linear-force))))))
      ))))))

 |#
