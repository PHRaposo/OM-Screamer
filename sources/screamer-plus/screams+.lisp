(in-package :screamer+)

;; EXAMPLES FROM SCREAMER-PLUS.PDF
;; By Simon White

;; A SCREAMER+ Program to Play the Mastermind Game
(defun mastermind ()
  (if (catch 'fail
	   (let* (
			  (colours '(red green blue yellow white black))
			  (peg1 (a-member-ofv colours))
			  (peg2 (a-member-ofv colours))
			  (peg3 (a-member-ofv colours))
			  (peg4 (a-member-ofv colours))
			  (sol (list peg1 peg2 peg3 peg4))
			  guess total-correct colour-correct
			  )
		(assert! (all-differentv (list peg1 peg2 peg3 peg4)))
		(loop
		  (setq guess (one-value (solution sol (static-ordering #'linear-force))))
		  (if (ith-value 1 (solution sol (static-ordering #'linear-force)) nil)
			  (format t "My guess is ~s~%" guess)
			(progn
				(format t "The code is ~s~%" guess)
				(return t)
				)
			)
		 (format t "How many are the right colour and correctly positioned ? ~%")
		 (setq total-correct (read))
		 (print total-correct)
		 (if (= total-correct (length sol))
			 (return t)
		   (assert! (notv (equalv sol guess)))
		  )
		 (assert! (exactlyv total-correct #'equalv guess sol))
		 (format t "How many are the right colour ? ~%")
		 (setq colour-correct (read))
		 (print colour-correct)
		 (assert! (=v (lengthv (intersectionv guess sol)) colour-correct))
		 )
	   )
	  )
	t
  (format t "Your replies must have been inconsistent.~%")
  )
)

;; Choose four colors among BLUE YELLOW WHITE BLACK RED GREEN in a specific order.
;; EXAMPLE: RED WHITE GREEN BLUE
;; > (mastermind) 
	
;; My guess is (BLACK WHITE YELLOW BLUE)
;; How many are the right colour and correctly positioned ? 
;; > 2
;; How many are the right colour ? 
;; > 2

;; My guess is (BLACK WHITE GREEN RED)
;;How many are the right colour and correctly positioned ? 
;; > 2
;; How many are the right colour ? 
;; > 3

;; My guess is (BLACK GREEN YELLOW RED)
;; How many are the right colour and correctly positioned ?
;; > 0
;; How many are the right colour ? 
;; > 2

;; The code is (RED WHITE GREEN BLUE)
;; T


;; A SCREAMER+ Program for Solving an Instance of the Car Sequencing
;; Problem with 10 Cars 

(defclass car-type ()
((option-1 :initarg :o1)
 (option-2 :initarg :o2)
 (option-3 :initarg :o3)
 (option-4 :initarg :o4)
 (option-5 :initarg :o5)))

(defun sublists (n x)
 (do ((acc nil)
	  (going x (cdr going)))
	  ((< (length going) n) acc)
  (push (subseq going 0 n) acc)))

(defun surrogate (sequence cars-with-option which-option)
 (declare (special *capacities*))
 (do* ((capacity (cadr (assoc which-option *capacities*)))
	   (capacity-size (caddr (assoc which-option *capacities*)))
	   (countdown (length sequence) (- countdown capacity-size))
	   (options-left cars-with-option (- options-left capacity))
	   (fn (constraint-fn #'(lambda (x) (slot-value x which-option)))))
   ((< countdown 0) t)
  (assert! (at-leastv options-left fn (subseq sequence 0 countdown)))))

(defun assert-capacities (seq which-option)
 (declare (special *capacities*))
 (do* ((option-capacity (cdr (assoc which-option *capacities*)))
	   (maxnum-in-sub (first option-capacity))
	   (from-sequence (second option-capacity))
	   (sub (sublists from-sequence seq) (cdr sub))
	   (s (car sub) (car sub)))
	 ((endp sub) t)
  (assert!
   (at-mostv maxnum-in-sub
            (constraint-fn #'(lambda (x) (slot-value x which-option))) s))))

(defun count-numbers (car-dist which-option)
 (do* ((count 0)
	   (diminish car-dist (cdr diminish))
	   (current (car diminish) (car diminish)))
	 ((endp diminish) count)
   (when (slot-value (car current) which-option)
	(setq count (+ count (cdr current))))))
	
(defun solve ()
(let (*capacities* car-types car-dist seq type1 type2 type3 type4 type5 type6)
  (declare (special *capacities*))
  (setq *capacities* '((option-1 1 2)
					   (option-2 2 3)
					   (option-3 1 3)
					   (option-4 2 5)
					   (option-5 1 5)))
  (setq
   type1 (make-instance 'car-type :o1 t :o2 nil :o3 t :o4 t :o5 nil)
	type2 (make-instance 'car-type :o1 nil :o2 nil :o3 nil :o4 t :o5 nil)
	type3 (make-instance 'car-type :o1 nil :o2 t :o3 nil :o4 nil :o5 t)
	type4 (make-instance 'car-type :o1 nil :o2 t :o3 nil :o4 t :o5 nil)
	type5 (make-instance 'car-type :o1 t :o2 nil :o3 t :o4 nil :o5 nil)
	type6 (make-instance 'car-type :o1 t :o2 t :o3 nil :o4 nil :o5 nil))
  (setq car-types (list type1 type2 type3 type4 type5 type6))
  (setq car-dist (pairlis car-types '(1 1 2 2 2 2)))
	
  (setq seq (make-listv 10)) ; Sequence 10 cars
  (dolist (s seq) (assert! (memberv s car-types)))
  (dolist (ty car-dist) ; Assert distribution of car types
   (assert!
	(exactlyv
	 (cdr ty)
	 (eval `(constraint-fn (function (lambda (x) (equal x ,(car ty))))))
   seq)))
  (dolist (o *capacities*)
   (assert-capacities seq (car o))
   (surrogate seq (count-numbers car-dist (car o)) (car o)))
  (all-values (solution seq (static-ordering #'linear-force)))))



(defun print-car-options (car)
  (let ((options '((option-1 "Option 1")
				   (option-2 "Option 2")
				   (option-3 "Option 3")
				   (option-4 "Option 4")
				   (option-5 "Option 5")) ))
	(format t "    [")
	(let ((first t))
	  (dolist (opt options)
		(when (slot-value car (first opt))
		  (unless first (format t ", "))
		  (setf first nil)
		  (format t "~A" (second opt)))))
	(format t "]~%")))


(cl:defun car-sequencing-problem ()
  (let ((solutions (solve)))
	(loop for sol in solutions
		  for x from 1
		  do (progn
			   (format t "Solution ~A:~%" x)
			   (loop for car in sol
					 for y from 1
					 do (progn
						  (format t "  Car ~A: " y)
						  (print-car-options car)))))))

;; >(car-sequencing-problem)
