(IN-PACKAGE :om-screamer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTION FROM T2L-SCREAMER
;;; Copyright (c) 2007, Kilian Sprotte. All rights reserved.

(defun absv (k)
(maxv k (*v k -1)))

(defun modv (n d)
 (funcallv #'mod n d))

(defun all-membersv (list sequence)
 (labels ((all-members (x seq)
          (if (null x)
              t
              (andv (memberv (car x) seq)
                    (all-members (cdr x) seq)))))
  (all-members list sequence)))

(defun all-membersv-alt (e sequence)
 (let ((sequence-flat (om::flat sequence)))
  (cond ((listp e) (reduce-chunks #'andv (mapcar #'(lambda (x) (memberv x sequence-flat)) (om::flat e))))
         (t (memberv e sequence-flat)))))

(defun funcallv-rec (fn tree)
(cond ((equalv nil tree) nil)
    ((funcallv #'consp tree)
     (cons (funcallv-rec fn (funcallv #'car tree))
           (funcallv-rec fn (funcallv #'cdr tree))))
 (t (funcallv fn tree))))

(defun modv-alt (n d)
(let ((x (an-integerv)))
(assert! (>=v x (minv 0 (+v d 1))))
(assert! (<=v x (maxv 0 (-v d 1))))
(assert! (=v x (-v n (*v d (an-integerv)))))
x))

(defun %v (n d) (modv n d))

(defun powv (a b) (funcallv #'pow a b))

(defun floorv (x)
(let* ((xR (a-realv))
      (y (an-integerv))
      (yR (a-realv))
      (d (a-realv)))
 (assert! (=v x xR))
 (assert! (=v y yR))
 (assert! (>=v d 0))
 (assert! (<v d 1))
 (assert! (=v d (-v xR yR)))
 y))
(defun ceilingv (x)
(let* ((y (an-integerv))
      (d (-v x y)))
 (assert! (<=v d 0))
 (assert! (>v d -1))
 y))

(cl:defun nsucc (input n &key step list-padding pad-character)
(cond
((null step) (nsucc input n :step (1- n) :list-padding list-padding :pad-character pad-character))
(t
 (let* ((list (if list-padding
                  (append input
                          (make-sequence 'list (* -1 (- (length input)
                                                        (* n (ceiling (/ (length input) n))))) :initial-element pad-character))
                input))
        (length (length list)))
   (loop for i from 0
         for j = (* i step)
         for k = (+ j n)
         while (< j (- (length list) step))
         collect (subseq list j (if (<= k length) k length)))))))

(cl:defun reduce-chunks (fn input &key default)
(cond
((null input) default)
((not (listp input)) (reduce-chunks fn (list input) :default default))
((>= (length input) call-arguments-limit)
 (reduce fn (mapcar #'(lambda (chunk) (apply fn chunk))
                    (nsucc input call-arguments-limit :step call-arguments-limit))))
(t (apply fn input))))

(defun sumv (list)
(reduce-chunks #'+v list :default 0))

(defun lists=v (list1 list2) ;&optional symbol-mode)
(apply #'andv
      (mapcar #'(lambda (a b) (=v a b))
              list1
              list2)))

(defun a-permutation-of (list)
(if (null list)
   nil
 (let ((i (an-integer-between 0 (1- (length list)))))
   (append (list (elt list i))
           (a-permutation-of
            (append (subseq list 0 i)
                    (subseq list (1+ i) (length list))))))))

(defun a-permutation-ofv (list) ;&key symbol-mode)
(let ((vars (list-of-members-ofv (length list) list))
           ;(mapcar #'(lambda (x)
            ;           (let ((v (an-integerv)))
            ;             (assert! (memberv v list))
            ;             v))
            ;       list))
     (perms (all-values (a-permutation-of list))))
 (assert! (reduce-chunks
           #'orv
           (mapcar #'(lambda (p) (lists=v p vars))
                   perms)))
 vars))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEEP-MAPCAR - FROM PATCHWORK, OM AND ESQUISSE

 (defun deep-mapcar (fun fun1 list? &rest args)
   "Mapcars <fun> or applies <fun1> to <list?> <args> whether <list?> is a list or not."
   (cond
     ((null list?) ())
     ((not (consp list?)) (apply fun1 list? args))
     (t (cons (apply #'deep-mapcar fun fun1 (car list?) args)
 	     (apply #'deep-mapcar fun fun1 (cdr list?) args)))))

 (defun car-mapcar (fun list?  &rest args)
    "Mapcars <fun> if list? is a list or applies <fun> if it is an atom or
  a one-element list"
    (cond  ((atom list?) (apply fun list? args))
           ((= (length list?) 1) (apply fun (car list?) args))
           (t (mapcar #'(lambda (x) (apply fun x  args ))  list? ))))

 (defun less-deep-mapcar (fun  list? &rest args)
    "Applies <fun> to <list?> <args> if <list?> is a one-level list .
     Mapcars <fun> to <list?> <args> if <list?> is a multi-level list. "
    (cond
      ((null list?) ())
      ((atom (car list?)) (apply fun list? args))
      ((atom (car (car list?)))
       (cons (apply fun (car list?)  args ) (apply #'less-deep-mapcar fun (cdr list?) args)))
      (t (cons (apply #'less-deep-mapcar fun  (car list?) args)
               (apply #'less-deep-mapcar fun  (cdr list?) args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OM-SCREAMER

(defun a-mc->pcv (x)
 (if (bound? x)
     (let ((pc (/ (mod (value-of x) 1200) 100)))
      pc)

     (let ((pcv (an-integer-betweenv 0 11))
            valx)
      (screamer::attach-noticer!
        #'(lambda ()
           (when (bound? x)
            (setq valx (apply-substitution x))
             (assert! (=v pcv (/v (funcallv #'mod (value-of valx) 1200) 100)))
           )
         )
       x)

       pcv)
  )
 )

 (defun x->dxv (list)
  (mapcar #'(lambda (x y) (-v y x)) list (cdr list)))

 (defun dx->xv (start list)
  (dx->xv-internal start list nil))

 (defun dx->xv-internal (start list accumul)
      (let ((sum (if accumul
                      (+v (first list) (first accumul))
                      (+v start (first list)))))
 (if (cdr list)
     (dx->xv-internal start (cdr list) (om::x-append sum accumul))
     (om::x-append start (reverse accumul) sum))))

 (defun dx->xv-listv (start list)
  (dx->xv-internal-listv start list nil))

 (defun dx->xv-internal-listv (start list accumul)
      (let ((sum (ifv accumul
                      (+v (firstv list) (firstv accumul))
                      (+v start (firstv list)))))
 (ifv (cdrv list)
     (dx->xv-internal-listv start (cdrv list) (consv sum accumul))
     (consv start (appendv (funcallv #'reverse accumul) (list sum))))))

 (defun x->dx-absv (list)
  (mapcar #'(lambda (x y) (absv (-v y x))) list (cdr list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VARIABLES GENERATORS

(defun list-of-members-ofv (n dom)
  (if (zerop n) nil
      (cons (a-member-ofv dom)
            (list-of-members-ofv (1- n) dom))))

(defun list-of-integers-betweenv (n min max)
  (if (zerop n) nil
      (cons (an-integer-betweenv min max)
            (list-of-integers-betweenv (1- n) min max))))

(defun list-of-integers-abovev (n min)
  (if (zerop n) nil
      (cons (an-integer-abovev min)
            (list-of-integers-abovev (1- n) min))))

(defun list-of-integers-belowv (n max)
  (if (zerop n) nil
      (cons (an-integer-belowv max)
            (list-of-integers-belowv (1- n) max))))

(defun list-of-booleansv (n)
  (if (zerop n) nil
      (cons (a-booleanv)
            (list-of-booleansv (1- n)))))

(defun list-of-integersv (n)
  (if (zerop n) nil
      (cons (an-integerv)
            (list-of-integersv (1- n)))))

(defun list-of-realsv (n)
  (if (zerop n) nil
      (cons (a-realv)
            (list-of-realsv (1- n)))))

(defun list-of-reals-betweenv (n min max)
  (if (zerop n) nil
      (cons (a-real-betweenv min max)
            (list-of-reals-betweenv (1- n) min max))))

(defun list-of-reals-abovev (n min)
  (if (zerop n) nil
      (cons (a-real-abovev min)
            (list-of-reals-abovev (1- n) min))))

(defun list-of-reals-belowv (n max)
  (if (zerop n) nil
      (cons (a-real-belowv  max)
            (list-of-reals-belowv  (1- n) max))))

(defun list-of-numbersv (n)
  (if (zerop n) nil
      (cons (a-numberv)
            (list-of-numbersv (1- n)))))

(defun list-of-chords-inv (lst1 lst2 &optional random?)
 (let ((v (mapcar #'(lambda (x)
             (if random? (list-of-random-members-ofv x lst2) (list-of-members-ofv x (reverse lst2))))
            lst1)))
 (mapcar #'(lambda (x) (assert! (apply '<v x))) v)
(value-of v)))

(defun list-of-random-members-ofv (n dom)
  (if (zerop n) nil
      (cons (a-random-member-ofv dom)
            (list-of-random-members-ofv (1- n) dom))))

(defun list-of-integers-modv (n d)
  (if (zerop n) nil
      (cons (modv (an-integerv) d)
            (list-of-integers-modv (1- n) d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NEW-VARIABLES

;;;RANDOM TEST
(defun random-domain (domain)
 (let* ((d domain)
        (random-elem (om::nth-random d))
		(random-first (om::x-append random-elem
			                       (remove-if #'(lambda (x) (= x random-elem)) d)))
		)
  random-first))

(defun a-random-member-ofv (values &optional (name nil name?))
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (memberv v (om::permut-random values)))
   (value-of v)))

(defun a-real-multiple-ofv (n m-max)
 (let ((v (a-realv))
        (all-multiples (reverse (all-values (*v (an-integer-between 1 m-max) n)))))
(assert! (memberv v all-multiples))
(value-of v)))

(defun a-multiple-of (n1 n2)
(integerpv (/v n1 n2)))

(defun an-integer-roundv (n) ;&optional (d 1))
  (let ((x (an-integer-betweenv (-v (-v n 0.5) 1e-6) (-v (+v n 0.5) 1e-6)))) ;(an-integer-betweenv (-v (-v (/v n d) 0.5) 1e-6) (-v (+v (/v n d) 0.5) 1e-6)))
         ;(rem-x (a-realv))) ;;;FIX-ME ===> REMAINDER (NEGATIVE-NUMBERS)
  (value-of x)))
  ;(assert! (=v rem-x (-v (absv n) (*v x d))))
  ; (values (value-of x) (value-of rem-x))))

; N = (* D X + REM-X)
; REM-X = N - (* D X)

(defun first-nv (list n)
  (ifv (<v (lengthv list) n) list)
   (funcallv #'butlast list (-v (lengthv list) n)))

(defun last-nv (list n)
  (funcallv #'last list n))

;;; MIDICENTSV

(defvar *max-midic* 12700)
(defvar *min-midic* 0)
(defvar *midics-range* (om::arithm-ser 0 12700 100))
(defvar *midics-approx* 100)

(defun a-mcv (approx)
 (let ((v (an-integerv))
        (a (/ 200 approx)))
 (assert! (<=v v *max-midic*))
(assert! (>=v v *min-midic*))
(assert! (integerpv (/v v a)))
(value-of v)))

(defun list-of-mcv (n approx)
  (if (zerop n) nil
      (cons (a-mcv approx)
            (list-of-mcv (1- n) approx))))

(defun a-mc-member-ofv (approx domain)
 (let ((v (a-mcv approx)))
 (assert! (memberv v domain))
(value-of v)))

(defun a-random-mc-member-ofv (approx domain)
 (let ((v (a-mcv approx)))
 (assert! (memberv v ;(random-domain domain)))
                    (om::permut-random domain)))
(value-of v)))

(defun list-of-mc-members-ofv (n approx dom)
  (if (zerop n) nil
      (cons (a-mc-member-ofv approx dom)
            (list-of-mc-members-ofv (1- n) approx dom))))

(defun list-of-random-mc-members-ofv (n approx dom)
  (if (zerop n) nil
      (cons (a-random-mc-member-ofv approx dom)
            (list-of-random-mc-members-ofv (1- n) approx dom))))

(defun list-of-mc-chords-inv (lst1 approx lst2 &optional random?)
 (let ((v (mapcar #'(lambda (x)
             (if random? (list-of-random-mc-members-ofv x approx lst2 ) (list-of-mc-members-ofv x approx (reverse lst2))))
            lst1)))
 (mapcar #'(lambda (x) (assert! (apply '<v x))) v)
(value-of v)))

(defun all-rotations-internal (list accumul)
 (let ((rotation (if accumul (om::x-append (cdr (first accumul)) (first (first accumul)))
                                         (om::x-append (cdr list) (first list)))))
(if (equal (first list) (first rotation))
    (om::x-append (list rotation) (reverse accumul))
    (all-rotations-internal list (om::x-append (list rotation) accumul)))))

(defun all-rotations (list)
 (all-rotations-internal list nil))

(defun smat-trans (lists) ;MAT-TRANS WITHOUT LOOP
(let* ((maxl (apply #'max (mapcar #'length lists)))
       (nths (all-values (an-integer-between 0 (1- maxl)))))
(mapcar #'(lambda (nth) (nth-of-lists nth lists)) nths)))

(defun nth-of-lists (n lists)
 (mapcar #'(lambda (lst) (nth n lst)) lists))

(defun spermut-random (list)
 (all-values (a-random-member-of list)))

(defun n-random-members (list n)
 (n-values n (a-random-member-of list)))

(defun spermutations (list)
(let ((var-list (list-of-members-ofv (length list) (reverse list))))
(assert!-all-differentv var-list)
 (all-values (solution var-list (static-ordering #'linear-force)))))

(defun asc-permutations (list n)
 (let ((var-list (list-of-members-ofv n (reverse list))))
 (assert!-all-differentv var-list)
 (assert! (apply #'<v var-list))
  (all-values (solution var-list (static-ordering #'linear-force)))))

(defun scombinations (list)
(let ((var-list (list-of-members-ofv (length list) (reverse list))))
 (all-values (solution var-list (static-ordering #'linear-force)))))

(defun closest-midic (note midics-domain)
(let* ((mcv (a-member-ofv midics-domain)))
  (let ((intervals (all-values
                          (solution (om?::absv (-v note mcv))
                          (static-ordering #'linear-force)))))
(assert! (orv (=v mcv (+v note (om::list-min intervals)))
                    (=v mcv (-v note (om::list-min intervals)))))
(one-value (solution mcv (static-ordering #'linear-force))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RECURSIVE FUNCTIONS FOR CONSTRAINTS

(defun apply-rec (fn list) (apply-rec-internal fn list nil))

(defun apply-rec-internal (fn list accumul)
 (let* ((fn-inputs (length (om::function-lambda-list fn)))
        (list-inputs (all-values (an-integer-between 0 (1- fn-inputs)))))
 (if (null (nth (1- fn-inputs) list))
      accumul
      (let ((one-result (apply fn (mapcar #'(lambda (n)
			  	                 (nth n list)) list-inputs))))
	(apply-rec-internal fn (cdr list) (om::x-append accumul one-result))))))

(defun assert!-apply-rec (fn list)
 (let* ((fn-inputs (length (om::function-lambda-list fn)))
        (list-inputs (all-values (an-integer-between 0 (1- fn-inputs)))))
  (labels ((app-rec (x)
           (if (null (nth (1- fn-inputs) x))
 	            nil
               (progn (assert! (apply fn (mapcar #'(lambda (n)
  		  	                              (nth n x)) list-inputs)))
                      (app-rec (cdr x))))))
   (app-rec list))))

(defun assert!-funcallv-rec (fn list)
 (labels ((funcall-rec (f x)
            (if (null x)
		 nil
            (progn (assert! (funcallv f x)) (funcall-rec f (cdr x))))))
 (funcall-rec fn list)))

(defun funcallv-rec-car-cdr (fn list)
 (labels ((funcall-car-cdr (f x xs)
           (if (null xs)
		        nil
               (progn (funcall f x xs)
                      (funcall-car-cdr f (car xs) (cdr xs))))))
  (funcall-car-cdr fn (car list) (cdr list))))

;(labels ((funcall-car-cdr (x xs)
;             (ifv (null xs)
;              t
;              (andv (funcallv fn x xs)
;                        (funcall-car-cdr (carv xs) (cdrv xs))))))
; (funcall-car-cdr (carv list) (cdrv list))))

(defun assert!-funcallv-rec-car-cdr (fn list)
    (labels ((funcall-car-cdr (f x xs)
               (if (null xs)
			        nil
               (progn (assert! (funcall f x xs))
                      (funcall-car-cdr f (car xs) (cdr xs))))))
      (funcall-car-cdr fn (car list) (cdr list))))

(defun assert!-all-differentv (list)
    (labels ((all-different (x xs)
               (if (null xs) nil
               (progn (assert! (notv (memberv x xs)))
                   (all-different (car xs) (cdr xs))))))
      (all-different (car list) (cdr list))))

(defun assert!-deep-mapcar (fun fun1 list? &rest args)
"Mapcars <fun> or applies <fun1> to <list?> <args> whether <list?> is a list or not."
(cond
  ((null list?) ())
  ((not (consp list?)) (assert! (apply fun1 list? args)))
  (t (progn (apply #'assert!-deep-mapcar fun fun1 (car list?) args)
     (apply #'assert!-deep-mapcar fun fun1 (cdr list?) args)))))

(defun assert!-less-deep-mapcar (fun  list? &rest args)
 "Applies <fun> to <list?> <args> if <list?> is a one-level list .
  Mapcars <fun> to <list?> <args> if <list?> is a multi-level list. "
 (cond
   ((null list?) nil)
   ((atom (car list?)) (assert! (apply fun (list list?) args)))
   ((atom (car (car list?)))
    (progn (assert! (apply fun (car list?) args)) (apply #'assert!-less-deep-mapcar fun (cdr list?) args)))
   (t (progn (apply #'assert!-less-deep-mapcar fun  (car list?) args)
            (apply #'assert!-less-deep-mapcar fun  (cdr list?) args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OM METHODS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :om)

(defun mk-growing (vars);==> GROWING LIST ((0) (0 1) (0 1 2) ...)
 (let* ((list-length (length vars))
         (posn (loop for x from 0 to (1- list-length)
                            for y = (arithm-ser 0 x 1)
                            collect y)))
 (posn-match vars posn)))

; APPLY-CONTV

(om::defmethod! apply-contv ((cs function) (mode string) (recursive? string) (vars t))
:initvals '(nil "atom" "off" nil)
:indoc '("patch in lambda mode" "string" "string" "list of variables" )
:menuins '((1 (("atom" "atom") ("list" "list")))
                 (2 (("off" "off") ("n-inputs" "n-inputs") ("car-cdr" "car-cdr") ("growing" "growing")))
                )
:doc "Applies constraint recursively to list of variables."
:icon 487

(cond ((equal mode "atom")
        (om?::assert!-deep-mapcar cs cs vars))
		;(om?::deep-mapcar cs cs vars))

          ((equal mode "list")
           (cond

           ((equal recursive? "n-inputs")
           (om?::assert!-apply-rec cs vars))
           ;(om?::-apply-rec cs vars))

           ((equal recursive? "car-cdr")
           (om?::assert!-funcallv-rec-car-cdr cs vars))
           ;(om?::-funcallv-rec-car-cdr cs vars))

           ((equal recursive? "growing")
            (om?::assert!-less-deep-mapcar cs (mk-growing vars)))

           (t (om?::assert!-less-deep-mapcar cs vars))
		   ;(om?::less-deep-mapcar cs vars))
          ))

         (t (progn (om-message-dialog "ERROR!") (om-abort)))))
#|
 (om::defmethod! apply-backtrack-cont ((cs function) (mode string) (recursive? string) (vars t))
 :initvals '(nil "atom" "off" nil)
 :indoc '("patch in lambda mode" "string" "string" "list of variables" )
 :menuins '((1 (("atom" "atom") ("list" "list")))
                  (2 (("off" "off") ("n-inputs" "n-inputs") ("car-cdr" "car-cdr") ("growing" "growing")))
                 )
 :doc "Applies constraint recursively to list of variables."
 :icon 486

 (cond ((equal mode "atom")
         (om?::deep-mapcar cs cs vars))

           ((equal mode "list")
            (cond

            ((equal recursive? "n-inputs")
            (om?::appc-rec cs vars))

            ((equal recursive? "car-cdr")
            (om?::funcall-rec-car-cdr cs vars))

            ((equal recursive? "growing")
            (appc-rec-growing cs (mk-growing vars)))

            (t (om?::less-deep-mapcar cs vars))
           ))

          (t (progn (om-message-dialog "ERROR!") (om-abort)))))
|#

 (om::defmethod! x->dxv ((list list))
 :initvals '((1 2 3 4 5)) :indoc '("variable or list of variables")
 :doc ""
 :icon 485
 (om?::x->dxv list))

 (om::defmethod! x->dxv ((listv screamer+::variable+))
 :initvals '((1 2 3 4 5)) :indoc '("variable or list of variables")
 :doc ""
 :icon 485
(?::mapcarv #'s::-v (?::cdrv listv) listv))

 (om::defmethod! rx->dxv ((listv screamer+::variable+))
 :initvals '((1 2 3 4 5)) :indoc '("variable or list of variables")
 :doc "Intervals in reverse order (from last to first)."
 :icon 485
(?::mapcarv #'s::-v (?::cdrv (s::funcallv #'reverse listv)) (s::funcallv #'reverse listv)))

 (om::defmethod! rx->dxv ((list list))
 :initvals '((1 2 3 4 5)) :indoc '("variable or list of variables")
 :doc  "Intervals in reverse order (from last to first)."
 :icon 485
 (om?::x->dxv (reverse list)))

 (om::defmethod! x->dx-absv ((list list))
 :initvals '((1 2 3 4 5)) :indoc '("variable or list of variables")
 :doc ""
 :icon 478
 (om?::x->dx-absv list))

 (om::defmethod! x->dx-absv ((listv screamer+::variable+))
 :initvals '((1 2 3 4 5)) :indoc '("variable or list of variables")
 :doc ""
 :icon 478
 (?::mapcarv #'(lambda (x y) (om?::absv (s::-v y x))) listv (?::cdrv listv)))

 (om::defmethod! dx->xv ((start number) (list list))
 :initvals '(0 (1 2 3 4 5)) :indoc '("variable or number" "variable, list of variables or list")
 :doc ""
 :icon 485
 (om?::dx->xv start list))

 (om::defmethod! dx->xv ((start number) (listv screamer+::variable+))
 :initvals '(0 (1 2 3 4 5)) :indoc '("variable or number" "variable, list of variables or list")
 :doc ""
 :icon 485
 (om?::dx->xv-listv start listv))

 (om::defmethod! not-intersectionv ((list1 list) (list2 list))
 :initvals '((0 2 4) (1 3 5)) :indoc '("list" "list")
 :doc ""
 :icon 477
 (s::=v (?::lengthv (?::intersectionv list1 list2)) 0))

; -----------------------------------------
; OM+V / OM-V / OM*V / OM/V / MOD12V / MC->PCV / OM-ABSV

(om::defmethod! om-absv ((n number))
:initvals '(-8) :indoc '("variable, number or list")
:icon 480
(om?::absv n))

(om::defmethod! om-absv ((lst list))
(mapcar #'om?::absv lst))

(om::defmethod! om-absv ((var screamer+::variable+))
:icon 480
(if (s::variable-number? var)
    (om?::absv var)
    (?::mapcarv #'(lambda (x) (om?::absv x)) var)))

(om::defmethod! modv ((n integer) (d integer))
:initvals '(-8 12) :indoc '("variable, number or list" "integer")
:icon 480
(s::funcallv #'mod n d))

(om::defmethod! modv ((lst list) (d integer))
(mapcar #'modv lst))

(om::defmethod! modv ((var screamer+::variable+)(d integer))
:icon 480
(if (s::variable-number? var)
    (s::funcallv #'mod var d)
    (?::mapcarv #'(lambda (x) (s::funcallv #'mod x d)) var)))

(om::defmethod! mod12v ((n integer))
:initvals '(-8) :indoc '("variable, number or list")
:icon 480
(s::funcallv #'mod n 12))

(om::defmethod! mod12v ((lst list))
(mapcar #'mod12v lst))

(om::defmethod! mod12v ((var screamer+::variable+))
:icon 480
(if (s::variable-number? var)
    (s::funcallv #'mod var 12)
    (?::mapcarv #'(lambda (x) (s::funcallv #'mod x 12)) var)))

(om::defmethod! mc->pcv ((n integer))
:initvals '(6000) :indoc '("variable, number or list")
:icon 479
(om?::a-mc->pcv n))
;(s::/v (om?::modv n 1200) 100))

(om::defmethod! mc->pcv ((n list))
:initvals '((6000 6400 6700)) :indoc '("variable, number or list")
:icon 479
(mapcar #'mc->pcv n))

(om::defmethod! mc->pcv ((var screamer+::variable+))
:initvals '(6000) :indoc '("variable, number or list")
:icon 479
 (if (s::variable-number? var)
     (om?::a-mc->pcv var)
     ;(s::/v (om?::modv var 1200) 100)
 (?::mapcarv #'om?::a-mc->pcv var)))
;(?::mapcarv #'(lambda (x) (s::/v (s::funcallv #'mod x 1200) 100) var))))


(om::defmethod! all-membersv ((list list) (sequence list))
:initvals '((11 5 2) (0 2 4 5 7 9 11)) :indoc '("variable or list" "variable or list")
:icon 477
;(apply #'s::andv (mapcar #'(lambda (x) (s::memberv x sequence)) list)))
(om?::all-membersv-alt list sequence))

(om::defmethod! all-membersv ((listv screamer+::variable+) (sequence list))
:initvals '((11 5 2) (0 2 4 5 7 9 11)) :indoc '("variable or list" "variable or list")
:icon 477
 (?::everyv #'(lambda (r)(s::equalv r 't))
  (?::mapcarv #'(lambda (x)
                (s::memberv x sequence))
   listv)))

(om::defmethod! all-membersv ((list list) (sequence screamer+::variable+))
:initvals '((11 5 2) (0 2 4 5 7 9 11)) :indoc '("variable or list" "variable or list")
:icon 477
 (?::everyv #'(lambda (r)(s::equalv r 't))
  (?::mapcarv #'(lambda (x)
                (s::memberv x sequence))
   list)))

(om::defmethod! all-diffv ((list t))
:initvals '(nil) :indoc '("list")
:icon 477
(if (listp list)
(apply 's::/=v list)
(s::applyv 's::/=v list)))

(om::defmethod! sort-listv ((list t) (direction string))
:initvals '(nil "<") :indoc '("list" "string")
:menuins '((1 (("<" "<") (">" ">"))))
:icon 474
(if (not (s::variable? list))
    (if  (not (some #'s::variable? list))
         (sort list (if (equal direction "<") #'< #'>))
 (if (equal direction "<")
     (s::funcallv #'sort list #'s::<v)
     (s::funcallv #'sort list #'s::>v)))))

(om::defmethod! quadratic-bezier ((p0 number) (p1 number) (p2 number) (steps integer))
 :initvals '(6000 4800 7400 20)
 :indoc '("number" "number" "number" "integer")
:doc "Solve a quadratic Bezier curve with three points in a given number of steps."
 :icon 473
 (let* ((t-var (interpolation 0 1 steps 0.0))
        (q0 (mapcar #'(lambda (z)
                                (+ (* (expt (- 1 z) 2) p0)
                                    (* (- 1 z) (* 2  z) p1)
                                    (* (expt z 2) p2)))
                t-var)))
(simple-bpf-from-list (om* t-var 1000)
                                  q0)))

(om::defmethod! cubic-bezier ((p0 number) (p1 number) (p2 number) (p3 number) (steps integer))
 :initvals '(3600 2100 8400 6000 20)
 :indoc '("number" "number" "number" "number" "integer")
:doc "Solve a cubic Bezier curve with four points in a given number of steps."
 :icon 473
 (let* ((t-var (interpolation 0 1 steps 0.0))
        (c0 (mapcar #'(lambda (z)
                                (+ (* (expt (- 1 z) 3) p0)
                                    (* 3 (expt (- 1 z) 2) z p1)
                                    (* 3 (- 1 z) (expt z 2) p2)
                                    (* (expt z 3) p3)))
                t-var)))
(simple-bpf-from-list (om* t-var 1000)
                                  c0)))

; -----------------------------------------

(om::defmethod! om*v ((arg1 t) (arg2 t))
:initvals '(0 0) :indoc '("variable, number or list" "variable, number or list")
:doc ""
:icon 483
(s::*v arg1 arg2))

(om::defmethod! om*v ((arg1 t) (arg2 list))
(mapcar #'(lambda (input)
            (om*v arg1 input)) arg2))

(om::defmethod! om*v ((arg1 list) (arg2 t))
(mapcar #'(lambda (input)
            (om*v  input arg2)) arg1))

; -----------------------------------------

(om::defmethod! om+v ((arg1 t) (arg2 t))
:initvals '(0 0) :indoc '("variable, number or list" "variable, number or list")
:doc ""
:icon 481
(s::+v arg1 arg2))

(om::defmethod! om+v ((arg1 t) (arg2 list))
(mapcar #'(lambda (input)
          (om+v arg1 input)) arg2))

(om::defmethod! om+v ((arg1 list) (arg2 t))
(mapcar #'(lambda (input)
          (om+v  input arg2)) arg1))

; -----------------------------------------

(om::defmethod! om-v ((arg1 t) (arg2 t))
:initvals '(0 0) :indoc '("variable, number or list" "variable, number or list")
:doc ""
:icon 484
(s::-v arg1 arg2))

(om::defmethod! om-v ((arg1 t) (arg2 list))
(mapcar #'(lambda (input)
          (om-v arg1 input)) arg2))

(om::defmethod! om-v ((arg1 list) (arg2 t))
(mapcar #'(lambda (input)
          (om-v  input arg2)) arg1))

; -----------------------------------------

(om::defmethod! om/v ((arg1 t) (arg2 t))
:initvals '(1 1) :indoc '("variable, number or list" "variable, number or list")
:doc ""
:icon 482
(s::/v arg1 arg2))

(om::defmethod! om/v ((arg1 t) (arg2 list))
(mapcar #'(lambda (input)
          (om/v arg1 input)) arg2))

(om::defmethod! om/v ((arg1 list) (arg2 t))
(mapcar #'(lambda (input)
          (om/v  input arg2)) arg1))

; -----------------------------------------

; UPDATE FUNCTIONS-WITHOUT-NAME

(setf *function-without-name*
 (let ((defaults *function-without-name*))
  (x-append (list 'om*v 'om-v 'om+v 'om/v) defaults)))

; -----------------------------------------

; UTILS

;;;RHYTHMIC CONSTRUCTOR

(defun group-ratios (timesig puls ratios)
 (let* ((tree (mktree ratios timesig))
        (tree-groups (mapcar #'second (second tree))))
  (mapcar #'(lambda (groups pulses)
	        (group-list groups pulses 'linear))
	tree-groups puls)
  )
 )

(defmethod! cons-tree ((timesig list) (puls list) (subdiv list) (mode string))
   :initvals '( ( (5 8) (6 8) (6 8)) ((2 3) (2 2 2) (1)) (((1 1) (1 1 1)) ((1 1) (1 1) (1 1)) ((1.0))) "tree")
   :indoc '( "list" "list" "list" "string")
   :menuins '((3 (("tree" "tree") ("ratio" "ratio"))))
   :doc
"Constructs a rhythmic tree from three arguments:
(1) A list of time signatures;
(2) A list of lists of pulses subdivisions;
(3) A list of lists of beats subdivisions or a list of ratios.
"
   :icon 254
 (cond ((equal mode "tree")
        (list '?
              (mapcar #'(lambda (tim p s)
                         (list tim
                              (mapcar #'list p s)))
               timesig puls subdiv)))

		((equal mode "ratio")
	     (let ((ratio-subdiv (group-ratios timesig puls subdiv)))
         (list '?
               (mapcar #'(lambda (tim p s)
                          (list tim
                               (mapcar #'list p s)))
                timesig puls ratio-subdiv))))

	    (t nil)
  )
 )

(defun tree-rotations (tree)
 (let* ((ratios (tree2ratio tree))
        (timesig (get-time-sig tree))
        (rot-positions (om?::all-rotations (arithm-ser 0 (1- (length ratios)) 1)))
        (rotations (mapcar #'(lambda (pos)
                         (posn-match ratios pos)) rot-positions)))
 (mapcar #'(lambda (ratios-list)
                (mktree ratios-list timesig))
  rotations)))

(defmethod! cons-subdiv ((subdiv list) &optional rest-pos tie-pos)
   :initvals '( (4 3 4 6) () ())
   :indoc '( "list" "list" "list")
   :doc
"Constructs the subdivisions of a measure.
"
   :icon 254
 (let ((beats (mapcar #'(lambda (x) (repeat-n 1 x)) subdiv))
        (rests (if (null rest-pos) (repeat-n nil (length subdiv)) rest-pos))
        (ties (if (null tie-pos) (repeat-n nil (length subdiv)) tie-pos)))
(mapcar #'(lambda (beat rests ties)
 (loop for n in beat
          for x from 0 to (1- (length beat))
          collect (cond ((member x rests) (* -1 n))
                               ((member x ties) (float n))
                               (t n))))
beats rests ties)))

; VOICE-MERGER

(defun voice-merger-internal (list accumul)
(cond  ((null list) accumul)
           ((null accumul) (voice-merger-internal (cdr list) (car list)))
           (t  (voice-merger-internal (cdr list) (merger (car list) accumul)))))

(defmethod! voice-merger ((objs list))
   :initvals '(nil)
   :indoc '("list of voices")
   :icon 253
   :doc "Merges a list of voices into a new voice object."
(voice-merger-internal objs nil))

(defmethod! voice-merger ((self om::poly))
   :initvals '(nil)
   :indoc '("poly object")
   :icon 253
   :doc "Merges a list of voices into a new voice object."
(voice-merger-internal (om::voices self) nil))

; BPF-FROM-POLY

(defmethod! bpf-lib-from-poly ((poly-obj poly))
    :initvals '(nil)
    :indoc '( "poly object")
    :doc "Builds a bpf-lib from a poly object."
    :icon 475
(make-instance 'bpf-lib
  :bpf-list
  (mapcar #'(lambda (voice)
   (let ((tempo-ms (float (/ 1000 (/ 60 (second (car (tempo voice))))))))
   (simple-bpf-from-list (butlast (dx->x 0 (om* tempo-ms (om-abs (tree2ratio (tree voice))))))
                                    (flat (mapcar #'lmidic (chords voice))))))
   (voices poly-obj))))


