(IN-PACKAGE :om-screamer)

(cl::defun all-rotations (lst &optional (dir "->"))
 (let ((rep (list lst)))
  (loop for x from 0
        while (< x (1- (length lst)))
        do (push (cons (car (last (car rep)))
                       (butlast (car rep)))
            rep))
  (cond ((equal "->" dir)
         (append (last rep) (butlast rep)))
        ((equal "<-" dir)
        (append (last rep) (reverse (butlast rep))))
       (t (error "Direction must be a string: <- or ->")))))

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
(assert! (all-differentv var-list))
 (all-values (solution var-list (static-ordering #'linear-force)))))

(defun asc-permutations (list n)
 (let ((var-list (list-of-members-ofv n (reverse list))))
 (assert! (all-differentv var-list))
 (assert! (apply #'<v var-list))
  (all-values (solution var-list (static-ordering #'linear-force)))))

(defun scombinations (list)
(let ((var-list (list-of-members-ofv (length list) (reverse list))))
 (all-values (solution var-list (static-ordering #'linear-force)))))

(defun closest-midic (note midics-domain)
 (let* ((mcv (a-member-ofv midics-domain)))
  (car (optimize-value 'min 
        (solution mcv (static-ordering #'linear-force))
        (absv (-v note mcv))))))
  
(defun all-diffv? (list)
 (assert! (all-differentv list))
 t)
 
(defun assert!-deep-mapcar (fun fun1 list? &rest args)
"Mapcars <fun> or applies <fun1> to <list?> <args> whether <list?> is a list or not."
(cond
  ((null list?) ())
  ((not (consp list?)) (assert! (apply fun1 list? args)))
  (t (progn (apply #'assert!-deep-mapcar fun fun1 (car list?) args)
     (apply #'assert!-deep-mapcar fun fun1 (cdr list?) args)))))

(defun any-fn (f &rest x)
;; note: Experimental function.
;; needs work: when POLARITY is NIL:
;; (assert! (eqv z (not (apply f (screamer-deep-value-of x))))
;; Should also attach noticer to z?
"This function returns a boolean variable which is the result of
the constraint function f applied to each variable in X as soon
as X is ground.
The constraint function f can be any LISP function (including
those that normally are not supported by SCREAMER) and will be
applied to bound values, not variables."
 (let ((b (a-booleanv))
 	      valf valx)     
  (screamer::attach-noticer!
    #'(lambda()
       (when (screamer::deep-bound? x)
        (setq valx (screamer::apply-substitution x))
		(setq valf (apply f (screamer::value-of valx)))
        (assert! (eqv b valf))))
        x)
  b))

(defun any-fn-return-variables (f &rest x)
;; note: Experimental function.
"This function is similar to any-fn, but returns a variable
instead of a boolean variable.
The constraint f can be any LISP function."
 (let ((z (make-variable))
        valf valx) 		       
  (screamer::attach-noticer!
    #'(lambda()
       (when (screamer::deep-bound? x)
        (setq valx (screamer::apply-substitution x))
	    (setq valf (apply f (screamer::value-of valx)))
        (assert! (equalv z valf))))
   x)
  z))

(defun absv (x)
 (let ((z (funcallv #'abs x)))
  z))

(defun modv (n d)
 (let ((z (funcallv #'mod n d)))
  z))

(defun sortv (list &optional (pred #'<))
 (cond ((null list) nil)
       ((and (listp list) (screamer::deep-bound? list))
        (sort list pred))
       ((and (listp list) (screamer::contains-variables? list))
        (let ((screamer-function (constraint-fn pred)))
         (apply screamer-function list)))
       ((screamer::variable? list)
         (let ((z (funcallv #'sort list pred)))
          z))
       (t (error "Argument of SORTV must be a list or a LISTV."))))

(defun all-membersv (e sequence)
;; needs work: when sequence is a variable, not a list.
 (let ((sequence-flat (flat sequence)))
  (cond ((listp e)
         (apply #'andv (mapcar #'(lambda (x) (memberv x sequence-flat)) (flat e))))
         (t (memberv e sequence-flat)))))
 
(defun lists=v (list1 list2)
 (unless (or (listp list1) (listp list2))
  (error "Both arguments of LISTS=V must be lists or LISTs"))
  (apply #'andv
        (mapcar #'(lambda (a b) (=v a b))
                list1
                list2)))

(defun list-elements-ofv (x)
 (apply #'?::listv x))

(defun listv-memberv (listv lists)
 (let ((z (a-booleanv))
       (booleans '()))
  (dolist (lst lists)
   (push (listv-equalv listv lst) booleans))
(assert! (eqv z (apply #'orv booleans)))
 z))

(defun equalv-lists (l1 l2)
 (equalv (apply #'?::listv l1) (apply #'?::listv l2)))

(defun a-midi->pcv (x)
 (if (bound? x)
     (mod (value-of x) 12)
     (let ((z (funcallv #'mod x 12)))
      z)))

(defun a-mc->pcv (x)
 (if (bound? x)
     (/ (mod (value-of x) 1200) 100)
     (let* ((mod-1200 (funcallv #'mod x 1200))
            (z (/v mod-1200 100)))
      z)))

 (defun x->dxv (list)
  (mapcar #'(lambda (x y) (-v y x)) list (cdr list)))

(defun dx->xv (i lst &optional (accumul '()))
  (if (null lst)
       (reverse accumul)
  	 (if (null accumul)
  	     (dx->xv i (cdr lst) (append (cons (+v i (first lst)) accumul) (list i))) 
  		 (dx->xv i (cdr lst) (cons (+v (first lst) (first accumul)) accumul)))))

 (defun dx->xv-internal-listv (start list accumul)
      (let ((sum (ifv-rec accumul
                          (+v (firstv list) (firstv accumul))
                          (+v start (firstv list)))))
 (ifv-rec (cdrv list)
          (dx->xv-internal-listv start (cdrv list) (consv sum accumul))
          (consv start (appendv (funcallv #'reverse accumul) (list sum))))))

 (defun dx->xv-listv (start list)
  (dx->xv-internal-listv start list nil))

 (defun x->dx-absv (list)
  (mapcar #'(lambda (x y) (absv (-v y x))) list (cdr list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VARIABLES GENERATORS

(defun list-of-members-ofv (n dom)
 (n-variables n 'a-member-ofv dom))

(defun list-of-integers-betweenv (n min max)
 (n-variables n 'an-integer-betweenv min max))

(defun list-of-integers-abovev (n min)
 (n-variables n 'an-integer-abovev min))

(defun list-of-integers-belowv (n max)
  (n-variables n 'an-integer-belowv max))

(defun list-of-booleansv (n)
 (n-variables n 'a-booleanv))

(defun list-of-integersv (n)
  (n-variables n 'an-integerv))

(defun list-of-realsv (n)
  (n-variables n 'a-realv))

(defun list-of-reals-betweenv (n min max)
  (n-variables n 'a-real-betweenv min max))

(defun list-of-reals-abovev (n min)
 (n-variables n 'a-real-abovev min))

(defun list-of-reals-belowv (n max)
  (n-variables n 'a-real-belowv max))

(defun list-of-numbersv (n)
  (n-variables n 'a-numberv))

(defun list-of-chords-inv (sizes domain &optional random?)
 (let ((v (if random? 
              (n-lists-of-variables sizes 'a-random-member-ofv domain)
              (n-lists-of-variables sizes 'a-member-ofv domain))))
  (mapc #'(lambda (x) (assert! (all-differentv x))) v)
  (value-of v)))

(defun list-of-random-members-ofv (n dom)
  (n-variables n 'a-random-member-ofv dom))

(defun an-integer-modv (d)
 (let* ((v (an-integerv))
        (modv (modv v d)))
  (value-of modv)))

(defun list-of-integers-modv (n d)
 (n-variables n 'an-integer-modv d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MIDI

(defvar *max-midi* 127)
(defvar *min-midi* 0)

(defun a-midiv (approx)
 (let ((v (if (<= approx 2) 
	          (an-integerv)
			  (a-realv)))
        (a (/ 2 approx)))
 (assert! (<=v v *max-midi*))
 (assert! (>=v v *min-midi*))
 (assert! (integerpv (/v v a)))
 (value-of v)))

(defun list-of-midiv (n approx)
 (n-variables n 'a-midiv approx))

(defun a-midi-member-ofv (approx domain)
 (let ((v (a-midiv approx)))
 (assert! (memberv v domain))
(value-of v)))

(defun a-random-midi-member-ofv (approx domain)
 (let ((v (a-midiv approx)))
 (assert! (memberv v (spermut-random domain)))
(value-of v)))

(defun list-of-midi-members-ofv (n approx dom)
 (n-variables n 'a-midi-member-ofv approx dom))

(defun list-of-random-midi-members-ofv (n approx dom)
(n-variables n 'a-random-midi-member-ofv approx dom))

(defun list-of-midi-chords-inv (sizes approx domain &optional random?)
 (let ((v (if random?
              (n-lists-of-variables sizes 'a-random-midi-member-ofv approx domain)
              (n-lists-of-variables sizes 'a-midi-member-ofv approx domain))))
   (mapcar #'(lambda (x) (assert! (all-differentv x))) v)
   (value-of v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MIDICENTSV

(defvar *max-midic* 12700)
(defvar *min-midic* 0)

(defun a-mcv (approx)
 (let ((v (an-integerv))
        (a (/ 200 approx)))
 (assert! (<=v v *max-midic*))
(assert! (>=v v *min-midic*))
(assert! (integerpv (/v v a)))
(value-of v)))

(defun list-of-mcv (n approx)
 (n-variables n 'a-mcv approx))

(defun a-mc-member-ofv (approx domain)
 (let ((v (a-mcv approx)))
 (assert! (memberv v domain))
(value-of v)))

(defun a-random-mc-member-ofv (approx domain)
 (let ((v (a-mcv approx)))
 (assert! (memberv v (spermut-random domain)))
(value-of v)))

(defun list-of-mc-members-ofv (n approx dom)
 (n-variables n 'a-mc-member-ofv approx dom))

(defun list-of-random-mc-members-ofv (n approx dom)
 (n-variables n 'a-random-mc-member-ofv approx dom))

(defun list-of-mc-chords-inv (lst1 approx lst2 &optional random?)
 (let ((v (if random? 
              (n-lists-of-variables lst1 'a-random-mc-member-ofv approx lst2)
              (n-lists-of-variables lst1 'a-mc-member-ofv approx lst2))))
 (mapcar #'(lambda (x) (assert! (all-differentv x))) v)
(value-of v)))
 