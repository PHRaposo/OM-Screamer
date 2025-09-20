;===============================================
;===============================================

;;; FROM

;;; PWConstraints by Mikael Laurson (c), 1995
	
;;; AND OMCS 1.4

;;; Adapted to OM-Screamer by Paulo Raposo, 2023-2025

;===============================================
;===============================================

(in-package :om-screamer)

;===============================================

(defparameter *SC-data-file* (namestring
                              (make-pathname :directory (pathname-directory *load-pathname*)
                                             :name "SCs-data.lisp")))

(defvar *all-possible-chroma-subsets-hash* (make-hash-table :test #'equal)
 "Hash-table with all possible chromatic subsets.")

(cl:defun fill-possible-chroma-subsets-hash ()
  (let (sets-list) 
    (with-open-file (in *SC-data-file* :direction :input)
      (setq sets-list (eval (read in))))
    (clrhash *all-possible-chroma-subsets-hash*)
    (dolist (set sets-list) 
      (setf (gethash (first set) *all-possible-chroma-subsets-hash*)  
            (second set)))
    (dolist (set sets-list) 
      (when (zerop (second (second set)))
      (setf (get  (first (second set)) :prime) (first set))))))

(fill-possible-chroma-subsets-hash)

 ; =============================================================== ;
   ;;; CONVERSION: OM SYMBOL <-> OM? SYMBOL
; =============================================================== ;

(defun om?-symb->om (om?-sym)
   (let ((write-symbol (lambda (x)  (car (list! (intern (string-upcase x) (package-name *package*)))))))
    (if (atom om?-sym)
         (funcall write-symbol om?-sym)
    (mapcar write-symbol om?-sym))))

(defun om-symb->om? (om-sym)
   (let ((read-symbol (lambda (x) (car (list! (intern (string-upcase x) :om?))))))
    (if (atom om-sym)
         (funcall read-symbol om-sym)
    (mapcar read-symbol om-sym)))) 

; =============================================================== ;
; DETERMINISTIC FUNCTIONS FOR PC SETS
; =============================================================== ;

(cl:defun prime (fn)
 (get (om-symb->om? fn) :prime))

(cl:defun card (SC)
"returns the cardinality of SC"
 (length (prime SC)))

(cl:defun fn (sc) 
(om?-symb->om (car (gethash (sort (remove-duplicates sc) #'<) *all-possible-chroma-subsets-hash*))))

(defun card-sublists (card sequence)
 (let* ((len (length sequence))
        (indexes (remove-if #'(lambda (n) (< n card))
                           (all-values (an-integer-between 1 len)))))
  (mapcar #'(lambda (i) (subseq sequence 0 i)) indexes)))

(cl:defun group-setclasses-by-cardinality (setclass-list)
  (let ((alist '()))
    (dolist (sc setclass-list)
      (let* ((c (card sc))
             (cell (assoc c alist)))
        (if cell
            (push sc (cdr cell))
            (push (cons c (list sc)) alist))))
    (mapcar #'reverse
            (mapcar #'cdr
                    (sort alist #'< :key #'car)))))

(defun list-of-intervals-mod12v (list)
 (mapcarv (lambda (x y)
  (modv (-v y x) 12)) (cdr list) list))  

(defun pc-set-transpositions (prime-form)
 (let ((v (list-of-integers-betweenv (length prime-form) 0 11)))
(assert! (equalv (list-of-intervals-mod12v v)
                 (list-of-intervals-mod12v prime-form)))
(all-values (solution v (static-ordering #'linear-force)))))

(defun fn-transpositions (fn-list)
(let ((transpositions '()))
 (dolist (fn fn-list)
  (setf transpositions (append (pc-set-transpositions (prime fn))
                                transpositions)))
(nreverse transpositions)))

(defun all-subsets (fn card-min card-max &optional forbid)
 (let* ((prime (prime fn))
        (subsets (all-values 
                  (apply (lambda (x) 
                          (if (and (>= (length x) card-min) 
                                  (<= (length x) card-max))
                              x
                            (fail)))
                  (list (a-subset-of prime))))))
  (if forbid
     (remove-if #'(lambda (item) (member item forbid)) 
                   (om-symb->om? (remove-duplicates 
                  (mapcar #'fn subsets))))
    (om-symb->om? (remove-duplicates  (mapcar #'fn subsets))))))

(defun set-complement (pcs)
 (one-value (solution (a-set-complement-ofv pcs) (static-ordering #'linear-force))))

(defun supersets (SC card)
 (let* ((prime (prime SC))
        (set-diff (set-complement prime))
        (s-space (list-of-members-ofv (- card (card SC)) set-diff)))
(assert! (apply #'<v s-space))
(assert! (all-differentv s-space))
  (remove-duplicates
   (mapcar #'(lambda (l) (fn (append prime l)))
    (all-values (solution s-space (static-ordering #'linear-force)))))))
                                                
;==============
(cl:defun calc-6vect (SC)
  (let ((res (make-list 6 :initial-element 0))
        (prime (prime SC))
        temp int ref)
    (while (cdr prime)
      (setq ref (pop prime))
      (setq temp prime)
      (while temp
        (setq int (- (first temp) ref))
        (when (> int 6) (setq int (- 12 int)))
        (setf (nth (1- int) res) (1+ (nth (1- int) res)))
        (pop temp)))
    res))
	
(cl:defun store-SC-icvectors ()
  (dolist (SCs *all-SC-names*)
    (dolist (SC SCs)
      (setf (get SC :icv) (calc-6vect SC)))))

(store-SC-icvectors)
;==============

(cl:defun ICV (SC)
"returns the interval-class vector (ICV) of SC"
  (get (om-symb->om? SC) :icv))

(cl:defun make-set (l)
  (let (lst)
    (while l (push (/ (mod (pop l) 1200) 100) lst))
    (sort (delete-duplicates  lst) #'<)))

(cl:defun make-pc-set (l)
  (let (lst)
    (while l (push (pop l) lst))
    (sort (delete-duplicates  lst) #'<)))

(cl:defun make-midiset (l)
  (let (lst)
    (while l (push (mod (pop l) 12) lst))
    (sort (delete-duplicates  lst) #'<)))

; ============ ;
; CONSTRAINTS
; ============ ;

(defun fnv (vars)
 (let* ((variables-mod12v (mapcar #'(lambda (el) (funcallv #'mod el 12)) vars))
        (no-dupv (remove-duplicatesv variables-mod12v :test #'=))
        (sortedv (sortv no-dupv #'<))
        (fnv (carv (funcallv #'gethash sortedv *all-possible-chroma-subsets-hash*))))
  fnv))

(defun fnv-pcs (vars)
 (let* ((no-dupv (remove-duplicatesv vars :test #'=))
        (sortedv (sortv no-dupv #'<))
        (fnv (carv (funcallv #'gethash sortedv *all-possible-chroma-subsets-hash*))))
  fnv))

(defun member-of-setclassv (vars list)
 (let* ((fn-max-card (apply #'max (mapcar #'card list)))
        (card-sublists (card-sublists fn-max-card vars))
        (z (a-booleanv))
        (all-members? '()))
 (dolist (sublist card-sublists)
  (let ((fnv (fnv sublist)))
   (push (memberv fnv list) all-members?)))
  (assert! (equalv z (apply #'andv all-members?)))
  z))

(defun member-of-setclassv-pcs (vars list)
 (let* ((fn-max-card (apply #'max (mapcar #'card list)))
        (card-sublists (card-sublists fn-max-card vars))
        (z (a-booleanv))
        (all-members? '()))
 (dolist (sublist card-sublists)
  (let ((fnv-pcs (fnv-pcs sublist)))
   (push (memberv fnv-pcs list) all-members?)))
  (assert! (equalv z (apply #'andv all-members?)))
  z))

(defun random-member-of-setclassv (vars list)
(let* ((set-classes (group-setclasses-by-cardinality list))
       (variables (copy-list vars))
       (z (a-booleanv))
       (booleans '()))
    (dolist (scs set-classes)
    (push (member-of-setclassv variables scs) booleans))
    (assert! (equalv z (apply #'orv (spermut-random booleans))))
    z))

(defun random-member-of-setclassv-pcs (vars list)
(let* ((set-classes (group-setclasses-by-cardinality list))
       (variables (copy-list vars))
       (z (a-booleanv))
       (booleans '()))
    (dolist (scs set-classes)
    (push (member-of-setclassv-pcs variables scs) booleans))
    (assert! (equalv z (apply #'orv (spermut-random booleans))))
    z))

(defun same-pcsetv? (vars1 vars2)
 (let  ((no-dupv1 (remove-duplicatesv vars1 :test #'=))
        (no-dupv2 (remove-duplicatesv vars2 :test #'=)))
 (set-equalv no-dupv1 no-dupv2)))

(defun pcv? (vars fn)
 (let* ((card-sublists (card-sublists (card fn) vars))
        (z (a-booleanv))
        (all-equal? '()))
 (dolist (sublist card-sublists)
  (let ((fnv (fnv sublist)))
   (push (equalv fnv fn) all-equal?)))
  (assert! (equalv z (apply #'andv all-equal?)))
  z))
 
 (defun pcv-pcs? (vars fn)
 (let* ((card-sublists (card-sublists (card fn) vars))
        (z (a-booleanv))
        (all-equal? '()))
 (dolist (sublist card-sublists)
  (let ((fnv-pcs (fnv-pcs sublist)))
   (push (equalv fnv-pcs fn) all-equal?)))
  (assert! (equalv z (apply #'andv all-equal?)))
  z))

(defun subv? (vars fn card-min card-max forbid)
 (let* ((all-subsets (all-subsets fn
                                 (if card-min card-min 1)
                                 (if card-max card-max (card fn))
                                 (if forbid forbid '(nil) )))
       (set-classes (spermut-random (group-setclasses-by-cardinality all-subsets)))
       (variables (copy-list vars))
       (z (a-booleanv))
       (booleans '()))
    (dolist (scs set-classes)
    (push (member-of-setclassv variables scs) booleans))
    (assert! (equalv z (apply #'orv booleans)))
    z))

(defun subv-pcs? (vars fn card-min card-max forbid)
 (let* ((all-subsets (all-subsets fn
                                 (if card-min card-min 1)
                                 (if card-max card-max (card fn))
                                 (if forbid forbid '(nil) )))
       (set-classes (spermut-random (group-setclasses-by-cardinality all-subsets)))
       (variables (copy-list vars))
       (z (a-booleanv))
       (booleans '()))
    (dolist (scs set-classes)
    (push (member-of-setclassv-pcs variables scs) booleans))
    (assert! (equalv z (apply #'orv booleans)))
    z))

(defun a-set-complement-ofv (list)
 (let ((v (list-of-integers-betweenv (-v 12 (length list)) 0 11)))
  (assert! (eqv (intersectionv v list) nil))
  (assert! (apply #'<v v))
  (assert! (all-differentv v))
 (value-of v)))
