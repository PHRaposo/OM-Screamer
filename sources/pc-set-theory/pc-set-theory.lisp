;===============================================
;===============================================

;;; FROM

;;; PWConstraints by Mikael Laurson (c), 1995
	
;;; AND OMCS 1.4

;===============================================
;===============================================
(in-package :om?)
;===============================================

(defparameter *SC-data-file* (namestring
                              (make-pathname :directory (pathname-directory *load-pathname*)
                                             :name "SCs-data.lisp")))

(defvar *all-possible-chroma-subsets-hash* (make-hash-table :test #'equalv))

(defun fill-possible-chroma-subsets-hash ()
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

;===============================================
;===============================================

;;; OM-SCREAMER FUNCTIONS

;;; by Paulo Raposo

;===============================================
;===============================================

 ; =============================================================== ;
   ;;; CONVERSION: OM SYMBOL <-> OM? SYMBOL
; =============================================================== ;

(defun om?-symb->om (om?-sym)
   (let ( (write-symbol (lambda (x)  (car (om::list! (intern (string-upcase x) :om))))))
    (if (atom om?-sym)
         (funcall write-symbol om?-sym)
    (mapcar write-symbol om?-sym)))) 

(defun om-symb->om? (om-sym)
   (let ( (read-symbol (lambda (x) (car (om::list! (intern (string-upcase x) :om?))))))
    (if (atom om-sym)
         (funcall read-symbol om-sym)
    (mapcar read-symbol om-sym)))) 

; =============================================================== ;

(defun fnv (vars)
(firstv (s::funcallgv #'gethash (?::make-mcsetv vars) *all-possible-chroma-subsets-hash*)))

(defun member-of-setclassv (vars list)
 (memberv (fnv vars) list))

(defun fnv-pcs (vars)
(firstv (s::funcallgv #'gethash (?::make-setv vars) *all-possible-chroma-subsets-hash*)))

(defun member-of-setclassv-pcs (vars list)
 (memberv (fnv-pcs vars) list))

(defun same-pcsetv? (vars1 vars2)
(let ((z (a-booleanv)))
(assert! (eqv z (set-equalv (?::list-elements-ofv vars1) (?::list-elements-ofv vars2))))
z))

(defun list-of-intervals-mod12v (list)
 (mapcarv (lambda (x y)
  (modv (-v y x) 12)) (cdr list) list))  

(defun pc-set-transpositions (prime-form)
 (let ((v (list-of-integers-betweenv (length prime-form) 0 11)))
(assert! (equalv (list-of-intervals-mod12v v)
                          (list-of-intervals-mod12v prime-form)))
(all-values (solution v (static-ordering #'linear-force)))))

(defun get-card (fn) ;;;fn must be a string ==> (string fn)
 (if (equal (aref fn 2) '#\-)
     (let ((digits-list (concatenate 'list (list (digit-char-p (aref fn 0)) (digit-char-p (aref fn 1))))))
	 (+ (* (first digits-list) 10) (second digits-list)))
	 (digit-char-p (aref fn 0))))
	 
(defun prime (fn)
(get (om-symb->om? fn) :prime))

(defun fn (sc) 
(om?-symb->om (car (gethash (sort  (remove-duplicates sc) #'<) *all-possible-chroma-subsets-hash*))))

(defun pcv? (vars fn)
 (equalv (fnv vars) fn))
 
(defun all-subsets (fn card-min card-max &optional forbid)
 (let* ((prime (prime fn))
         (subsets (all-values 
                         (apply (lambda (x) 
                                     (if (and (>= (length x) card-min) 
                                                 (<= (length x) card-max))
                                         x
                                        (fail)))
                         (list (?::a-subset-of prime))))))
(if forbid
   (remove-if #'(lambda (item) (member item forbid)) 
                       (om-symb->om? (remove-duplicates 
                        (mapcar #'fn subsets))))
   (om-symb->om? (remove-duplicates  (mapcar #'fn subsets))))
))

(defun subv? (vars fn card-min card-max forbid)
 (memberv (fnv vars) 
          (all-subsets fn
          (if card-min card-min 1)
          (if card-max card-max (get-card (string fn)))
          (if forbid forbid '(nil) ))))

(defun a-set-complement-ofv (list)
 (let ((v (list-of-integers-betweenv (-v 12 (length list)) 0 11)))
  (assert! (notv (intersectionv v list)))
  (assert! (apply #'<v v))
  (assert!-all-differentv v)
 (value-of v)))
 
(defun set-complement (pcs)
 (one-value (solution (a-set-complement-ofv pcs) (static-ordering #'linear-force))))

(defun supersets (SC card)
 (let* ((prime (prime SC))
        (set-diff (set-complement prime))
        (s-space (list-of-members-ofv (- card (card SC)) set-diff)))
(assert! (apply #'<v s-space))
(assert!-all-differentv s-space)
  (remove-duplicates
   (mapcar #'(lambda (l) (fn (append prime l)))
    (all-values (solution s-space (static-ordering #'linear-force)))))))
                                                
;==============
(defun calc-6vect (SC)
  (let ((res (make-list 6 :initial-element 0))
        (prime (prime SC))
        temp int ref)
    (om::while (cdr prime)
      (setq ref (pop prime))
      (setq temp prime)
      (om::while temp
        (setq int (- (first temp) ref))
        (when (> int 6) (setq int (- 12 int)))
        (setf (nth (1- int) res) (1+ (nth (1- int) res)))
        (pop temp)))
    res))
	
(defun store-SC-icvectors ()
  (dolist (SCs *all-SC-names*)
    (dolist (SC SCs)
      (setf (get SC :icv) (calc-6vect SC)))))

(store-SC-icvectors)
;==============
(defun card (SC)
"returns the cardinality of SC"
  (length (prime SC)))

;(time (repeat 10000 (card '12-1)))

(defun ICV (SC)
"returns the interval-class vector (ICV) of SC"
  (get (om-symb->om? SC) :icv))

(defun make-set (l)
  (let (lst)
    (om::while l (push (/ (mod (pop l) 1200) 100) lst))
    (sort (delete-duplicates  lst) #'<)))

;===============================================
;;; OM METHODS 
;===============================================
;===> CONSTRAINTS 

(om::defmethod! member-of-scv? ((vars t) (sc-list list) (mode string))  
  :initvals '((nil) (om::3-11a om::3-11b) "midics") 
:indoc '("list of screamer variables" "list of set-classes<fn>" "pcs or midics") 
  :doc "Constraint a list of Screamer variables to be all members of a list of set-classes in Forte notation."
:menuins '((2 (("midics" "midics") ("pcs" "pcs"))))
    :icon 487
(if (equal mode "midics")
    (member-of-setclassv vars (om-symb->om? sc-list))
    (member-of-setclassv-pcs vars (om-symb->om? sc-list))))

(om::defmethod! set-classpv? ((vars t) (pc-set list))  
  :initvals '((nil) (0 4 7)) 
:indoc '("list of screamer variables => midics" "fn or integers") 
  :doc "Constraint a list of Screamer variables <integers-between 0 11> to be all members of the selected pc-set <list of integers>."
    :icon 487
  (pcv? vars (car (gethash (sort  (remove-duplicates pc-set) #'<) *all-possible-chroma-subsets-hash*))))

(om::defmethod! set-classpv? ((vars t) (pc-set symbol))  
  :initvals '((nil) 'om::|3-11B|) 
:indoc '("list of screamer variables => midics" "fn or integers") 
  :doc "Constraint a list of Screamer variables <integers-between 0 11> to be all members of the selected pc-set <list of integers>."
    :icon 487
  (pcv? vars (om-symb->om? pc-set))) 

(om::defmethod! sub-setpv? ((vars t) (pc-set list) &optional (card-min nil) (card-max nil) (forbid nil))  
  :initvals '((nil) (0 1 3 4 6 9) nil nil nil) 
:indoc '("list of screamer variables => (integers-betweenv 0 11)" "fn or integers" "integer" "integer" "list of fns") 
  :doc "Constraint a list of Screamer variables <integers-between 0 11> to be all subsets of the selected pc-set.
Optional arguments: 
 <CARD-MIN> minimun cardinality.
 <CARD-MAX> maximun cardinality.
 <FORBID> list of set classes in Forte notation [ex.: (3-11a 3-11b 3-4b)]."
    :icon 487
(subv? vars pc-set card-min card-max (om-symb->om? forbid)))   

(om::defmethod! sub-setpv? ((vars t) (pc-set symbol) &optional (card-min nil) (card-max nil) (forbid nil))  
  :initvals '((nil) 'om::|6-27A| nil nil nil) 
:indoc '("list of screamer variables => (integers-betweenv 0 11)" "fn or integers" "integer" "integer" "list of fns") 
  :doc "Constraint a list of Screamer variables <integers-between 0 11> to be all subsets of the selected pc-set.
Optional arguments: 
 <CARD-MIN> minimun cardinality.
 <CARD-MAX> maximun cardinality.
 <FORBID> list of set classes in Forte notation."
    :icon 487
(subv? vars pc-set card-min card-max (om-symb->om? forbid)))   

;===============================================
;;;===> SCS

(om::defmethod! SC-subsets ((fn symbol) &optional (card-min nil) (card-max nil) (forbid nil))
  :initvals '('om::|6-27A| nil nil nil)
:indoc '("fn symbol" "integer" "integer") 
  :doc "Return all subsets."
    :icon 487
(om?-symb->om  (all-subsets (om-symb->om? fn)
                                     (if card-min card-min 0)
                                     (if card-max card-max (get-card (string fn)))
                                     (if forbid forbid nil))))

(om::defmethod! SC-subsets ((fn list) &optional (card-min nil) (card-max nil) (forbid nil))
  :initvals '('(om::|6-27A| om::|6-27B|) nil nil nil)
:indoc '("fn symbol" "integer" "integer") 
  :doc "Return all subsets."
    :icon 487
(remove-duplicates 
 (om::flat 
  (mapcar #'(lambda (x)
             (om?-symb->om  (all-subsets (om-symb->om? x)
                            (if card-min card-min 0)
                            (if card-max card-max (get-card (string x)))
                            (if forbid forbid nil))))
             fn)
 )
 :test #'equal))

(om::defmethod! SCs-card ((card integer))
  :initvals '(6)
:indoc '("integer" ) 
:menuins '((0 (("1" 1)  ("2" 2) ("3" 3) ("4" 4) ("5" 5) ("6" 6) ("7" 7) ("8" 8) ("9" 9) ("10" 10) ("11" 11) ("12" 12))))
  :doc "Return all fn symbols."
    :icon 487
(om?-symb->om
 (case card 
 (1 card1)
 (2 card2)
 (3 card3 )
 (4 card4 )
 (5 card5 )
 (6 card6 )
 (7 card7 )
 (8 card8 )
 (9 card9 )
 (10 card10 )
 (11 card11 )
 (12 card12))))

(om::defmethod! SC+off ((midics list)) 
  :initvals '((6000 6100))
  :indoc '("midics")
  :icon 487
  :doc  "returns a list containing the SC-name and the offset 
(i.e. the transposition relative to the prime form of the SC) of 
 midis (a list of midic-values), midics can also be a list of lists 
 of midics in which case SC+off returns the SCs with offsets 
 for each midic-value sublist."
  (if (atom (car midics))
      (let ((res (gethash (make-set midics) *all-possible-chroma-subsets-hash*)))
         (om::x-append (om?-symb->om (first res)) (second res)))
    (let (res)
      (dolist (midics-l midics)
        (push (gethash  (make-set midics-l) *all-possible-chroma-subsets-hash*) res))
      (mapcar #'(lambda (x)
       (om::x-append (om?-symb->om (first x)) (second x))) (nreverse res)))))

(om::defmethod! SC-name ((midics list)) 
  :initvals '((6000 6100))
  :indoc '("midics")
  :icon 487
  :doc  "returns a list containing the SC-name and the offset 
(i.e. the transposition relative to the prime form of the SC) of 
 midis (a list of midic-values), midics can also be a list of lists 
 of midics in which case SC+off returns the SCs with offsets 
 for each midic-value sublist."
  (if (atom (car midics))
      (om?-symb->om (car (gethash (make-set midics) *all-possible-chroma-subsets-hash*)))
    (let (res)
      (dolist (midics-l midics)
        (push (car (gethash  (make-set midics-l) *all-possible-chroma-subsets-hash*)) res))
      (mapcar #'om?-symb->om (nreverse res)))))

(om::defmethod! sub/supersets ((SC t) (card number))
  :initvals '('om::4-z15a 9)
  :indoc '("SC" "card")
  :icon 487
  :doc "returns all subset classes of SC (when card is less than the cardinality of SC)
or superset classes (when card is greater than the cardinality of SC) 
of cardinality card."
  (if (= (card (om-symb->om? SC)) card)
    SC
    (if (> (card (om-symb->om? SC)) card)
      (om?-symb->om (all-subsets (om-symb->om? SC) card card nil))
      (supersets (om-symb->om? SC) card))))

(om::defmethod! SC-info ((mode symbol) (SC om::t))
  :initvals '(:prime 'om::4-z15a)
:indoc '("mode" "SC" ) 
:menuins '((0 (("prime" :prime) ("icv" :icv)  ("member-sets" :member-sets) ("complement-pcs" :complement-pcs))))
  :doc "Returns the selected info (prime-form, interval class vector, members-sets or complement) about an SC."
    :icon 487
(cond 
((equal mode :prime) (prime SC))
((equal mode :icv) (icv SC))
((equal mode :member-sets) (pc-set-transpositions (prime SC)))
((equal mode :complement-pcs) (set-complement  (prime SC)))
(t (progn (om::om-message-dialog "Please select a valid mode (:prime, :icv, :member-sets or :complement-pcs).") (om::om-abort)))))

