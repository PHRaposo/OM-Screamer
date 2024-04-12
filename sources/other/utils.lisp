(in-package :om-screamer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SYSTEM FUNCTIONS - OM FUNCTIONS - PW FUNCTIONS

(defun function-lambda-list (fn)
(system::function-lambda-list fn))

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

;----------------EXPAND LIST

(defvar *valid-expand-chars* '(#\* #\_))

(defun is-in (list chars)
(let (res)
  (dolist (ch chars res)
    (if (setq res (member ch list :test #'char=)) (return res)))))


(cl::defmethod expand-lst ((list list))
(and list
     (let ((lists (list! list)) result)
       (while lists
              (let ((next-elem (pop lists)))
                (cond 
                 ((symbolp next-elem)
                  (let* ((form (coerce (format () "~A" next-elem) 'list))
                         (from-char (is-in form *valid-expand-chars*))
                         (char-symb (car from-char))
                         (third (cdr from-char))
                         (int (butlast form (length from-char)))
                         up-to)
                    (cond 
                     ((and (not third) char-symb (char= #\* char-symb) int
                           (numberp (setq int (read-from-string (coerce int 'string)))))
                      (push (apply #'append
                                   (make-list int
                                              :initial-element 
                                              (expand-lst (pop lists))))
                            result))
                     ((and char-symb (char= #\_ char-symb) third
                           (numberp (setq int (read-from-string (coerce int 'string)))))
                      (if (setq from-char (member #\s  ;;;[CR, 14/01/99] #\S
                                                  third :test #'char=))
                        (progn (setq up-to (butlast third (length from-char))
                                     char-symb (car from-char) third (cdr from-char))
                               (if (and char-symb 
                                        (char= #\s ;;;[CR, 14/01/99] #\S
                                               char-symb)
                                        (or (null third)
                                            (numberp 
                                             (setq third (read-from-string (coerce third 'string)))))
                                        (numberp 
                                         (setq up-to (read-from-string (coerce up-to 'string)))))
                                 (push (arithm-ser int up-to (or third 1)) result)
                                 (push (list next-elem) result)))
                        (progn
                          (setq up-to (read-from-string (coerce third 'string)))
                          (push (arithm-ser int up-to 1) result))
                        ))
                     (t (push (list next-elem) result)))))
                 ((consp next-elem)
                  (push (list (expand-lst next-elem)) result))
                 (t (push (list next-elem) result)))))
       ;(apply #'append (nreverse result))  ;;; does not support long lists
       (loop for item in (nreverse result) append item))))
	   
(cl::defmethod arithm-ser ((begin number) (end number) (step number) &optional (nummax MOST-POSITIVE-FIXNUM))
 (if (plusp step)
   (loop for i from begin to end by step
         for counter from 1 to nummax
         collect i)
   (loop for i from begin downto end by (abs step)
         for counter from 1 to nummax
         collect i)))	
	   		  	  
;=================
;safe random
(defun om-random-value (num)
(if (= num 0) 0
(if (< num 0)
  (- (random (- num)))
  (random num))))

(cl::defmethod x-append ((l1? list) (l2? list) &rest lst?)
(apply 'append l1? l2? (mapcar #'list! lst?)))

(cl::defmethod x-append ((l1? t) (l2? list) &rest lst?)
(apply 'append (list l1?) l2? (mapcar #'list! lst?)))

(cl::defmethod x-append ((l1? list) (l2? t) &rest lst?)
(apply 'append l1? (list l2?) (mapcar #'list! lst?)))

(cl::defmethod x-append ((l1? t) (l2? t) &rest lst?)
(apply 'append (list l1?) (list l2?) (mapcar #'list! lst?)))  

  ;-----------FLAT

(defun rev-flat (lst)  
(let ((l ()))
  (while lst
         (if (not (consp (car lst)))
           (newl l (nextl lst))
           (setq l (nconc (rev-flat (nextl lst)) l))))
  l ))

(defun lo-flat (list) 
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
	 
(cl::defun npermut-random (list)
   (let ((result ()) 
           (length (length list)) 
           (list (copy-list list)))
    (nconc list list)
    (loop for i from 0 to length do
              (setq list (nthcdr (om-random-value length) list)
            result (rplacd (prog1 (cdr list) (rplacd list (cddr list))) result)
            length (1- length)))
    result))

(defun permut-random (list)
  (if (or (null list) (= (length list) 1)) list
      (npermut-random (copy-list list))))
	  
(defun string-to-list (string &optional (separator " "))
  (when string
    (multiple-value-bind (token rest)
        (string-until-char string separator)
      (cons token (string-to-list rest separator)))))

(defun string-until-char (string char)
(let ((index (search char string)))
  (if index (values (subseq string 0 index) (subseq string (+ index 1)))
      (values string nil))))
	  
(cl::defmethod posn-match ((list list) (positions list))
(do-posn-match list (expand-lst positions)))

(cl::defmethod posn-match ((list list) (positions integer) )
(nth positions list ))	
	
(cl::defmethod do-posn-match ((self list) (positions list))
  (cond 
   ((numberp positions) (nth positions self))
   ((listp positions)
    (loop for pos in positions
          collect (do-posn-match self pos)))))

(cl::defmethod do-posn-match ((self list) (positions number))
   (nth positions self))

(defun list! (thing) (if (listp thing) thing (list thing)))

 ;;;;;;;;;;;;;;;;;;;;;;;;;tree2ratio;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;;;from jean bresson

 (defun tree-to-ratios (list)
   (loop for mesure in (cadr list) collect
         (let* ((signature (car mesure))
                (vals (cadr mesure))
                (ratios (mesure-ratios vals)))
           (om/
            (om* ratios (car signature))
            (cadr signature)))))

 (defun mesure-ratios (list)
  (let ((div (round (loop for elt in list sum (abs (if (listp elt) (car elt) elt))))))
     (flat (loop for elt in list 
                 collect (if (listp elt)
                           (om* (/ (round (car elt)) div) (mesure-ratios (cadr elt)))
                           (/ (round elt) div)))
 )))

 ;;this is to get the floats working correctly
 ;;this is me
 (defun get-s-from-meas (tree)
   (if (atom tree)
     tree
    (mapcar 'get-s-from-meas (second tree))))

 (defun get-s-by-mes (tree)
   (loop 
     for elt in (cadr tree)
     collect (flat (get-s-from-meas elt))))

 (defun correct-measurefloats (tree)
   (let* (res
          (pulses (flat (get-s-by-mes tree)))
          (ratios (flat (tree-to-ratios tree))))
     (loop for p in pulses do 
           (if (floatp p)
               ;;; tie with previous ratio
               (if res 
                   ;; not the first pulse
                   (setf (car res) (+ (or (car res) 0) (pop ratios)))
                 ;; first pulse
                 (setf res (list (- (pop ratios)))))
             ;;; new ratio
             (push (pop ratios) res)
             ))
     (reverse res)))

 (defmethod! tree2ratio ((tree t))
   :initvals '((? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1)))))
   :indoc '("a rythm tree")
   :icon 254
   :doc "
 Converts <tree> into a list of ratio where 
 1/4 is a quarter note, 1/8 is an eight note etc.
 "
   (correct-measurefloats tree))
	  
;;;---------EOF
	  