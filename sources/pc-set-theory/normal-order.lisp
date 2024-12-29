;===============================================
;===============================================

;;; NORMAL ORDER

;;; by Paulo Raposo

;===============================================
;===============================================

(in-package :om-screamer)

(defparameter *normal-order-data-file* (namestring
                              (make-pathname :directory (pathname-directory *load-pathname*)
                                             :name "normal-order-data.lisp")))

(screamer::defvar-compile-time *normal-order-hash* (make-hash-table :test #'equalv)
 "Stores all normal order values for all set classes.")

(defun fill-normal-order-hash () 
  (let (normal-order-list) 
    (with-open-file (in *normal-order-data-file* :direction :input)
      (setq normal-order-list (eval (read in))))
    (clrhash *normal-order-hash*)
    (dolist (n-ord normal-order-list) 
      (setf (gethash (first n-ord) *normal-order-hash*)  
            (second n-ord)))))

(fill-normal-order-hash)

(defun get-n-ord (pcs)
 (gethash (sort (remove-duplicates pcs) #'<) *normal-order-hash*))

 (defun n-ordv (vars)
 (s::funcallgv #'gethash (?::make-midisetv vars) *normal-order-hash*))
 
 (defun n-ordv-pcs (vars)
 (s::funcallgv #'gethash (?::make-setv vars) *normal-order-hash*))
 
;===============================================

(om::defmethod! normal-order ((input list) (mode string))  
  :initvals '((6000 6700 6400 7100) "midic") 
:indoc '("list of midics or pitch classes" "pc or midic") 
  :doc "Returns the normal order [list of integers] of the given set [midicents or pitch classes]."
:menuins '((1 (("midic" "midic") ("pc" "pc"))))
    :icon 486 ;487
(if (equal mode "midic")
    (om::list! (get-n-ord (om::mc->pcv input)))
    (om::list! (get-n-ord input))))
	
(om::defmethod! normal-orderv ((input t) (mode string))  
  :initvals '(t "midi") 
:indoc '("list of midicents or pitch classes" "pcs or midics") 
  :doc "Returns an Screamer variable constrained to be the normal order of a given set [midi values or pitch classes]. The input can be variables or non variables."
:menuins '((1 (("midi" "midi") ("pc" "pc"))))
    :icon 486 ;487
(if (equal mode "midi")
    (if (every #'screamer::bound? input)
        (om::list! (get-n-ord (om::m->pcv input)))
        (n-ordv input))
	(if (every #'screamer::bound? input)
	    (om::list! (get-n-ord input))
        (n-ordv-pcs input))))
	
;===============================================
