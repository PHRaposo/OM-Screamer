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

(defvar *normal-order-hash* (make-hash-table :test #'equalv))

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
 (s::funcallgv #'gethash (?::make-mcsetv vars) *normal-order-hash*))
 
 #|
 (let ((normal-orderv (s::funcallgv #'gethash (?::make-mcsetv vars) *normal-order-hash*))
       (old-p-variables om::*p-variables*)
 	  )
  (setf om::*p-variables* (om::x-append old-p-variables (list vars) normal-orderv))
  normal-orderv))
  |#
  
 (defun n-ordv-pcs (vars)
 (s::funcallgv #'gethash (?::make-setv vars) *normal-order-hash*))
 
 #|
 (let ((normal-orderv (s::funcallgv #'gethash (?::make-setv vars) *normal-order-hash*))
       (old-p-variables om::*p-variables*)
 	  )
  (setf om::*p-variables* (om::x-append old-p-variables (list vars) normal-orderv))
  normal-orderv))
 |#
 
;===============================================

(om::defmethod! normal-order ((input list) (mode string))  
  :initvals '((6000 6700 6400 7100) "midics") 
:indoc '("list of midicents or pitch classes" "pcs or midics") 
  :doc "Returns the normal order [list of integers] of the given set [midicents or pitch classes]."
:menuins '((1 (("midics" "midics") ("pcs" "pcs"))))
    :icon 487
(if (equal mode "midics")
    (om::list! (get-n-ord (om::mc->pcv input)))
    (om::list! (get-n-ord input))))
	
(om::defmethod! normal-orderv ((input t) (mode string))  
  :initvals '(t "midics") 
:indoc '("list of midicents or pitch classes" "pcs or midics") 
  :doc "Returns an Screamer variable constrained to be the normal order of a given set [midicents or pitch classes]. The input can be variables or non variables."
:menuins '((1 (("midics" "midics") ("pcs" "pcs"))))
    :icon 487
(if (equal mode "midics")
    (if (every #'screamer::bound? input)
        (om::list! (get-n-ord (om::mc->pcv input)))
        (n-ordv input))
	(if (every #'screamer::bound? input)
	    (om::list! (get-n-ord input))
        (n-ordv-pcs input))))
	
;===============================================
