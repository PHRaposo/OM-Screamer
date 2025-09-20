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

(defvar *normal-order-hash* (make-hash-table :test #'equal)
 "Stores all normal order values for all set classes.")

(cl:defun fill-normal-order-hash () 
  (let (normal-order-list) 
    (with-open-file (in *normal-order-data-file* :direction :input)
      (setq normal-order-list (eval (read in))))
    (clrhash *normal-order-hash*)
    (dolist (n-ord normal-order-list) 
      (setf (gethash (first n-ord) *normal-order-hash*)  
            (second n-ord)))))

(fill-normal-order-hash)

(cl:defun get-n-ord (pcs)
 (gethash (make-pc-set pcs) *normal-order-hash*))

(cl:defun get-n-ord-midi (midis)
 (gethash (make-midiset midis) *normal-order-hash*))

(cl:defun get-n-ord-midic (midics)
 (gethash (make-set midics) *normal-order-hash*))

 (defun n-ordv (vars)
  (let* ((variables-mod12v (mapcar #'(lambda (el) (funcallv #'mod el 12)) vars))
        (no-dupv (remove-duplicatesv variables-mod12v :test #'=))
        (sortedv (sortv no-dupv #'<))
        (normal-orderv (funcallv #'gethash sortedv *normal-order-hash*)))
    normal-orderv))
 
 (defun n-ordv-pcs (vars)
  (let* ((no-dupv (remove-duplicatesv vars :test #'=))
         (sortedv (sortv no-dupv #'<))
         (normal-orderv (funcallv #'gethash sortedv *normal-order-hash*)))
    normal-orderv))
 