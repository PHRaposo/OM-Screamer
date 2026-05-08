;; Copyright (c) 2025 Paulo Henrique Raposo

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

;;;; =========================================================================
;;;; CONSTRAINT-PATCH CLASSES
;;;; =========================================================================
;;; The body of a ConstraintPatch is wrapped with SCREAMER::ASSERT! at
;;; compile-patch time (via PUSH-ASSERT!-INTO), so the
;;; specialized TRANSFORM-ASSERT! dispatch (assert!-<v, assert!-memberv,
;;; assert!-andv, ...) fires at compile time instead of materializing a
;;; boolean variable at runtime.

(defclass ConstraintPatch (OMPatch) ()
  (:metaclass omstandardclass))

(defclass ConstraintPatchAbs (ConstraintPatch OMPatchAbs) ()
  (:metaclass omstandardclass))

(defclass ConstraintLispPatch (OMLispPatch) ()
  (:metaclass omstandardclass))

(defclass ConstraintLispPatchAbs (ConstraintLispPatch OMLispPatchAbs) ()
  (:metaclass omstandardclass))

(defmethod get-object-insp-name ((self ConstraintPatch)) "constraint patch")
(defmethod get-object-insp-name ((self ConstraintLispPatch)) "constraint lisp")


;;; ---- Assert! sink walker --------------------------------------------------

(defun push-assert!-into (form)
  "Return a form equivalent to (SCREAMER::ASSERT! FORM) with the assert!
pushed into tail positions of Common Lisp control-flow forms.

TRANSFORM-ASSERT! (invoked by the ASSERT! macro) only dispatches on the
top-level form. Without this walker, a CS body like
  (if (and v0 v1) (<v v0 v1) t)
would fall through to (assert!-true (if ...)), losing the specialized
compile-time dispatch for <v / memberv / =v / notv / etc.

Handles (body-in-tail, recurse on last body form): IF, WHEN, UNLESS, COND,
PROGN, LET, LET*, LABELS, FLET, CASE, TYPECASE, ETYPECASE, DESTRUCTURING-BIND,
and the
screamer side-effect wrappers LOCAL, GLOBAL, WHEN-FAILING (whose body has
progn-like tail semantics).

Pass-through: standalone (ASSERT! X) returns unchanged, respecting explicit
user intent and avoiding double-wrap (inner assert! returns nil -> outer
asserts nil -> FAIL).

Literal T is a protocol signal meaning 'CS applied via side-effect; no outer
assertion needed' -- returned unchanged. Used by ConstraintPatch
special-lambda-value wrappers and by users writing multiple explicit
(assert! ...) forms ending with T to skip wrap of the last one.

Signals error: PROG1, MULTIPLE-VALUE-PROG1, UNWIND-PROTECT (return value is
the first arg, not the tail -- wrapping breaks semantics); MULTIPLE-VALUE-BIND
(multi-value decomposition is non-idiomatic in CS; refactor with LET*);
LOCALLY (declarations, irrelevant in CS); CCASE, CTYPECASE (interactive correction,
inappropriate in constraint code).

Everything else (CL AND/OR, screamer ANDV/ORV/MEMBERV/<V/etc., function
calls, atoms) is left for TRANSFORM-ASSERT! to handle normally."
  (cond
    ((eq form t) t)
    ((atom form) `(screamer::assert! ,form))
    ((eq (first form) 'if)
     (let ((c (second form)) (a (third form)))
       (if (null (cdddr form))
           `(if ,c ,(push-assert!-into a))
           `(if ,c ,(push-assert!-into a) ,(push-assert!-into (fourth form))))))
    ((eq (first form) 'when)
     (let ((c (second form)) (body (cddr form)))
       (if (null body)
           `(screamer::assert! ,form)
           `(when ,c ,@(butlast body) ,(push-assert!-into (car (last body)))))))
    ((eq (first form) 'unless)
     (let ((c (second form)) (body (cddr form)))
       (if (null body)
           `(screamer::assert! ,form)
           `(unless ,c ,@(butlast body) ,(push-assert!-into (car (last body)))))))
    ((eq (first form) 'cond)
     `(cond ,@(mapcar #'(lambda (clause)
                          (let ((c (first clause)) (body (rest clause)))
                            (if (null body)
                                clause
                                `(,c ,@(butlast body)
                                     ,(push-assert!-into (car (last body)))))))
                      (rest form))))
    ((eq (first form) 'progn)
     (let ((body (rest form)))
       (if (null body)
           `(screamer::assert! ,form)
           `(progn ,@(butlast body) ,(push-assert!-into (car (last body)))))))
    ((member (first form) '(let let*) :test #'eq)
     (let ((bindings (second form)) (body (cddr form)))
       (if (null body)
           `(screamer::assert! ,form)
           `(,(first form) ,bindings
              ,@(butlast body)
              ,(push-assert!-into (car (last body)))))))
    ((member (first form) '(labels flet) :test #'eq)
     (let ((defs (second form)) (body (cddr form)))
       (if (null body)
           `(screamer::assert! ,form)
           `(,(first form) ,defs
              ,@(butlast body)
              ,(push-assert!-into (car (last body)))))))
    ((member (first form) '(screamer::local screamer::global) :test #'eq)
     (let ((body (rest form)))
       (if (null body)
           `(screamer::assert! ,form)
           `(,(first form)
              ,@(butlast body)
              ,(push-assert!-into (car (last body)))))))
    ((eq (first form) 'screamer::when-failing)
     (let ((failing (second form)) (body (cddr form)))
       (if (null body)
           `(screamer::assert! ,form)
           `(screamer::when-failing ,failing
              ,@(butlast body)
              ,(push-assert!-into (car (last body)))))))
    ((member (first form) '(case typecase etypecase) :test #'eq)
     (let ((op (first form)) (key-expr (second form)) (clauses (cddr form)))
       `(,op ,key-expr
          ,@(mapcar #'(lambda (clause)
                        (let ((keys (first clause)) (body (rest clause)))
                          (if (null body)
                              clause
                              `(,keys ,@(butlast body)
                                 ,(push-assert!-into (car (last body)))))))
                    clauses))))
    ((eq (first form) 'destructuring-bind)
     (let ((pattern (second form)) (expr (third form)) (body (cdddr form)))
       (if (null body)
           `(screamer::assert! ,form)
           `(destructuring-bind ,pattern ,expr
              ,@(butlast body)
              ,(push-assert!-into (car (last body)))))))
    ((and (eq (first form) 'screamer::assert!) (null (cddr form)))
     form)
    ((member (first form)
             '(prog1 multiple-value-prog1 unwind-protect
               multiple-value-bind locally ccase ctypecase)
             :test #'eq)
     (error "push-assert!-into: ~S cannot be used inside constraints." (first form)))
    (t `(screamer::assert! ,form))))

;;;; =========================================================================
;;;; COMPILE-PATCH -- body wrapped with PUSH-ASSERT!-INTO
;;;; =========================================================================

(defmethod compile-patch ((self ConstraintPatch))
  (unless (compiled? self)
    (handler-bind
        ((error #'(lambda (c)
                    (when *msg-error-label-on*
                      (om-message-dialog
                       (string+ "Error while compiling constraint patch '"
                                (string (name self)) "' : "
                                (om-report-condition c))
                       :size (om-make-point 300 200))
                      (om-abort)))))
      (if (lisp-exp-p self)
          (compile-constraint-lisp-patch-fun self)
          (let* ((boxes        (boxes self))
                 (out-box      (find-class-boxes boxes 'OMout))
                 (temp-out-box (find-class-boxes boxes 'OMtempOut))
                 (self-boxes   (patch-has-temp-in-p self))
                 (in-boxes     (find-class-boxes boxes 'OMin))
                 (out-symb     (code self))
                 (oldletlist   *let-list*)
                 (oldlambdacontext *lambda-context*)
                 (pname        (or (name self) "constraint-patch"))
                 symbols body)
            (setf out-box   (list+ temp-out-box (sort out-box '< :key 'indice)))
            (setf in-boxes  (list+ self-boxes   (sort in-boxes '< :key 'indice)))
            (setf symbols   (mapcar #'(lambda (thein) (setf (in-symbol thein) (gensym))) in-boxes))
            (setf *let-list* nil)
            (let ((outs (mapcar #'(lambda (o) (gen-code o 0)) out-box)))
              (cond
                ((null outs)
                 (om-beep-msg (string+ "Constraint patch '" pname "' has no output. Using T as body."))
                 (setf body (push-assert!-into t)))
                ((cdr outs)
                 (om-beep-msg (string+ "Constraint patch '" pname
                                       "' has multiple outputs. Using only the first."))
                 (setf body (push-assert!-into (car outs))))
                (t
                 (setf body (push-assert!-into (car outs))))))
            (let ((defun-form `(defun ,(intern (string out-symb) :om) (,.symbols)
                                 ,pname
                                 (let* ,(reverse *let-list*) ,body))))
              ;(format *om-stream* "~%[compile-patch ConstraintPatch '~A']~%~S~%"
              ;        pname defun-form)
              (eval defun-form))
            (setf *let-list* oldletlist)
            (setf *lambda-context* oldlambdacontext))))
    (setf (compiled? self) t)))

(defmethod compile-patch ((self ConstraintLispPatch))
  (unless (compiled? self)
    (handler-bind
        ((error #'(lambda (c)
                    (when *msg-error-label-on*
                      (om-message-dialog
                       (string+ "Error while compiling constraint lisp patch '"
                                (string (name self)) "' : "
                                (om-report-condition c))
                       :size (om-make-point 300 200))
                      (om-abort)))))
      (compile-constraint-lisp-patch-fun self)
      (setf (compiled? self) t))))

(defun compile-constraint-lisp-patch-fun (patch)
  (let* ((expr   (get-lisp-exp (lisp-exp patch)))
         (symb   (intern (string (code patch)) :om))
         (pname  (or (name patch) "constraint-lisp-patch")))
    (cond
      ((and (consp expr) (eq (first expr) 'lambda))
       (let* ((args      (second expr))
              (body-all  (cddr expr))
              (body      (if (and (stringp (first body-all)) (rest body-all))
                             (rest body-all)
                             body-all))
              (leading   (butlast body))
              (last-form (car (last body))))
         (let ((defun-form
                 (if (null last-form)
                     `(defun ,symb ,args ,pname nil)
                     `(defun ,symb ,args
                        ,pname
                        ,@leading
                        ,(push-assert!-into last-form)))))
           ;(format *om-stream* "~%[compile-patch ConstraintLispPatch '~A']~%~S~%"
           ;        pname defun-form)
           (eval defun-form))))
      (t
       (let ((defun-form `(defun ,symb () ,pname nil)))
         ;(format *om-stream* "~%[compile-patch ConstraintLispPatch '~A' -- empty]~%~S~%"
         ;        pname defun-form)
         (eval defun-form))))))

;;;; =========================================================================
;;;; BOX CLASS + SPECIAL-LAMBDA-VALUE -- inject trailing T so compile-screamer-
;;;; constraint-2's push-assert!-into sees a no-op tail. The real assertion
;;;; happens as a side-effect of (APPLY 'SYM ...) executing the patch's
;;;; compiled defun, whose body already contains the specialized ASSERT!.
;;;; =========================================================================

(defclass OMBoxConstraintPatch (OMBoxAbsPatch) ()
  (:metaclass omstandardclass))

(defmethod box-class ((self ConstraintPatchAbs))     'OMBoxConstraintPatch)
(defmethod box-class ((self ConstraintLispPatchAbs)) 'OMBoxConstraintPatch)

(defmethod special-lambda-value ((self OMBoxConstraintPatch) symbol)
  (multiple-value-bind (nesymbs args) (get-args-eval-currry self)
    (eval `#'(lambda ,(reverse nesymbs)
               (apply ',symbol (list ,.args))
               t))))

;;;; =========================================================================
;;;; OM-SAVE / LOADERS -- preserve subclass across .omp / .oml roundtrip
;;;; =========================================================================

(defmethod om-save ((self ConstraintPatch) &optional (values? nil))
  (if (lisp-exp-p self)
      `(setf *om-current-persistent*
             (om-load-clisp-patch ,(name self) ,*om-version*
                                  ,(str-without-nl (lisp-exp-p self))))
      (let ((boxes        (mapcar #'(lambda (b) (omNG-save b values?)) (boxes self)))
            (connectiones (mk-connection-list (boxes self)))
            (pictlist     (omng-save (pictu-list self))))
        `(setf *om-current-persistent*
               (om-load-cpatch1 ,(name self) ',boxes ',connectiones ,pictlist ,*om-version*)))))

(defmethod om-save ((self ConstraintPatchAbs) &optional (values? nil))
  (if (lisp-exp-p self)
      `(om-load-clisp-abspatch ,(name self) ,*om-version* ,(str-without-nl (lisp-exp-p self)))
      (let ((boxes        (mapcar #'(lambda (b) (omNG-save b values?)) (boxes self)))
            (connectiones (mk-connection-list (boxes self)))
            (doc          (str-without-nl (doc self)))
            (pictlist     (omng-save (pictu-list self))))
        (when (editorframe self)
          (set-win-size self (om-interior-size (window (editorframe self))))
          (set-win-position self (om-view-position (window (editorframe self)))))
        `(om-load-cpatch-abs1 ,(name self) ',boxes ',connectiones ,*om-version* ,pictlist ,doc
                              ,(om-save-point (w-pos self)) ,(om-save-point (w-size self))))))

(defmethod om-save ((self ConstraintLispPatch) &optional (values? nil))
  (declare (ignore values?))
  `(setf *om-current-persistent*
         (om-load-clisp-patch ,(name self) ,*om-version*
                              ,(str-without-nl (lisp-exp self)))))

(defmethod om-save ((self ConstraintLispPatchAbs) &optional (values? nil))
  (declare (ignore values?))
  `(om-load-clisp-abspatch ,(name self) ,*om-version* ,(str-without-nl (lisp-exp self))))

(defun om-load-cpatch1 (name boxes connections &optional (fond-ec nil) (version nil) (pictueditors nil))
  (declare (ignore pictueditors))
  (let ((newpatch (omNG-make-new-cpatch name)))
    (setf (boxes newpatch) nil)
    (mapc #'(lambda (b) (omNG-add-element newpatch (eval b))) boxes)
    (setf (boxes newpatch) (reverse (boxes newpatch)))
    (setf (connec newpatch) (loop for i in connections collect (load-connection i)))
    (setf (pictu-list newpatch) fond-ec)
    (when version (setf (omversion newpatch) version))
    newpatch))

(defun om-load-cpatch-abs1 (name boxes connections
                            &optional (version nil) (pictlist nil) (doc "") wpos wsize
                            &rest args)
  (declare (ignore args))
  (let ((newpatch (make-instance 'ConstraintPatchAbs :name name :icon 180210)))
    (setf (boxes newpatch) nil)
    (mapc #'(lambda (b) (omNG-add-element newpatch (eval b))) boxes)
    (setf (boxes newpatch) (reverse (boxes newpatch)))
    (setf (saved? newpatch) connections)
    (remk-connections (boxes newpatch) (loop for i in connections collect (load-connection i)))
    (setf (pictu-list newpatch) pictlist)
    (setf (doc newpatch) (str-with-nl doc))
    (when wpos  (setf (w-pos newpatch) wpos))
    (when wsize (setf (w-size newpatch) wsize))
    (compile-patch newpatch)
    (when version (setf (omversion newpatch) version))
    newpatch))

(defun om-load-clisp-patch (name version expression)
  (let ((newpatch (omNG-make-new-clisp-patch name)))
    (setf (omversion newpatch) version)
    (setf (lisp-exp newpatch) (get-lisp-str expression))
    newpatch))

(defun om-load-clisp-abspatch (name version expression)
  (let ((newpatch (make-instance 'ConstraintLispPatchAbs :name name :icon 180123)))
    (setf (omversion newpatch) version)
    (setf (lisp-exp newpatch) (get-lisp-str expression))
    (compile-constraint-lisp-patch-fun newpatch)
    newpatch))

;;;; =========================================================================
;;;; CONSTRUCTORS
;;;; =========================================================================

(defun omNG-make-new-cpatch (name &optional (posi (om-make-point 0 0)))
  (let ((newpatch (make-instance 'ConstraintPatch :name name :icon 180183)))
    (set-icon-pos newpatch posi)
    newpatch))

(defun omNG-make-new-clisp-patch (name &optional (posi (om-make-point 0 0)))
  (let ((newpatch (make-instance 'ConstraintLispPatch :name name :icon 180124)))
    (set-icon-pos newpatch posi)
    newpatch))

;;;; =========================================================================
;;;; PATCH2ABS / ABS2PATCH -- preserve subclass across conversion
;;;; =========================================================================

(defmethod patch2abs ((self ConstraintPatch))
  (let ((newabs (make-instance 'ConstraintPatchAbs :name (name self) :icon 180210)))
    (if (lisp-exp-p self)
        (setf (lisp-exp-p newabs) (lisp-exp-p self))
        (let ((boxes (boxes self)))
          (loop for item in (reverse (mapcar #'omNG-copy boxes)) do
                (omng-add-element newabs (eval item)))
          (copy-connections boxes (boxes newabs))
          (setf (pictu-list newabs) (mapcar 'copy-picture (pictu-list self)))))
    (set-icon-pos newabs (get-icon-pos self))
    (set-win-size newabs (get-win-size self))
    (setf (doc newabs) (doc self))
    newabs))

(defmethod abs2patch ((self ConstraintPatchAbs) name pos)
  (let ((newabs (omNG-make-new-cpatch name pos)))
    (if (lisp-exp-p self)
        (setf (lisp-exp-p newabs) (lisp-exp-p self))
        (let ((boxes (boxes self)))
          (loop for item in (reverse (mapcar #'omNG-copy boxes)) do
                (omng-add-element newabs (eval item)))
          (copy-connections boxes (boxes newabs))
          (setf (pictu-list newabs) (mapcar 'copy-picture (pictu-list self)))))
    (set-icon-pos newabs (get-icon-pos self))
    (set-win-size newabs (get-win-size self))
    (setf (doc newabs) (doc self))
    newabs))

(defmethod patch2abs ((self ConstraintLispPatch))
  (let ((newabs (make-instance 'ConstraintLispPatchAbs :name (name self) :icon 180123)))
    (setf (lisp-exp newabs) (lisp-exp self))
    (set-icon-pos newabs (get-icon-pos self))
    (setf (doc newabs) (doc self))
    newabs))

(defmethod abs2patch ((self ConstraintLispPatchAbs) name pos)
  (let ((newabs (omNG-make-new-clisp-patch name pos)))
    (setf (lisp-exp newabs) (lisp-exp self))
    (set-icon-pos newabs (get-icon-pos self))
    (setf (doc newabs) (doc self))
    newabs))

;;;; =========================================================================
;;;; ADVICE ON COMPILE-LISP-PATCH-FUN -- dispatch to constraint compiler
;;;; =========================================================================
;;; OM calls COMPILE-LISP-PATCH-FUN directly from COMPILE-WITHOUT-CLOSE
;;; (editor), LOAD-ABSTRACTION-ATTRIBUTES, GET-PATCH-INPUTS, OMNG-COPY --
;;; bypassing COMPILE-PATCH entirely. This advice intercepts those paths for
;;; ConstraintLispPatch instances so the assert!-wrapped defun is emitted.

(lw:defadvice (compile-lisp-patch-fun intercept-constraint-lisp-patch :around)
              (patch)
  (cond
    ((typep patch 'ConstraintLispPatch)
     ;(format *om-stream* "~%[compile-lisp-patch-fun advice -> ConstraintLispPatch '~A']~%"
     ;        (or (name patch) "unnamed"))
     (compile-constraint-lisp-patch-fun patch))
    (t (lw:call-next-advice patch))))

;;;; =========================================================================
;;;; ADVICE -- register "cpatch" / "clisp" as creation keywords
;;;; =========================================================================

(lw:defadvice (update-pack-symbols add-cpatch-clisp-symbols :after) ()
  (setf *all-om-pack-symbols*
        (sort (append *all-om-pack-symbols* (list "cpatch" "clisp"))
              'string<)))

(update-pack-symbols)

;;;; =========================================================================
;;;; ADVICE -- redirect kernel icon lookup to library's icon folder
;;;; =========================================================================
;;; Icons 170 / 180183 / 180210 / 180123 / 180124 are NOT class-box icons
;;; (which OM resolves via def-icon-var + lib-icons-folder), but patch-class
;;; icons resolved via om-load-icon against *om-icon-folder* (OM kernel).
;;; Instead of copying PNGs into the OM install folder (requires admin
;;; on Win/Mac/Linux), we intercept om-load-icon for these specific IDs and
;;; redirect to OM-Screamer's own resources/icon folder.

(lw:defadvice (om-load-icon intercept-constraint-patch-icons :around)
              (id &optional folder)
  (if (and (not folder)
           (integerp id)
           (member id '(170 180183 180210 180123 180124) :test #'=)
           *om-screamer-icon-folder*)
      (or (lw:call-next-advice id *om-screamer-icon-folder*)
          (lw:call-next-advice id folder))
      (lw:call-next-advice id folder)))

(lw:defadvice (add-box-in-patch-panel intercept-cpatch-clisp :around)
              (str scroller pos)
  (let* ((*package* (find-package :om))
         (funname   (ignore-errors (read-from-string str))))
    (cond
      ((equal funname 'cpatch)
       (let ((newbox (omNG-make-new-boxcall
                      (make-instance 'ConstraintPatchAbs
                                     :name (mk-unique-name scroller "mycpatch")
                                     :icon 180210)
                      pos
                      (mk-unique-name scroller "mycpatch"))))
         (when (and newbox (box-allowed-p newbox scroller))
           (omG-add-element scroller (make-frame-from-callobj newbox)))
         newbox))
      ((equal funname 'clisp)
       (let ((newbox (omNG-make-new-boxcall
                      (make-instance 'ConstraintLispPatchAbs
                                     :name (mk-unique-name scroller "myclisp")
                                     :icon 180123)
                      pos
                      (mk-unique-name scroller "myclisp"))))
         (when (and newbox (box-allowed-p newbox scroller))
           (omG-add-element scroller (make-frame-from-callobj newbox)))
         newbox))
      (t (lw:call-next-advice str scroller pos)))))
