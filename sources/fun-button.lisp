(in-package :om)

(defun get-icon-lock (str)
  (cond
   ((string= str "x") 167)
   ((string= str "&") 168)
   ((string= str "l") 145)
   ((string= str "o") 166)
   ((string= str "#") 170)))

(defun get-str-lock (num)
  (case num
    (167 "x") (168 "&") (145 "l") (166 "o") (170 "#")))

(defmethod allowed-lock-modes ((self omboxcall)) '("x" "&" "l" "o" "#"))

(defmethod add-rem-fun-button ((self omboxframe))
  (cond ((and (lock-button self) (string-equal (allow-lock (object self)) "#")
              (mode-allowed-p (object self) "#"))
         (remove-lock-button self))
        ((and (lock-button self) (mode-allowed-p (object self) "#"))
         (remove-lock-button self)
         (add-lock-button self "#"))
        (t (add-lock-button self "#"))))

;;; Around method that adds the #\# key to patchPanel without
;;; redefining OM's primary handle-key-event. The #\# case toggles the
;;; fun-button on the active boxes; every other key is delegated to
;;; the OM primary via call-next-method. This keeps the file in sync
;;; with future OM updates: any new key added by IRCAM is inherited
;;; automatically without manual merging.
(defmethod handle-key-event :around ((self patchPanel) char)
  (case char
    (#\# (modify-patch self)
         (mapc 'add-rem-fun-button (get-actives self)))
    (otherwise (call-next-method))))
