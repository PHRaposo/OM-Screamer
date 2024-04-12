(in-package :om)

; -----------------------------------------		

; ANALYSIS

(defclass! INTERVAL-ANALYSIS (ABSTRACT-ANALYSIS) ())

(defmethod default-segment-class ((self interval-analysis)) 'chord-segment)

(defmethod analysis-add-click ((self interval-analysis) panel pos) 
  (om-beep-msg "Use SELECTION + 'S' to create new segments in this analysis"))

(defmethod compute-segments-p ((self interval-analysis)) t)
(defmethod analyse-segments-p ((self interval-analysis)) t)
(defmethod compute+analyse-segments-p ((self interval-analysis)) t)

(defmethod compute-analysis-segments ((self interval-analysis) (object t)) 
  (if (or (not (analysis-segments self))
          (om-y-or-n-dialog "This operation will delete the current segmentation. Continue?"))
      (loop for c in (get-real-chords object)
            for i = 0 then (+ i 1) collect
            (make-instance 'chord-segment
                       :chord-ids (list i)
                       :color (om-random-color)
                       ))
    ))

(defclass! int-a (ryth-coerce)
   ((puntos :initform '(3 4) :accessor puntos :initarg :puntos))
   (:icon 421))
 
(defmethod! chord2ints ((Self chord) approx)
  :initvals '(nil 2) :indoc '("the chord"  "approx")
  :doc "Generates all intervals mod12 from chords" 
:icon 421
    (make-instance 'int-a
      :puntos (mapcar #'(lambda (x) (mod x 12)) (om/ (x->dx (sort-list (approx-m (Lmidic self) approx))) 100))))


(defmethod analyse-one-segment ((self interval-analysis) (seg segment) (object t))
  (setf (segment-data seg) 
        (chord2ints (make-instance 'chord 
                                :lmidic (apply 'append (mapcar 'lmidic (chords seg))))
                 2)))

(defmethod segment-data-tostring ((self interval-analysis) segment)
  (if (segment-data segment)
      (format nil "~A" (car (puntos (segment-data segment))))
    ""))

(defmethod draw-segment-data ((self interval-analysis) segment view)
  (let* ((x1 (time-to-pixels view (segment-begin segment)))
         (x2 (time-to-pixels view (segment-end segment)))
         (mid (round (+ x1 x2) 2))
         ;(cr 40)
		 ) 
  (om-with-font *om-default-font1*
       (om-draw-string (- mid (round (om-string-size (segment-data-tostring self segment) *om-default-font1*) 2))
                       (- (h view) 60)
                       (segment-data-tostring self segment))
       )))

(defclass! SETCLASS-ANALYSIS (ABSTRACT-ANALYSIS) ())

(defmethod default-segment-class ((self setclass-analysis)) 'chord-segment)

(defmethod analysis-add-click ((self setclass-analysis) panel pos) 
  (om-beep-msg "Use SELECTION + 'S' to create new segments in this analysis"))

(defmethod compute-segments-p ((self setclass-analysis)) t)
(defmethod analyse-segments-p ((self setclass-analysis)) t)
(defmethod compute+analyse-segments-p ((self setclass-analysis)) t)

(defmethod compute-analysis-segments ((self setclass-analysis) (object t)) 
  (if (or (not (analysis-segments self))
          (om-y-or-n-dialog "This operation will delete the current segmentation. Continue?"))
      (loop for c in (get-real-chords object)
            for i = 0 then (+ i 1) collect
            (make-instance 'chord-segment
                       :chord-ids (list i)
                       :color (om-random-color)
                       ))
    ))

(defclass! setclass-a (ryth-coerce)
   ((puntos :initform '(3-11A 3-11B) :accessor puntos :initarg :puntos))
   (:icon 421))
 
(defmethod! chord2setclass ((Self chord) approx)
  :initvals '(nil 2) :indoc '("the chord"  "approx")
  :doc "Generates all intervals mod12 from chords" 
:icon 421
    (make-instance 'setclass-a
      :puntos  (list (om?::fn (mc->pcv (approx-m (Lmidic self) approx)) ))))


(defmethod analyse-one-segment ((self setclass-analysis) (seg segment) (object t))
  (setf (segment-data seg) 
        (chord2setclass (make-instance 'chord 
                                :lmidic (apply 'append (mapcar 'lmidic (chords seg))))
                 2)))

(defmethod segment-data-tostring ((self setclass-analysis) segment)
  (if (segment-data segment)
      (format nil "~A" (car (puntos (segment-data segment))))
    ""))

(defmethod draw-segment-data ((self setclass-analysis) segment view)
  (let* ((x1 (time-to-pixels view (segment-begin segment)))
         (x2 (time-to-pixels view (segment-end segment)))
         (mid (round (+ x1 x2) 2))
         ;(cr 40)
		 ) 
  (om-with-font *om-default-font1*
       (om-draw-string (- mid (round (om-string-size (segment-data-tostring self segment) *om-default-font1*) 2))
                       (- (h view) 60)
                       (segment-data-tostring self segment))
       )))