;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OM-SCREAMER LIBRARY
;;; Copyright 2024 Paulo Henrique Raposo
;;;
;;;   Includes:
;;;
;;; * ADDITIONS TO OM-BACKTRACK LIBRARY (SCREAMER PROPAGATION / CONSTRAINT PACKAGE)
;;;
;;; * PC-SET-THEORY from PW-CONSTRAINTS and OMCS
;;;   by Mikael Laurson (1995) - Ported to OpenMusic by Orjan Sandred (1999)
;;;   Adapted to OM-Screamer by Paulo Henrique Raposo
;;;
;;; * SCREAMER-CONSTRAINT-SOLVER and SCREAMER-SCORE by Paulo Raposo
;;;
;;;   LISP LIBRARIES:
;;;
;;; * SCREAMER-PLUS 0.1 by Simon White
;;;  Copyright 1998-2000 University of Aberdeen
;;;
;;; * Code excerpts by Killian Sprotte
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :om)

(require-library "OM-Backtrack")

;--------------------------------------------------
;Variable definiton with files to load
;--------------------------------------------------

(defvar *screamer-files* nil)
(setf  *screamer-files* (list
			 (om::om-relative-path '("sources") "fun-button")
		         (om::om-relative-path '("sources") "om-backtrack-additions")
                         (om::om-relative-path '("sources" "screamer-plus") "screamer-plus")
                         (om::om-relative-path '("sources" "screamer-plus") "screamer-plus-additions")
                         (om::om-relative-path '("sources") "package")
                         (om::om-relative-path '("sources") "om-screamerfuns")
                         (om::om-relative-path '("sources") "screamer-additions")
                         (om::om-relative-path '("sources" "pc-set-theory") "SCs-data")
                         (om::om-relative-path '("sources" "pc-set-theory") "normal-order-data")
                         (om::om-relative-path '("sources" "pc-set-theory") "all-SCs")
                         (om::om-relative-path '("sources" "pc-set-theory") "pc-set-theory")
                         (om::om-relative-path '("sources" "pc-set-theory") "normal-order")
                         (om::om-relative-path '("sources") "screamer-solver")
                         (om::om-relative-path '("sources") "screamer-score-classes")
                         (om::om-relative-path '("sources") "screamer-score-domains")
                         (om::om-relative-path '("sources") "screamer-score-constraints")
                         (om::om-relative-path '("sources") "screamer-score")
                         (om::om-relative-path '("sources") "constraint-utils")
                          ))

;--------------------------------------------------
;Loading files
;--------------------------------------------------
(mapc #'compile&load *screamer-files*)

;--------------------------------------------------
;Fill library
;--------------------------------------------------


(fill-library '(("Propagation" nil nil (best-value solution static-ordering linear-force divide-and-conquer-force random-force reorder domain-size range-size order) nil)
	            ("Pc-set-theory"
				   (("SCs" nil nil (om?::SC-name om?::SC+off om?::SCs-card om?::SC-info om?::sub/supersets om?::SC-subsets om?::normal-order) nil)
				    ("constraints" nil nil (om?::set-classpv? om?::sub-setpv? om?::member-of-scv? om?::normal-orderv) nil)
				   ) Nil Nil Nil)

 		       ("Screamer-Solver"
 		          (("main" nil nil (screamer-solver force-function om-asert! screamer-doc) nil)
				   ("om-methods" nil nil (om+v om-v om*v om/v m->pcv mc->pcv modv mod12v om-absv sumv x->dxv x->dx-absv dx->xv all-intervalsv all-membersv not-intersectionv all-diffv list-equalv? om-make-equal) nil)
				   ("variables" nil nil (screamer-variable list-ofvs list-of-lists-ofv list-of-chords-inv) nil)
				   ("functions" nil nil (apply-contv) nil)
  				   ("constraints"
					(("general" nil nil (om?::assert!-all-differentv) nil)) nil nil nil)
				   ("utils" nil nil (om?::all-rotations) nil)
 					 ) Nil Nil Nil)

	   		       ("Screamer-Score"
	   		        (("main" nil nil (screamer-score screamer-score-domain constraint-one-voice constraint-harmony constraint-profile constraint-measure) nil)
					 ("constraints-utils" nil nil (contain-rests? variables-in pcset-equalv) nil)
	  				 ("om-utils" nil nil (quadratic-bezier cubic-bezier voice-merger bpf-lib-from-poly) nil)
	   			      ) Nil Nil Nil)

		       ("Screamer"
		           (("primitives" nil nil (s::an-integer-between s::a-member-of s::fail) nil)
				    ("variables" nil nil (s::a-member-ofv s::an-integerv s::an-integer-abovev s::an-integer-belowv s::an-integer-betweenv
										  s::a-realv s::a-real-abovev s::a-real-belowv s::a-real-betweenv s::a-numberv s::a-booleanv s::make-variable) nil)
				    ("assert!" nil nil (s::assert! s::assert!-integerpv s::assert!-notv-integerpv s::assert!-realpv s::assert!-notv-realpv
										s::assert!-numberpv s::assert!-notv-numberpv s::assert!-booleanpv s::assert!-notv-booleanpv
										s::assert!-=v2 s::assert!-<=v2 s::assert!-<v2 s::assert!-/=v2 s::assert!-memberv s::assert!-notv-memberv
										s::assert!-equalv s::assert!-notv-equalv s::assert!-=v s::assert!-<v s::assert!-<=v s::assert!->v
										s::assert!->=v s::assert!-/=v) nil)
				    ("type-restrictions" nil nil (s::numberpv s::realpv s::integerpv s::booleanpv s::memberv) nil)
		            ("boolean" nil nil (s::andv s::orv s::notv) nil)
		            ("numeric" nil nil (s::<v s::<=v s::>v s::>=v s::=v s::/=v s::+v s::-v s::*v s::/v s::minv s::maxv s::=v2 s::<=v2 s::<v2 s::/=v2 ) nil)
		            ("expression" nil nil (s::equalv) nil)
		            ("functions" nil nil (s::funcallv s::applyv) nil) ) Nil Nil Nil)

	 		       ("Screamer-Plus"
	 		           (("primitives" nil nil (screamer+::a-subset-of screamer+::a-partition-of screamer+::members-ofv screamer+::not-equalv) nil)
					    ("variables" nil nil (screamer+::a-listv screamer+::a-consv screamer+::a-symbolv screamer+::a-stringv
						                      screamer+::a-typed-varv) nil)
	 				    ("type-restrictions" nil nil (screamer+::listpv screamer+::conspv screamer+::symbolpv screamer+::stringpv screamer+::typepv) nil)
	 		            ("boolean" nil nil (screamer+::impliesv) nil)
	 		            ("expression" nil nil (screamer+::ifv screamer+::make-equal) nil)
						("lists" nil nil (screamer+::carv screamer+::cdrv screamer+::consv screamer+::firstv screamer+::secondv screamer+::thirdv
							                screamer+::fourthv screamer+::nthv screamer+::subseqv screamer+::lengthv screamer+::appendv
							                screamer+::make-listv screamer+::all-differentv) nil)
	 		            ("sets-and-bags" nil nil (screamer+::set-equalv screamer+::subsetpv screamer+::intersectionv screamer+::unionv screamer+::bag-equalv) nil)
	 		            ("arrays" nil nil (screamer+::make-arrayv screamer+::arefv) nil)
						;("objects" nil nil (screamer+::make-instancev screamer+::classpv screamer+::slot-valuev screamer+::class-ofv screamer+::class-namev
							;screamer+::slot-exists-pv screamer+::reconcile) nil)
						("high-order-fns" nil nil (screamer+::funcallinv screamer+::mapcarv screamer+::maplistv screamer+::everyv screamer+::somev
                                                                                         screamer+::noteveryv screamer+::notanyv screamer+::at-leastv screamer+::at-mostv screamer+::exactlyv 
                                                                                         screamer+::constraint-fn) nil)
						;("stream-output" nil nil (screamer+::formatv) nil)
						("functions" nil nil (s::funcallgv) nil)
						("additions" nil nil (screamer+::interval-memberv screamer+::interval-notv-memberv screamer+::abs-interval-memberv screamer+::abs-interval-notv-memberv screamer+::hard-memberv
							screamer+::mod-interval-memberv screamer+::mod-interval-notv-memberv) nil)
	 					) Nil Nil Nil)

                ;("FOLDER" Nil Nil (package::FUNCTION) Nil)

                 ))

(print (format nil "
OM-SCREAMER LIBRARY
Includes:

* PC-SET-THEORY from PW-CONSTRAINTS and OMCS
  by Mikael Laurson (1995) - Ported to OpenMusic by Orjan Sandred (1999)
  Adapted to OM-Screamer by Paulo Henrique Raposo

* SCREAMER-CONSTRAINT-SOLVER and SCREAMER-SCORE
  Copyright 2024 Paulo Henrique Raposo

  LISP LIBRARIES:

* SCREAMER-PLUS ~A by Simon White
  Copyright 1998-2000 University of Aberdeen

* Code excerpts by Killian Sprotte"
 ?::*screamer+-version*))
