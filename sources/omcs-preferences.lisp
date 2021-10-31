;;; PWConstraints by Mikael Laurson (c), 1995
;;;
;;; Preferences
;;; K . Haddad , IRCAM 2021
;===============================================
(in-package :om)



;by default diagnostics is nil
;(setf *constraint-diagnostics* t)
;(setf *constraint-diagnostics* nil)


;===================================================================================
;         PREFERENCES PANEL MODULE
;===================================================================================



(defvar omcs::*constraint-diagnostics* nil)
(setf  omcs::*constraint-diagnostics* nil)

;(defvar *engine-inf* nil)
;(setf *engine-inf* "engine")


(defmethod get-def-vals ((iconID (eql :omcs-lib)))
   (list 
    :constr-diag nil
  ;  :engine-inf "engine" 
    ))


(defmethod put-preferences ((iconID (eql :omcs-lib)))

  (let* ((modulepref (find-pref-module iconID)))
    (setf omcs::*constraint-diagnostics* (get-pref modulepref :constr-diag))
   ; (setf *eng-inf* (get-pref modulepref :engine-inf))
    ))

(defmethod save-pref-module ((iconID (eql :omcs-lib)) item)
   (list iconID `(list 
                  :constr-diag ,omcs::*constraint-diagnostics*
                 ; :engine-inf ,*engine-inf* 
                  ) *om-version*))



(defmethod make-new-pref-scroll  ((num (eql :omcs-lib)) modulepref)
  (let ((thescroll (om-make-view 'preference-pane
                                 :pref-id num
                                 :name "OMCS"
                                 :size (get-pref-scroll-size)
                                 :position (om-make-point 0 0)
                                 :font *controls-font* 
                                ;:scrollbars :v 
                                ;:retain-scrollbars t
                                 :bg-color *om-light-gray-color*
                                 ))
        (l1 50)
	(l2 (round (om-point-h (get-pref-scroll-size)) 2))
	(l3 (- (om-point-h (get-pref-scroll-size)) 60))
        (i 40)
        (posy 0)
	(dy 40)
        outtxt tmptxt)
    
    (om-add-subviews thescroll 
                     

                     (om-make-dialog-item 'om-static-text (om-make-point l1 (incf posy dy)) (om-make-point 90 24) "Rule Diagnostics"
                                          :font *controls-font*) 

                     (om-make-dialog-item 'om-check-box (om-make-point (+ l1 100) posy)
                                          (om-make-point 200 15) ""
                                          :font *controls-font*
                                          :checked-p (get-pref modulepref :constr-diag)
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :constr-diag (om-checked-p item))))
                     #|
                     (om-make-dialog-item 'om-static-text (om-make-point l1 (incf posy dy)) (om-make-point 90 24) "Engine Info"
                                          :font *controls-font*) 

                     (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point (+ l1 100) posy)
                                          (om-make-point 200 15) ""
                                          :range '("engine" "allsols" "domains" "other-values" "partialsol")
                                          :value (cond 
                                                  ((string= *engine-inf* "engine") "engine")
                                                  ((string= *engine-inf* "allsols") "allsols")
                                                  ((string= *engine-inf* "domains") "domains")
                                                  ((string= *engine-inf* "other-values") "other-values")
                                                  ((string= *engine-inf* "partialsol") "partialsol"))
                                          
					  :di-action (om-dialog-item-act item 
                                                       (let ((choice (om-get-selected-item item)))
                                                         ;(set-pref modulepref :engin-inf
                                                                   (if
                                                                       (string= choice "engine")
                                                                       (print (omcs::engine-info 'omcs::engine))
                                                                     (omcs::engine-info 'omcs::partialsol)
                                                                   )
                                                                   ;)
                                                         ))
					  :font *controls-font*
                                          )
                     |#
                     )

 ;   (setf posy 0)
    
 ; (om-add-subviews thescroll
                    ; (om-make-dialog-item 'om-static-text (om-make-point l2 (incf posy 50)) (om-make-point 200 30) "Template Files"
                    ;                      :font *om-default-font2b*)
    ;              )
          
    thescroll))




;set and load tab in om preferences panel 
(pushr :omcs-lib *pref-order*)

(defun add-omcs-preferences ()
(push-pref-module (list :omcs-lib (get-def-vals :omcs-lib))))

(add-omcs-preferences)



