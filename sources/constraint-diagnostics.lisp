;===============================================
;===============================================

;;; PWConstraints by Mikael Laurson (c), 1995

;===============================================
;===============================================
(in-package omcs)
;===============================================


;========================================================
;constraint-diagnostics
;========================================================

;by default diagnostics is nil
;(setf *constraint-diagnostics* t)
;(setf *constraint-diagnostics* nil)

(defun read-diagnostics (fns)
  (when *constraint-diagnostics* 
    (let ((index 0) 
          res)
      (dolist (fn fns)
        (when (> (read-key fn :stats) 0)
          (push (list (incf index) 
                      (documentation fn 'function) (read-key fn :stats))
                res)))
      (mapc #'print (sort res #'> :key #'third))))
  (values))

(defun write-diagnostics (fn)
  (when *constraint-diagnostics* 
    (let* ((stats (read-key fn :stats))
           (cnt (1+ stats))
           (print-cnt 50))
      ;(print (list (documentation fn 'function) cnt))
      (write-key fn :stats cnt)
      (when (= (mod cnt print-cnt) 0) 
        (print (list (documentation fn 'function) cnt))))))

(defun start-diagnostics (fns)
  (when *constraint-diagnostics*
    (dolist (fn fns)
      (write-key fn :stats 0))))


;("Engine" "Allsols" "Partialsol")

(om::defmethod! engine-info ((function symbol)) 
  :initvals '('engine) 
  :indoc '("info type")
  :menuins '((0 (("engine" 'engine) 
                 ("allsols" 'allsols)
                 ("domains" 'domains)
                 ("other-values" 'othervalues)
                 ("partialsol" 'partialsol))))
  :icon 407
  :doc "allows the user to access some of the internals of the current search-engine. This box is useful for debugging purposes"
 (let ((fn function))
    (cond 
     ((equal fn 'engine) (omcs::engine))
     ((equal fn 'allsols) (when (omcs::engine) (omcs::all-sols (omcs::engine))))
     ((equal fn 'domains)  
      (when (omcs::engine) (mapcar #'(lambda (sv) (omcs::domain sv)) (omcs::search-variables-list (omcs::engine)))))
    ((equal fn 'other-values) 
     (when (omcs::engine) (mapcar #'(lambda (sv) (omcs::other-values sv)) (omcs::search-variables-list (omcs::engine)))))
    ((equal fn 'partialsol) 
     (when (omcs::engine) (mapcar #'omcs::value (omcs::search-variables-list (omcs::engine))))))))




