;===============================================
;===============================================

;;; PWConstraints by Mikael Laurson (c), 1995

;===============================================
;===============================================
(in-package omcs)
;===============================================

;===============================================
;          arc
;===============================================
(defclass arc ()
  ((fn :initarg :fn :accessor fn)
   (prev-s-variables :initarg :prev-s-variables :accessor prev-s-variables)
   (next-s-variable :initarg :next-s-variable :accessor next-s-variable)))

(defun mk-arc (fn prev-s-variables next-s-variable)
  (make-instance 'arc :fn fn :prev-s-variables prev-s-variables 
                 :next-s-variable next-s-variable))


;===============================================
;          search-variable
;===============================================
(defclass search-variable (key-item linked-list-item)
  ((domain :initform () :initarg :domain :accessor domain) 
   (domain-copy :initform () :initarg :domain-copy :accessor domain-copy) 
   (arcs :initform () :initarg :arcs :accessor arcs) ;;
   (value :initform () :initarg :value :accessor value)
   (other-values :initform () :initarg :other-values :accessor other-values)))

(defmethod update-values ((self search-variable) values)
  (setf (value self) (first values))
  (setf (other-values self) (rest values)))

(defmethod set-init-state ((self search-variable))
  (setf (value self) ())
  (setf (other-values self) ())) 

(defmethod set-new-forward-state- ((self search-variable))
  (if (other-values self)
    (progn 
      (setf (value self) (first (other-values self)))
      (setf (other-values self) (rest (other-values self)))
      t)
    (progn 
      (set-init-state self)
      nil)))

(defmethod set-new-forward-state ((self search-variable))
  (let ((fl (set-new-forward-state- self))
        (val (value self)))
    (setf (first (read-key self :write-pos)) val)
    (setf (first (read-key self :rev-sols-list)) val)
    fl))

(defmethod all-prev-sols ((self search-variable))
  (let ((prev-item self)
        res)
    (loop while (setq prev-item (prev-item prev-item))
          do (push (value prev-item) res))
    (nreverse res)))

(defmethod all-next-sols ((self search-variable))
  (let ((next-item self)
        res)
    (loop while (setq next-item (next-item next-item))
          do (push (value next-item) res))
    (nreverse res)))

;===============================================
(defun make-search-variable (domain)
  (make-instance 'search-variable :domain domain :domain-copy domain))



