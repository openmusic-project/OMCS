;===============================================
;===============================================

;;; PWConstraints by Mikael Laurson (c), 1995

;===============================================
;===============================================
(in-package omcs)
;===============================================

;===============================================
;          search-engine
;===============================================
(defclass search-engine (key-item)
  ((search-variables :initform () :initarg :search-variables :accessor search-variables) 
   (search-variables-list :initform () :initarg :search-variables-list :accessor search-variables-list)
   (variable-pos :initform 0 :initarg :variable-pos :accessor variable-pos) 
   (rules :initform nil :initarg :rules :accessor rules)
   (heuristic-rules :initform nil :initarg :heuristic-rules :accessor heuristic-rules)
   (all-sols :initform nil :accessor all-sols)
   (print-fl :initform nil :initarg :print-fl :accessor print-fl)
   (sols-mode :initform :once :initarg :sols-mode :accessor sols-mode)))


;========================
; misc
;========================
(defmethod get-current-variable ((self search-engine))
  (svref (search-variables self) (variable-pos self)))

(defmethod succeed-case? ((self search-engine))
  (= (variable-pos self)(length (search-variables self))))

(defmethod fail-case? ((self search-engine))
  (= (variable-pos self) -1))

(defmethod step-forwards ((self search-engine))
  (incf (variable-pos self)))

(defmethod step-forwards :after ((self search-engine))
  (when (print-fl self)
    (progn (princ (variable-pos self)) (princ " "))))  

(defmethod step-backwards ((self search-engine))
  (decf (variable-pos self)))

(defmethod set-start-position ((self search-engine))
  (setf (variable-pos self) 0))

;========================
; forward-checking
;========================
(defmethod update-domains ((self search-engine) search-variable)
  (dolist (arc (arcs search-variable))
    (setf (domain (next-s-variable arc)) (domain-copy (next-s-variable arc)))))

(defmethod prune-last-arc-domain ((self search-engine) variable candidate)
  (let (next-domain-temp)
    (dolist (arc (arcs variable))
        (let* ((fn (fn arc))
               (tail (list ()))
               (sols (nconc (mapcar #'(lambda (i) (value i)) (prev-s-variables arc)) 
                            (list candidate) tail))
               (next-variable-domain (domain (next-s-variable arc))) next)
          (setq next-domain-temp nil) 
          (loop while next-variable-domain
                do (progn (setq next (pop next-variable-domain))
                          (setf (first tail) next) 
                          (when (apply #'funcall fn sols)  
                            (push next next-domain-temp))))
          (setf (domain (next-s-variable arc)) (nreverse next-domain-temp))))))

(defmethod forward-check ((self search-engine) variable candidates)
  (let ((arcs (arcs variable))  fl arcs-c arc res)
    (if arcs
      (progn
        ;(print (mapcar #'(lambda (arc) (prev-s-variables arc)) arcs))
        (dolist (candidate candidates)
          (setq arcs-c arcs)
          (setq fl t)
          (loop while (and fl arcs-c)
                do (progn (setq fl nil) ;; each arc has to be true
                          (setq arc (pop arcs-c))
                          (let* ((fn (fn arc))
                                 (tail (list ()))
                                 (sols (nconc (mapcar #'(lambda (i) (value i)) (prev-s-variables arc)) 
                                              (list candidate) tail))
                                 (next-variable-domain  (domain (next-s-variable arc))) next)
                            (loop while (and (null fl) next-variable-domain)
                                  do (progn (setq next (pop next-variable-domain))
                                         (setf (first tail) next) 
                                         (when (apply #'funcall fn sols) ; at least one value is supported
                                           (setq fl t)))))))
          (when fl (push candidate res)))
        (nreverse res))
      candidates)))

;========================
;   heuristic rules
;========================
; if a rule gives a numeric value (i.e. it is applied) then candidates are
; sorted so that the candidate(s) that has (have) the highest score is  (are) found first!
(defmethod apply-heuristic-rules ((self search-engine) variable candidates)
 (if (heuristic-rules self)
  (let* ((prev-sols-rev (read-key variable :rev-sols-list))
         (prev-sols (read-key variable :sols-list))
         (ith (read-key variable :write-pos)) 
         (end-sols-list (read-key variable :end-sols-list)) 
         (index (read-key variable :note-index))
         (fns (heuristic-rules self))
         (len (cur-index)) 
         score score+cand-pairs heuristic-rules-applied? res)
    (setf (nthcdr (1+ index)  prev-sols) nil)
    ;(print (list 'prev (butlast prev-sols) candidates))
    (dolist (candidate candidates)
      (setf (first ith) candidate);; ML 22.01.04 bug pointed by Kilian: (car candidates) -> candidate
      (setf (first prev-sols-rev) candidate)
      (setq score 0)
      (dolist (fn fns)
        (setq res (funcall fn prev-sols prev-sols-rev len))
        (when (numberp res) 
          (setq heuristic-rules-applied? t)
          (incf score res)))  
      (push (list score candidate) score+cand-pairs))
    (setq candidates (if heuristic-rules-applied?
                       (mapcar #'second (sort score+cand-pairs #'> :key #'first))
                       candidates))
    ;(setf (first ith) (car candidates))
    ;(setf (first prev-sols-rev) (car candidates))
    (setf (nthcdr (1+ index)  prev-sols) end-sols-list)
    candidates)
  candidates))

;========================
; apply-rules
;========================
(defmethod apply-rules-loop ((self search-engine) variable)
  (let* ((prev-sols-rev (read-key variable :rev-sols-list))
         (prev-sols (read-key variable :sols-list))
         (ith (read-key variable :write-pos)) 
         (end-sols-list (read-key variable :end-sols-list)) 
         (index (read-key variable :note-index))
         (candidates (domain variable))
         (fns (rules self))
         (len (cur-index)) 
         temp)
    (setf (nthcdr (1+ index)  prev-sols) nil)
    (dolist (fn fns)
      (setq temp nil)
      (loop while candidates
            do (progn (setf (first ith) (car candidates))
                      (setf (first prev-sols-rev) (car candidates))
                      (when (funcall fn prev-sols prev-sols-rev len) ;; 3 args len !!  
                        (push (car candidates) temp))
                      (pop candidates)))
      (unless temp
        (write-diagnostics fn)
        (return nil))
      (setq candidates (nreverse temp)))
    (when candidates 
        (setf (first ith) (car candidates))
        (setf (first prev-sols-rev) (car candidates)))
    (setf (nthcdr (1+ index)  prev-sols) end-sols-list)
    candidates))

(defmethod write-value-to-partial-sols-list ((self search-engine) s-variable candidate)
  (setf (first (read-key s-variable :write-pos)) candidate)
  (setf (first (read-key s-variable :rev-sols-list)) candidate))

(defmethod apply-rules ((self search-engine) search-variable)
  (let (candidates)
    (when (setq candidates (apply-rules-loop self search-variable))
      (setq candidates 
             (apply-heuristic-rules self search-variable
                                    (forward-check self search-variable candidates)))
      (when candidates (prune-last-arc-domain self search-variable (first candidates)))             
      (if (and candidates (listp candidates))
        (progn 
          (update-values search-variable candidates)
          (write-value-to-partial-sols-list self search-variable (first candidates))
          t)
        nil))))

;========================
; forward,backtrack,succeed 
;========================
(defmethod forward ((self search-engine))
  (if (succeed-case? self)
    (succeed self)
    (let* ((search-variable (get-current-variable self))
           (value (value search-variable)))
      (if value ;; only from backtrack 
        (progn (prune-last-arc-domain self search-variable value) 
               (step-forwards self) (forward self))
        (if (apply-rules self search-variable)
          (progn (step-forwards self) (forward self))
          (progn (set-init-state search-variable) (step-backwards self) (backtrack self)))))))
 
(defmethod backtrack ((self search-engine))
  (if (fail-case? self)
    (fail self)
    (let ((search-variable (get-current-variable self)))
      (update-domains self search-variable) 
      (if (set-new-forward-state search-variable)
        (forward self)
        (progn (set-init-state search-variable) (step-backwards self) (backtrack self))))))

(defmethod succeed ((self search-engine))
  (let ((res (mapcar #'value (search-variables-list self))))
    (push res (all-sols self))
    (if (or (eq (sols-mode self) :once) 
            (and (numberp (sols-mode self)) 
                 (= (length (all-sols self)) (sols-mode self)))) 
      res
      (progn
        (step-backwards self)
        (update-domains self (get-current-variable self))
        (backtrack self)))))

(defmethod fail ((self search-engine)) :fail)
      
(defmethod start ((self search-engine))
  (setf (all-sols self) nil)
  (mapc  #'set-init-state (search-variables-list self))
  (when (print-fl self);  (and (print-fl self) *constraint-diagnostics*)
    (format t "Number of search-variables = ~A" (length (search-variables  self))) 
    (terpri))
  (set-start-position self)
  (forward self))

;=======================================================
;     optimize - avoid consing with l and rl
;=======================================================
(defun optimize-search-lists (s-variables)
  (let*  ((rev-items (reverse s-variables))
          (orig-list (make-list (length s-variables))) 
          (rev-list (make-list (length s-variables))) 
          (rev-items-c rev-items)) 
    ;(for (i 1 1 (length s-variables))
    (loop for i from 1 to (length s-variables)
          do (progn
               (write-key (nth (1- i) s-variables) :sols-list orig-list) 
               (write-key (nth (1- i) s-variables) :write-pos (nthcdr (1- i) orig-list)) ;; (setf (first write-pos ..)
               (write-key (nth (1- i) s-variables) :end-sols-list (nthcdr i orig-list))))
    ;(for (i 0 1 (1- (length rev-items)))
    (loop for i from 0 to (1- (length rev-items))
      do (write-key (pop rev-items-c) :rev-sols-list (nthcdr i rev-list)))))

;=======================================================
;    make-search-engine
;=======================================================
(defun make-search-engine (search-space rules 
                            &key (fwc-rules ()) (heuristic-rules ()) (sols-mode :once) 
                            (class 'search-engine) (print-fl ()))
  (let ((engine
         (make-instance class
           :search-variables (apply #'vector (mapcar #'make-search-variable search-space))
           :rules rules  
           :heuristic-rules heuristic-rules  
           :sols-mode sols-mode
           :print-fl print-fl)))
    (setf *current-SE* engine)
    (setf (search-variables-list engine) (coerce (search-variables engine) 'list))
    ;(link-list (search-variables-list engine))  
    (when fwc-rules (convert-rules-to-arcs (search-variables-list engine) fwc-rules))
    (dolist (variable (search-variables-list engine))
      (write-key variable :note-index (position variable (search-variables-list engine))))  
    (optimize-search-lists (search-variables-list engine)) ;; !!
    engine))

;=======================================================================
; User search-engine functions
;=======================================================================
(defun PMC (search-space rules 
                    &key (fwc-rules ()) (heuristic-rules ()) (sols-mode :once) (rnd? nil)
                    (print-fl nil) (class 'search-engine))
  (unless (listp (car rules)) (setq rules (list rules))) 
  (let* ((engine (make-search-engine       
                  (if rnd?  (mapcar #'om::permut-random search-space) search-space)
                  (mk-PMC-fns rules) 
                  :fwc-rules fwc-rules
                  :heuristic-rules (mk-PMC-fns heuristic-rules) 
                  :sols-mode sols-mode :class class :print-fl print-fl)))
    (start-diagnostics (rules engine))
    (start engine)
    (when *constraint-diagnostics* ;;(and *constraint-diagnostics*print-fl)  
         (terpri) (format t "Rule diagnostics:") (read-diagnostics (rules engine)))
    (all-sols engine)))

