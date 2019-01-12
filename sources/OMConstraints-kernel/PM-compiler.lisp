;===============================================
;===============================================

;;; PWConstraints by Mikael Laurson (c), 1995
;;; Bugg fix in OSX (OM 4.7): 
;;; compile-pattern-matching-rule: i changed to I (marked red)

;===============================================
;===============================================
(in-package omcs)
;(in-package :cl-user)
;===============================================

;===============================================
;; PM syntax: 
;===============================================
;   ?1         = variable
;   ?          = anonymous-variable
;   *          = wild card
;   i1         = index-variable (counting from 1) 
;===============================================
;; Lisp-code part: 
;===============================================

;   (?if ...)  = Lisp expression (test)

;===============================================
;   Reserved variables
;===============================================
;   l           = partial solution
;   rl          = reversed partial solution
;   len         = length of the partial solution
;===============================================
;===============================================
(defun variable-p (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun anon-variable-p (x) (eq x '?))

(defun index-variable-p (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\I)))
;(index-variable-p 'i1)

(defun wild-p (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\*)))

(defun constant-p (x) (numberp x))

;===============================================
(defun omcs::make-anon-fn (fn)
  (eval `(defun ,(gensym) ,.(rest fn))))

(defun compile-pattern-matching-rule (rule)
; (assert (string= (package-name  *package*) "COMMON-LISP-USER") ()
;   "*** The current package should be COMMON-LISP-USER ! ***" ())
  (let ((doc (find-if #'stringp rule))) ;; should be last item in rule
    (setq rule (remove-if #'stringp  rule))
    (let* ((plain-pat (remove '?if rule :key #'(lambda (n) (when (listp n) (first n)))))
           (variable-count (count-if #'(lambda (n) (or (numberp n) (variable-p n) (anon-variable-p n))) rule))
           (variable-count-c variable-count)
           (expr (second (find '?if rule :key #'(lambda (n) (when (listp n) (first n))))))
           let-res constant-res index-res wild-flag index-variable-indexes)

      (loop for i from 0 to (1- (length plain-pat))
            do (cond
                ((wild-p (nth i plain-pat))
                 (setq wild-flag t)) 
                ((anon-variable-p (nth i plain-pat))
                 (decf variable-count))
                ((variable-p (nth i plain-pat))
                 (if (not wild-flag)
                   (push `(,(nth i plain-pat) (nth ,i l)) let-res)
                   (push `(,(nth i plain-pat) (nth ,(1- variable-count) rl)) let-res))
                 (decf variable-count))
                ((constant-p (nth i plain-pat))
                 (if (not wild-flag)
                   (push `(= ,(nth i plain-pat) (nth ,i l)) constant-res)
                   (push `(= ,(nth i plain-pat) (nth ,(1- variable-count) rl)) constant-res))
                 (decf variable-count))
                ((index-variable-p (nth i plain-pat)) (print plain-pat)
                 (push (1- (read-from-string (remove "i" (format nil "~A" (nth i plain-pat)) :test #'string=))) index-variable-indexes) 
                 (push `(,(nth i plain-pat) (nth ,(1- (read-from-string (remove "i" (format nil "~A" (nth i plain-pat)) :test #'string=))) l)) let-res)
                 (push `,(nth i plain-pat) index-res))
                (t (error (format nil "unknown item =  ~A" (nth i plain-pat))))))
      (setq let-res (nreverse let-res)  constant-res (nreverse constant-res) index-res (nreverse index-res))
      `(lambda (l rl len) ,(if doc doc "") 
          l rl ;; to avoid "Unused lexical variable ?1" message
          (if ,(if (not index-variable-indexes)
                 `(< len ,variable-count-c)
                 `(not (= len ,(1+ (apply #'max index-variable-indexes))))) 
            t
            (when (and ,.constant-res)
              (let (,@let-res)
                ,@(mapcar #'first let-res)  ;; to avoid "Unused lexical variable ?1" message
                (when (and ,@index-res)
                  ,(if expr expr t)))))))))

#|
;(compile-pattern-matching-rule '(* ?1 ?2 (?if (member (- ?2 ?1) '(1 -1 2 -2))) "is interval betw ?1 and ?2 member of (1 -1 2 -2)"))
;(compile-pattern-matching-rule '(* ?1 ?2 i1 i2 (?if (or (member (- i2 i1) '(5 7)) (member (- ?2 ?1) '(1 -1 2 -2))))))
;(compile-pattern-matching-rule '(?1 ?2 ?3 ?4 ? ?5 ? ?6 ? * (?if (not (intersection (list ?1 ?2 ?3 ?4 ?5 ?6) '(0 1 2))))))
;(compile-pattern-matching-rule '(?  ?  ?  ?  0 ?  2 ?  1 * ))
;(compile-pattern-matching-rule '(i1 i3 i7))
;(compile-pattern-matching-rule '(i1 i3 i7 * ?1)) ; ??
;(compile-pattern-matching-rule '(* ?1 (?if (/= ?1 1))))
;(compile-pattern-matching-rule '(* ?1 (?if (not (member ?1 (rest rl))))))
;(compile-pattern-matching-rule '(* i78 (?if (not (member i78 (rest rl))))))

(funcall 
 (make-anon-fn (compile-pattern-matching-rule '(* ?1 (?if (if (= (mod (length l) 2) 1) (member ?1 '(68 67)) t))))) 
   '(67 -55 68) (reverse '(67 -55 68)))

(funcall (make-anon-fn (compile-pattern-matching-rule '(i7 i9 i11 (?if (eq-SC? '(3-1 3-2a 3-2b 3-5a 3-5b) i7 i9 i11)))))
 '(1 2 3 4 5 6 1 8 0 10 2) (reverse '(1 2 3 4 5 6 1 8 0 10 2)) 11)

(mk-PMC-fn '(* ?1 ?2 (?if (< ?1 ?2))))
|#






#|
(mapcar #'(lambda (n) (read-from-string (first n))) (ccl::list-definitions (front-window)))

(defun mk-PMC-fn (l)
  (let ((old-package *package*) 
        fn str)
    (ccl::set-package omcs)
    (setq str (find-if  #'stringp l))
    (when str (setq l (butlast l))) 
    ;(setq l (read-from-string (format () "~A" l)))
    ;(setq l (read-from-string (format () "~S" l)))
    (setq l (read-from-string (prin1-to-string l)))
    (when str (setq l (append l (list str))))
    ;(print (list 'l l))
    (setq fn (make-anon-fn
              (compile-pattern-matching-rule
               l)))
    (ccl::set-package old-package)
    fn))
|#

(defun mk-PMC-fn (l) ;; check !!
  (make-anon-fn (compile-pattern-matching-rule l)))


(defun remove-package-expression (rules) 
  (remove '(in-package omcs) 
          (remove '(in-package :omcs) rules :test #'equal)
          :test #'equal))

(defun mk-PMC-fns (rules) 
  (mapcar #'mk-PMC-fn (remove-package-expression rules)))