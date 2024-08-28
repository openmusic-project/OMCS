;===============================================
;===============================================

;;; PWConstraints by Mikael Laurson (c), 1995

;===============================================
;===============================================
(in-package :omcs)
;===============================================

(defparameter *SC-data-file* (namestring
                              (make-pathname :directory (pathname-directory *load-pathname*)
                                             :name "SCs-data.lisp")))

(defvar *all-possible-chroma-subsets-hash* (make-hash-table :test #'equal))

(defun fill-possible-chroma-subsets-hash ()
  (let (sets-list) 
    (with-open-file (in *SC-data-file* :direction :input)
      (setq sets-list (eval (read in))))
    (clrhash *all-possible-chroma-subsets-hash*)
    (dolist (set sets-list) 
      (setf (gethash (first set) *all-possible-chroma-subsets-hash*)  
            (second set)))
    (dolist (set sets-list) 
      (when (zerop (second (second set)))
      (setf (get  (first (second set)) :prime) (first set))))))

(fill-possible-chroma-subsets-hash)


 ; =============================================================== ;
   ;;; CONVERSION: OM SYMBOL <-> OMCS SYMBOL
   
   ;;; by Paulo Raposo
; =============================================================== ;

(defun omcs-symb->om (omcs-sym)
  (let ( (write-symbol (lambda (x)
                         (if (not (string-equal "OM" (package-name (symbol-package x))))
                             (car (om::list! (intern (string-upcase x) :om)))
                             x))))
    (if (atom omcs-sym)
         (funcall write-symbol omcs-sym)
    (mapcar write-symbol omcs-sym)))) 

(defun om-symb->omcs (om-sym)
  (let ( (read-symbol (lambda (x)  (if (not (string-equal "OMCS" (package-name (symbol-package x))))
                                       (car (om::list! (intern (string-upcase x) :omcs)))
                                       x))))
    (if (atom om-sym)
         (funcall read-symbol om-sym)
         (mapcar read-symbol om-sym))))

; =============================================================== ;
	 
(defun prime (fn)
(get (om-symb->omcs fn) :prime)) ;convert om symbol to omcs (phr)

;(time (om::repeat-n (prime 'om::4-17) 10000))
;(prime 'om::0-1)
;==============
(defun calc-6vect (SC)
  (let ((res (make-list 6 :initial-element 0))
        (prime (prime SC))
        temp int ref)
    (om::while (cdr prime)
      (setq ref (pop prime))
      (setq temp prime)
      (om::while temp
        (setq int (- (first temp) ref))
        (when (> int 6) (setq int (- 12 int)))
        (setf (nth (1- int) res) (1+ (nth (1- int) res)))
        (pop temp)))
    res))
	
;(calc-6vect '3-1) 
;(calc-6vect '0-1) 
		
(defun store-SC-icvectors ()
  (dolist (SCs *all-SC-names*)
    (dolist (SC SCs)
      (setf (get SC :icv) (calc-6vect SC)))))

(store-SC-icvectors)

;==============
(om::defmethod! card ((SC t))
  :initvals '('4-1)
  :indoc '("SC")
  :icon 403
  :doc "returns the cardinality of SC"
  (length (prime SC)))

;(time (repeat 10000 (card '12-1)))

(om::defmethod! ICV (SC)
  :initvals '('4-1)
  :indoc '("SC")
  :icon 403
  :doc "returns the interval-class vector (ICV) of SC"
  (get (om-symb->omcs SC) :icv))
	  
(defun make-set (l)
  (let (lst)
       (om::while l (push (mod (pop l) 12) lst))
    (sort (delete-duplicates  lst) #'<)))

(defun SC-name-from-points (midis)
  (omcs-symb->om (car (gethash  (make-set midis) *all-possible-chroma-subsets-hash*))))

(defun SC-name-from-pcs (pcs)
"pcs has to be a list of  pitch classes (only numbers from 0 to 11)
and should not include duplicates !!!"
 (omcs-symb->om (car (gethash (sort (copy-list pcs) #'<) *all-possible-chroma-subsets-hash*))))

(defun SC-name+off-from-points (midis)
 (omcs-symb->om (gethash  (make-set midis) *all-possible-chroma-subsets-hash*)))

(defun SC-name+off-from-pcs (pcs)
  (omcs-symb->om (gethash  (sort (copy-list pcs) #'<) *all-possible-chroma-subsets-hash*)))
	  
;===========================
; subsets
(defun indexi (x y fn)
(cond ((null x) y)
        (t (funcall fn
               (car x)
               (indexi (cdr x) y fn)))))

(defun all-subsets (x)
(cond ((null x) (list ()))
      (t (indexi
          (all-subsets (cdr x))
          nil
          (function (lambda (u v)
                      (cons (cons (car x) u)
                            (cons u v))))))))

;(all-subsets '(a b c))

(om::defmethod! all-subs ((SCs om::t))
:initvals '('4-1)
:indoc '("SCs")
:icon 403
:doc "returns all subset classes of SCs (a single SC or a list of SCs)"
(if (atom SCs)
  (remove-duplicates (mapcar #'(lambda (set) (SC-name-from-pcs set)) (all-subsets (prime SCs)))) 
  (remove-duplicates 
   (apply #'append 
          (mapcar #'(lambda (SC) 
                      (remove-duplicates (mapcar #'(lambda (set) (SC-name-from-pcs set)) 
                                                 (all-subsets (prime SC)))))
                  SCs)))))

;(time (all-subs '6-z6)) 
;(time (all-subs '10-1)) 
;(time (all-subs '8-1)) 
;(time (intersection card4 (all-subs '6-z6))) 
;(time (intersection card4 (all-subs '8-6))) 
;(time (intersection card6 (all-subs '10-1))) 
;(time (all-subs '(6-z3a 6-5a 6-5b 6-z6 6-z11b 6-z12a 6-z12b 6-z17a 6-z17b 6-18a 6-18b 6-z36a 6-z38 6-z41a 6-z41b 6-z43a 6-z43b)))

#|
(defun store-SC-all-subs ()
  (dolist (SCs *all-SC-names*)
    (dolist (SC SCs)
      (all-subs SC))))
;      (setf (get SC :all-subs) (all-subs SC)))))

; too slow (109.628 seconds)
;(time (store-SC-all-subs)) 
|#
;===========================
				  
(defun subsets (SC card)
"returns all subset classes of cardinality card of SC"
(remove-duplicates
 (mapcar #'SC-name-from-pcs
         (PMC (make-list card :initial-element (prime SC))
              '((* ?1 ?2 (?if (< ?1 ?2)))
                (* ?1 ?2 (?if (not (member ?2 (rest rl))))))
              :sols-mode :all))))

;(time (subsets '8-6 4)) 
;(time (subsets '6-z3a 4)) 

(defun supersets (SC card)
"returns all superset classes of cardinality card of SC"
(let* ((prime (prime SC))
      (set-diff (nreverse (set-difference '(0 1 2 3 4 5 6 7 8 9 10 11) prime)))
      (s-space (make-list (- card (card SC)) :initial-element set-diff)))
(remove-duplicates
 (mapcar #'(lambda (l) (SC-name-from-pcs (append prime l))) 
         (PMC s-space
              '((* ?1 ?2 (?if (< ?1 ?2)))
                (* ?1 ?2 (?if (not (member ?2 (rest rl))))))
              :sols-mode :all)))))

;(time (supersets '4-z15a 9)) 
  
(defun MEMBER-SETS (sc) 
  (let ((prime (prime sc)) res)
    (om::for (int 0 1 11)
      (push (mapcar #'(lambda (n) (mod (+ int n) 12)) prime) res))
    (nreverse res))) 

(defun complement-pcs (sc) 
  (reverse (set-difference '(0 1 2 3 4 5 6 7 8 9 10 11) (prime sc))))	  

#|
; for testing
(defun normal-order (SC)
(let (set)
(maphash   
 #'(lambda (key data) 
     (when (and (eq (first data) SC) (= (second data) 0)) 
       (setq set key)))
 *all-possible-chroma-subsets-hash*)
set))

(defun test-setns ()
(let ((lst (all-subsets '(0 1 2 3 4 5 6 7 8 9 10 11)))
     (fl t))
(while (and fl lst)
 (when (car lst)
  (when (not (eq (first (gethash (car lst) *all-possible-chroma-subsets-hash*))
                 (pcs::set-name-from-points (car lst))))
     (setq fl nil)))
   (pop lst))
fl))
;(test-setns)  

(defun test-primes ()
(let (false)
(dolist (SCs *all-SC-names*)
  (dolist (SC SCs)
    (unless (equal (pcs::prime (symbol-value SC)) (get SC :prime))
      (print (list SC (pcs::prime (symbol-value SC)) (get SC :prime)))
      (push SC false))))
false))
;(test-primes)


(defun test-icvs ()
(let (false)
(dolist (SCs *all-SC-names*)
  (dolist (SC SCs)
    (unless (equal (coerce (pcs::icv (symbol-value SC)) 'list) (get SC :icv))
      (print (list SC (coerce (pcs::icv (symbol-value SC)) 'list) (get SC :icv)))
      (push SC false))))
false))
;(test-icvs)

(defun eq-lists (l1 l2)
(let (c)
(while (and l1 l2)
  (setq c (car l1))
  (setq l1 (remove c l1))
  (setq l2 (remove c l2)))
(list l1 l2)))
|#


(defun eq-set (setnames &rest notes)
"setnames can be SC or list of SC's, notes can be midis or a list of midis"
(setq setnames (om::list! setnames))
(when (listp (car notes)) (setq notes (car notes)))
(member (SC-name-from-points notes)  setnames))

(om::defmethod! omcs::eq-SC? ((set-classes om::t) &rest midis) 
  :initvals '('4-1)
  :indoc '("set-classes" "midis")
  :icon 403
  :doc "checks whether the SC identity of midis (a list of midi-values) is found
in set-classes (a list of SC-names).
set-classes can be a single SC or list of SCs, midis individual midi-values
or a list of midis."
  (setq set-classes (om::list! set-classes)) 
  (when (listp (car midis)) (setq midis (car midis))) 
  (member (SC-name-from-points midis)  set-classes))
  
;(defun eq-SC? (set-classes &rest midis)
;"checks whether the SC identity of midis (a list of midi-values) is found
;in set-classes (a list of SC-names).
;set-classes can be a single SC or list of SCs, midis individual midi-values
;or a list of midis."
;(setq set-classes (om::list! set-classes)) 
;(when (listp (car midis)) (setq midis (car midis))) 
;(member (SC-name-from-points midis)  set-classes))

;(eq-SC? '(3-11a 3-11b) 60 64 67))
;(eq-SC? '(3-11a 3-11b) '(60 64 67))
	                                            
;===============================================
;;;===> SCS

(om::defmethod! SC-subsets ((fn om::t))
  :initvals '('(om::|6-27A| om::|6-27B|))
:indoc '("fn symbol or list") 
  :doc "Return all subsets from a set-class."
    :icon 403
(omcs-symb->om (all-subs fn)))

(om::defmethod! SCs-card ((card integer))
  :initvals '(6)
:indoc '("integer" ) 
:menuins '((0 (("1" 1)  ("2" 2) ("3" 3) ("4" 4) ("5" 5) ("6" 6) ("7" 7) ("8" 8) ("9" 9) ("10" 10) ("11" 11) ("12" 12))))
:doc "Return all fn symbols."
:icon 403
(omcs-symb->om
 (case card 
 (1 card1)
 (2 card2)
 (3 card3 )
 (4 card4 )
 (5 card5 )
 (6 card6 )
 (7 card7 )
 (8 card8 )
 (9 card9 )
 (10 card10 )
 (11 card11 )
 (12 card12))))

(om::defmethod! SC+off ((midis list)) 
  :initvals '((60 61))
  :indoc '("midis")
  :icon 403
  :doc  "returns a list containing the SC-name and the offset 
(i.e. the transposition relative to the prime form of the SC) of 
 midis (a list of midi-values), midis can also be a list of lists 
 of midis in which case SC+off returns the SCs with offsets 
 for each midi-value sublist."
  (if (atom (car midis))
      (let ((res (gethash (make-set midis) *all-possible-chroma-subsets-hash*)))
         (om::x-append (omcs-symb->om (first res)) (second res)))
    (let (res)
      (dolist (midis-l midis)
        (push (gethash  (make-set midis-l) *all-possible-chroma-subsets-hash*) res))
      (mapcar #'(lambda (x)
       (om::x-append (omcs-symb->om (first x)) (second x))) (nreverse res)))))

(om::defmethod! SC-name ((midis list)) 
  :initvals '((60 61))
  :indoc '("midis")
  :icon 403
  :doc  "returns a list containing the SC-name and the offset 
(i.e. the transposition relative to the prime form of the SC) of 
 midis (a list of midi-values), midis can also be a list of lists 
 of midis in which case SC+off returns the SCs with offsets 
 for each midi-value sublist."
  (if (atom (car midis))
      (omcs-symb->om (car (gethash (make-set midis) *all-possible-chroma-subsets-hash*)))
    (let (res)
      (dolist (midis-l midis)
        (push (car (gethash  (make-set midis-l) *all-possible-chroma-subsets-hash*)) res))
      (mapcar #'omcs-symb->om (nreverse res)))))

(om::defmethod! sub/supersets ((SC t) (card number))
  :initvals '('om::4-z15a 9)
  :indoc '("SC" "card")
  :icon 403
  :doc "returns all subset classes of SC (when card is less than the cardinality of SC)
or superset classes (when card is greater than the cardinality of SC) 
of cardinality card."
  (if (= (card (om-symb->omcs SC)) card)
    SC
    (if (> (card (om-symb->omcs SC)) card)
      (omcs-symb->om (subsets (om-symb->omcs SC) card))
      (omcs-symb->om (supersets (om-symb->omcs SC) card)))))

(om::defmethod! SC-info ((menu symbol) (sc-name symbol))
  :initvals '(:prime 'om::4-z15a)
:indoc '("mode" "SC" ) 
:menuins '((0 (("prime" :prime) ("icv" :icv)  ("member-sets" :member-sets) ("complement-pcs" :complement-pcs))))
  :doc "Returns the selected info (prime-form, interval class vector, members-sets or complement) about an SC."
    :icon 403
(cond 
((equal menu :prime) (prime sc-name))
((equal menu :icv) (icv sc-name))
((equal menu :member-sets) (member-sets sc-name))
((equal menu :complement-pcs) (complement-pcs sc-name))
(t (progn (om::om-message-dialog "Please select a valid mode (:prime, :icv, :member-sets or :complement-pcs).") (om::om-abort)))))

(om::defmethod! SC-info ((menu symbol) (sc-name list))
  (mapcar #'(lambda (sc) (sc-info sc menu)) sc-name))
  
;---------
(defun format-card-table ()
  (loop for i in omcs::*all-flat-SC-names*
        collect (list (format nil "~S" i) (format nil "~S" i))))

(defparameter *mytable* nil)
(setf *mytable* (format-card-table))


(om::defmethod! omcs::all-SC-names (sc)
  :initvals '("0-1")
  :indoc '("SC name")
  :menuins (list (list 0 *mytable*))
  :doc "Outputs a Set-Class name."
  :icon 404
  (car (om::list! (read-from-string sc)))
  )
;--------

;; ===============================================
;; NEW - PHRAPOSO (2024) - NORMAL-ORDER

(defparameter *normal-order-data-file* (namestring
                              (make-pathname :directory (pathname-directory *load-pathname*)
                                             :name "normal-order-data.lisp")))

(defvar *normal-order-hash* (make-hash-table :test #'equal))

(defun fill-normal-order-hash () 
  (let (normal-order-list) 
    (with-open-file (in *normal-order-data-file* :direction :input)
      (setq normal-order-list (eval (read in))))
    (clrhash *normal-order-hash*)
    (dolist (n-ord normal-order-list) 
      (setf (gethash (first n-ord) *normal-order-hash*)  
            (second n-ord)))))

(fill-normal-order-hash)

(defun get-n-ord (pcs)
"pcs has to be a list of  pitch classes (only numbers from 0 to 11)
in ascending order and should not include duplicates !!!" ;used with (make-set pcs)
 (gethash pcs *normal-order-hash*))

(defun mc->pc (midics)
 (cond ((null midics) nil)
	   ((atom midics) (mod (/ midics 100) 12))
	   (t (om::x-append (mc->pc (car midics))
	       (mapcar #'mc->pc (cdr midics))))))
 
(om::defmethod! normal-order ((input list) (mode string))  
  :initvals '((60 67 64 71) "pc/midi") 
:indoc '("list of midics or pitch classes" "pc/midi or midic") 
  :doc "Returns the normal order [list of integers] of the given set [midicents or pitch classes]."
:menuins '((1 (("midic" "midic") ("pc/midi" "pc/midi"))))
    :icon 403
(if (equal mode "midic")
    (om::list! (get-n-ord (make-set (mc->pc input))))
    (om::list! (get-n-ord (make-set input)))))
