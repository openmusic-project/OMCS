;
;  PWConstraints by Mikael Laurson (1995)
;  All copyright belongs to Mikael Laurson, who also is the author of 
;  the code for the PWConstraints library.
;
;  Functions for graphical interface in OM by Ørjan Sandred 1999 and Aug-Sept 2002 (Paris/Stockholm)
;  
;
;   Use the wildcard-rule and i-rule functions to build rules graphicaly for the pmc engine.
;   Also works for heuristic rules. See example patches.
;
;------ wildcard-rule

(in-package omcs)

(defun write-one-searchvariable (variable-nr)
  (read-from-string (concatenate 'string "?" (format nil "~D" variable-nr))))


(defun write-searchvariables (nr-of-variables)
  (mapcar 'write-one-searchvariable (om::arithm-ser 1 nr-of-variables 1)))


(defun make-wildcard-rule (function)

  "Takes any function and creats a pmc rule out of it. The inputs to the function 
will be called ?1 ?2 ?3.... ?n. The function migth be an OM patch in the lambda state."

  (let ((nr-? (length (ccl::function-lambda-list function))))
    (append '(*) (write-searchvariables nr-?) (list (list 'common-lisp-user::?if 
                                                          (append (list 'funcall function) (write-searchvariables nr-?))
                                                          )))))



(defun make-wildcard-rule2 (function var1 var2)
  
  "Takes any function and creats a pmc rule out of it. The inputs to the function 
will be called ?1 ?2 ?3.... ?n. The function migth be an OM patch in the lambda state.
If the inputs var1 var2 has any value but 'om::no_input, these will replace the last 
inputs to the rule."
  
  (let ((nr-var 0)        
        nr-?)
    (if (not (equal var1 'om::no_input))  (setf nr-var 1))
    (if (not (equal var2 'om::no_input)) (setf nr-var (+ nr-var 1)))
    (setf nr-? (- (length (ccl::function-lambda-list function)) nr-var))
    (if (< nr-? 0) (print "Not enough inputs to rule")
        (append '(*) (write-searchvariables nr-?) 
                (list (list 'common-lisp-user::?if 
                            (append (list 'funcall function) 
                                    (write-searchvariables nr-?)
                                    (remove 'om::no_input (list (if (listp var1) 
                                                                  (append '(quote) (list var1))
                                                                  var1)
                                                                (if (listp var2) 
                                                                  (append '(quote) (list var2))
                                                                  var2))))))))))

;------ i-rule

(defun write-one-searchvariable-i (variable-nr)
  (read-from-string (concatenate 'string "i" (format nil "~D" variable-nr))))

(defun write-searchvariables-i (variable-nr-list)
  (mapcar 'write-one-searchvariable-i variable-nr-list))

(defun make-i-rule (function i-nr-list)
  
  "no doc"
  
  (let ((nr-inputs (length (ccl::function-lambda-list function))))
    (if (/= nr-inputs (length i-nr-list))
      (print "error when making rule: number of arguments must be the same as number of i")
      (append (write-searchvariables-i i-nr-list) (list (list 'common-lisp-user::?if 
                                                              (append (list 'funcall function) (write-searchvariables-i i-nr-list))
                                                              ))))))


(defun make-i-rule2 (function i-nr-list var1 var2)
  
  "no doc"
  
  (let ((nr-var 0) nr-inputs)
    (if (not (equal var1 'om::no_input))  (setf nr-var 1))
    (if (not (equal var2 'om::no_input)) (setf nr-var (+ nr-var 1)))
    (setf nr-inputs (- (length (ccl::function-lambda-list function)) nr-var))
    (if (/= nr-inputs (length i-nr-list))
      (print "error when making rule: number of arguments must be the same as number of i")
      (append (write-searchvariables-i i-nr-list) 
              (list (list 'common-lisp-user::?if 
                          (append (list 'funcall function) 
                                  (write-searchvariables-i i-nr-list)
                                  (remove 'om::no_input (list (if (listp var1) 
                                                                (append '(quote) (list var1))
                                                                var1)
                                                              (if (listp var2) 
                                                                (append '(quote) (list var2))
                                                                var2))))))))))


;------ OM interface


(om::defmethod! omcs::wildcard-rule ((function function)
                                     &optional (variable1 'om::no_input) (variable2 'om::no_input))
  
  :initvals '(nil 'om::no_input 'om::no_input)
  :indoc '("function" "variable1" "variable2")
  :icon 401
  :doc "Takes any function and creats a pmc rule out of it. The inputs to the function 
will be called ?1 ?2 ?3.... ?n. The function migth be an OM patch in the lambda state.

A rule should output either true or false, an heuristic rule should output a number.

If the inputs variable1 and/or variable2 are used, the last inputs to the rule will
always get the values from these inputs in the place of the search variable (i.e.
if variable1 is used, and the rule has 3 inputs, input 1 and to will be used for ?1 and ?2,
and input 3 will be used for the value of variable1."



  (make-wildcard-rule2 function variable1 variable2))


(om::defmethod! omcs::index-rule ((function function) (index-numbers list)
                                  &optional (variable1 'om::no_input) (variable2 'om::no_input))
  
  :initvals '(nil '(1) 'om::no_input 'om::no_input)
  :indoc '("function" "list" "variable1" "variable2")
  :icon 402
  :doc "Takes any function and creats a pmc index rule out of it. The inputs to the function 
will be assigned to the index numbers given in the list of index-numbers (and the index numbers 
must be exactly as many as arguments to the function). The function 
migth be an OM patch in the lambda state.

A rule should output either true or false, an heuristic rule should output a number.

If the inputs variable1 and/or variable2 are used, the last inputs to the rule will
always get the values from these inputs in the place of the search variable (i.e.
if variable1 is used, and the rule has 3 inputs, input 1 and to will be used for the first
and second index variable, and input 3 will be used for the value of variable1."
 
    
  (make-i-rule2 function index-numbers variable1 variable2))


(om::defmethod! omcs::current-index ()
  :doc "returns the current search-index (counted from 1)"
  :icon 403    
  (omcs::cur-index))

(om::defmethod! omcs::current-length ()
  :doc "returns the number of search-variables of the current search-engine"
  :icon 404    
  (omcs::cur-slen))

(om::defmethod! omcs::partial-solution ()
  :doc "returns the partial-solution"
  :icon 405
  (let ((my-search-engine omcs::*current-SE*))
    (omcs::read-key (svref (omcs::search-variables my-search-engine) (omcs::variable-pos my-search-engine)) :sols-list)))

(om::defmethod! omcs::rev-partial-solution ()
  :doc "returns the partial-solution in reverse"
  :icon 406
  (let ((my-search-engine omcs::*current-SE*))
    (omcs::read-key (svref (omcs::search-variables my-search-engine) (omcs::variable-pos my-search-engine)) :rev-sols-list)))


;(read-key s-var :domain)

;(defmethod get-all-variables ((self omcs::search-engine))
;  (mapcar #'(lambda (position) (omcs::value (svref (omcs::search-variables self) position)))
;          (om::arithm-ser 0 (1- (omcs::variable-pos self)) 1)))

;(defun test ()
;  (get-all-variables omcs::*current-SE*))




