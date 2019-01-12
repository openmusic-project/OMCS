;===============================================
;===============================================

;;; PWConstraints by Mikael Laurson (c), 1995

;===============================================
;===============================================
(in-package omcs)
;===============================================
;added from PatchWork by Orjan
(defun cirlist (elem)
  "makes a circular list out of elem"
  (setq elem (list elem))
  (rplacd elem elem))
 
;===============================================
;  User search-engine functions
;===============================================
(defun engine () 
  "returns the current search-engine" *current-SE*)

(defun cur-index () 
  "returns the current search-index (counted from 1)" 
  (1+ (variable-pos *current-SE*)))

(defun cur-slen () 
  "returns the number of search-variables of the current search-engine" 
  (length (search-variables *current-SE*)))



;****************************
(om::defmethod! omcs::pmc-engine ((s-space list)
                                  (rules list)
                                  &optional (heuristic-rules nil) (fwc-rules nil) (sols-mode :once) (rnd? t) (print-fl nil))
  
  :initvals '(nil nil nil nil :once t nil)
  :indoc '("s-space" "rules" "heuristic-rules" "fwc-rules" "sols-mode" "random?" "print-indexnr?" )
  :doc "PMC is the search engine from Mikael Laurson's PWConstraints library (PatchWork).
PMC first creates a search-engine and then starts the search.  
After the search is completed, PMC returns a list of solutions. 
The solutions should satisfy the constraints
given by the user. For more details, refer to the PWConstraints documentation.
PMC has the following arguments:
- s-space        a list of domains for each search-variable  
- rules            a list of rules ('ordinary' PWConstraints rules)  
- heuristic-rules  a list of heuristic rules 
- fwc-rules    a list of forward-checking rules  
- sols-mode    indicates the number of solutions required:
    :once, the default case, one solution,
    :all   all solutions, 
     sols-mode can also be a positive integer giving the number of desired solutions. 
- rnd?             a flag indicating whether or not the search-space 
         is randomly reordered (by default rnd? is true). 
- print-fl       a flag. If print-fl is true then the index of the current search-variable 
   is printed on the Listener window indicating how far the search has proceeded. 

"
  :icon 400
    
  (time (pmc s-space rules :fwc-rules fwc-rules :heuristic-rules heuristic-rules :sols-mode sols-mode :rnd? rnd? :print-fl print-fl)))