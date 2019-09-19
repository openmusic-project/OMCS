;===============================================
;;; This is a preliminary release of the pmc engine from the 
;;; PWConstraints by Mikael Laurson (c), 1995
;;; 
;;; The pmc engine transported to OpenMusic 1999 (by Orjan Sandred)
;;; 
;;; All copyright belongs to Mikael Laurson, who also is the author of 
;;; the code for this library.

;===============================================

;version 0.50 is the first version released on IRCAM Forumm CDrom
;version 0.53 adds a graphical interface for rules
;version 1.0 corrects a bug for the l variable (27/01/2004)
;version 1.01 corrects a bug in the index rule (function compile-pattern-matching-rule)
;version 1.02 updated the code to load properly in OM 6.12
;   Known issue: icons are not showing properly (default icon is used)
;version 1.1 merge versions/update encoding and files

;--------------------------------------------------
(in-package :cl-user)
 

;--------------------------------------------------
;Loading files 
;--------------------------------------------------
(mapc #'(lambda (file) 
          (compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "sources")) :name file)))
      '(
        "package"
        "constraint-utilities"
        "constraint-diagnostics"  
        "search-variable"
        "search-engine"
        "PM-compiler"
        "forward-checking"
        "OMCs-utilities-split1"
        "Graphic-rule-interface"
        ))


(in-package :omcs)

;--------------------------------------------------
;filling packages
;--------------------------------------------------
; RC subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------

(om::fill-library '(
        ("01-PMC" nil nil (pmc-engine) nil)
        ("02-RULES" nil nil (wildcard-rule index-rule) nil)
        ("03-TOOLS" nil nil (partial-solution rev-partial-solution current-index) nil)
        ))


(om::set-lib-release 1.2)

(print "
;;;=============================================
;;; OMCS 1.2
;;; PMC engine for OM by M. Laurson / O. Sandred
;;;=============================================
")

;ERRORS function while? in constraint-utilities.lisp, search-variable.lisp