;===============================================
;;; This is a preliminary release of the pmc engine from the 
;;; PWConstraints by Mikael Laurson (c), 1995
;;; 
;;; The pmc engine transported to OpenMusic 1999 (by Orjan Sandred)
;;; Extra Portage to OpenMusic 2021 (by Karim Haddad)
;;; 
;;; All copyright belongs to Mikael Laurson, who is the author of 
;;; the code for this library.

;===============================================

;version 0.50 is the first version released on IRCAM Forumm CDrom
;version 0.53 adds a graphical interface for rules
;version 1.0 corrects a bug for the l variable (27/01/2004)
;version 1.01 corrects a bug in the index rule (function compile-pattern-matching-rule)
;version 1.02 updated the code to load properly in OM 6.12
;   Known issue: icons are not showing properly (default icon is used)
;version 1.1 merge versions/update encoding and files
;version 1.3:
; - pmc-engine now uses key input instead of optional input
; - pc-theory pack ported
; - many code fixes
; - added examples
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
        "omcs-preferences"
        ))

(mapc #'(lambda (file) 
          (compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) 
                                                          (list "sources" "PC-set-theory")) :name file)))
      '(
	  "SCs-data"
	  "normal-order-data"
      "all-SCs"
      "PC-set-theory" 
        ))

(mapc #'(lambda (file) 
          (compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) 
                                                          (list "sources")) :name file)))
      '("OMCs-utilities"
        ))

(in-package :omcs)

;--------------------------------------------------
;filling packages
;--------------------------------------------------
; RC subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------

(om::fill-library '(
        ("01-PMC" nil nil (pmc-engine engine-info) nil)
        ("02-RULES" nil nil (wildcard-rule index-rule) nil)
        ("03-TOOLS" nil nil (partial-solution rev-partial-solution current-index current-length) nil)
        ("04-PC-SET-THEORY" (("Tools" nil nil (card eq-SC? ICV prime subsets supersets) nil))
         nil (SC-name SC+off SCs-card SC-info all-sc-names sub/supersets SC-subsets normal-order) nil)
        ))


;(om::set-lib-release 1.5)

(print "
;;;=============================================
;;; OMCS 1.5
;;; PMC engine for OM by M. Laurson / O. Sandred
;;; Augmented and revised by K. Haddad
;;; Pc-set theory: revised by Paulo Raposo
;;;=============================================
")


