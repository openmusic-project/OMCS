;===============================================
;;; This is a preliminary release of the pmc engine from the 
;;; PWConstraints by Mikael Laurson (c), 1995
;;; 
;;; The pmc engine transported to OpenMusic 1999 (by Orjan Sandred)
;;; 
;;; All copyright belongs to Mikael Laurson, who also is the author of 
;;; the code for this library.
;===============================================
;--------------------------------------------------
(In-package :cl-user)

  
;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

(defvar *OM_OMCS-lib-files* nil)
(setf *OM_OMCS-lib-files* (list 
                           (om::om-relative-path '("sources" "Load-OMCs") "OMConstraints-package")
                           (om::om-relative-path '("sources" "OMConstraints-kernel") "constraint-utilities")
                           
                           (om::om-relative-path '("sources" "OMConstraints-kernel") "constraint-diagnostics")
                           (om::om-relative-path '("sources" "OMConstraints-kernel") "search-variable")
                           (om::om-relative-path '("sources" "OMConstraints-kernel") "search-engine")
                           (om::om-relative-path '("sources" "OMConstraints-kernel") "PM-compiler")
                           (om::om-relative-path '("sources" "OMConstraints-kernel") "forward-checking")
                           (om::om-relative-path '("sources" "OMConstraints-kernel") "OMCs-utilities-split1")
                          ))

;--------------------------------------------------
;Loading files 
;--------------------------------------------------
(mapc #'om::compile&load *OM_OMCS-lib-files*)
(in-package omcs)

;--------------------------------------------------
; RC subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(defvar *subpackages-list* nil)
(setf *subpackages-list*
      '(
        ("01-PMC" nil nil (pmc-engine) nil)
        ))
;--------------------------------------------------
;filling packages
;--------------------------------------------------
(om::fill-library *subpackages-list*)



;ERRORS function while? in constraint-utilities.lisp, search-variable.lisp