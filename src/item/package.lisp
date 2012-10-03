;;; package.lisp --- Package items

;; Copyright (C) 2010, 2011 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>

;; This file is part of Declt.

;; Declt is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 3,
;; as published by the Free Software Foundation.

;; Declt is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; Contents management by FCM version 0.1.


;;; Code:

(in-package :com.dvlsoft.declt)
(in-readtable :com.dvlsoft.declt)


;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun package-external-symbols (package &aux external-symbols)
  "Return the list of external symbols from PACKAGE."
  (do-external-symbols (symbol package external-symbols)
    (when (eq (symbol-package symbol) package)
      (push symbol external-symbols))))

(defun package-internal-symbols
    (package &aux (external-symbols (package-external-symbols package))
		  internal-symbols)
  "Return the list of internal definitions from PACKAGE."
  (do-symbols (symbol package internal-symbols)
    (when (and (not (member symbol external-symbols))
	       (eq (symbol-package symbol) package))
      (push symbol internal-symbols))))



;; ==========================================================================
;; Rendering Protocols
;; ==========================================================================

(defmethod name ((package package))
  "Return PACKAGE's name."
  (package-name package))



;; ==========================================================================
;; Item Protocols
;; ==========================================================================

;; ---------------
;; Source protocol
;; ---------------

;; #### PORTME.
(defmethod source
    ((package package)
     &aux (defsrc (sb-introspect:find-definition-source package)))
  "Return PACKAGE's definition source."
  (when defsrc (sb-introspect:definition-source-pathname defsrc)))


;; ---------------------------------------
;; Definition package definitions protocol
;; ---------------------------------------

(defgeneric definition-package-definitions (definition package)
  (:documentation
   "Return the list of definitions from DEFINITION that belong to PACKAGE.")
  (:method (definition package)
    "Default method for definitions not containing sub-definitions."
    (when (eq (symbol-package (definition-symbol definition)) package)
      (list definition)))
  (:method ((accessor accessor-definition) package)
    "Handle ACCESSOR and its writer function."
    (nconc (call-next-method)
	   (definition-package-definitions
	    (accessor-definition-writer accessor)
	    package)))
  (:method ((accessor-method accessor-method-definition) package)
    "Handle ACCESSOR-METHOD and its writer method."
    (nconc (call-next-method)
	   (definition-package-definitions
	    (accessor-method-definition-writer accessor-method)
	    package)))
  (:method ((generic generic-definition) package)
    "Handle GENERIC function and its methods."
    (nconc (call-next-method)
	   (mapcan (lambda (method)
		     (definition-package-definitions method package))
		   (generic-definition-methods generic))))
  (:method ((generic-accessor generic-accessor-definition) package)
    "Handle GENERIC-ACCESSOR and its generic writer function."
    (nconc (call-next-method)
	   (definition-package-definitions
	    (generic-accessor-definition-writer generic-accessor)
	    package))))

(defun package-definitions (package definitions)
  "Return the subset of DEFINITIONS that belong to PACKAGE."
  (sort (mapcan (lambda (definition)
		  (definition-package-definitions definition package))
		definitions)
	#'string-lessp
	:key #'definition-symbol))


;;; package.lisp ends here
