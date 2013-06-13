;;; package.lisp --- Package items

;; Copyright (C) 2010, 2011, 2012 Didier Verna

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


;; ------------------
;; Docstring protocol
;; ------------------

(defmethod docstring ((package package))
  "Return PACKAGE's docstring."
  (documentation package t))


;; ---------------------------------------
;; Definition package definitions protocol
;; ---------------------------------------

;; #### NOTE: contrary to DEFINITION-FILE-DEFINITIONS, this function could be
;; optimized a bit. For instance, when we figure out that a generic function
;; does belong to a package, we know that all methods do to because they share
;; the same name. This means that we could save some package name comparison
;; tests.
(defgeneric definition-package-definitions (definition package)
  (:documentation
   "Return the list of definitions from DEFINITION that belong to PACKAGE.")
  (:method (definition package)
    "Default method for definitions not containing sub-definitions."
    (when (eq (symbol-package (definition-symbol definition)) package)
      (list definition)))
  (:method ((macro macro-definition) package)
    "Handle MACRO and its setf expander."
    (nconc (call-next-method)
	   (when (macro-definition-expander macro)
	     (definition-package-definitions
	      (macro-definition-expander macro)
	      package))))
  (:method ((accessor accessor-definition) package)
    "Handle ACCESSOR, its writer and its setf expander."
    (nconc (call-next-method)
	   (when (accessor-definition-writer accessor)
	     (definition-package-definitions
	      (accessor-definition-writer accessor)
	      package))
	   (when (accessor-definition-expander accessor)
	     (definition-package-definitions
	      (accessor-definition-expander accessor)
	      package))))
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
    "Handle GENERIC-ACCESSOR, its generic writer and its setf expander."
    (nconc (call-next-method)
	   (when (generic-accessor-definition-writer generic-accessor)
	     (definition-package-definitions
	      (generic-accessor-definition-writer generic-accessor)
	      package))
	   (when (generic-accessor-definition-expander generic-accessor)
	     (definition-package-definitions
	      (generic-accessor-definition-expander generic-accessor)
	      package)))))

(defun package-definitions (package definitions)
  "Return the subset of DEFINITIONS that belong to PACKAGE."
  (mapcan-definitions-pool
   (lambda (definition) (definition-package-definitions definition package))
   definitions))


;;; package.lisp ends here
