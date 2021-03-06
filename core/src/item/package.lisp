;;; package.lisp --- Package items

;; Copyright (C) 2010-2013, 2017 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of Declt.

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


;;; Commentary:



;;; Code:

(in-package :net.didierverna.declt)
(in-readtable :net.didierverna.declt)


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
  (reveal (package-name package)))



;; ==========================================================================
;; Item Protocols
;; ==========================================================================

;; ---------------
;; Source protocol
;; ---------------

;; #### PORTME.
(defmethod source ((package package))
  "Return PACKAGE's definition source."
  (when-let (defsrc (sb-introspect:find-definition-source package))
    (sb-introspect:definition-source-pathname defsrc)))


;; ------------------
;; Docstring protocol
;; ------------------

(defmethod docstring ((package package))
  "Return PACKAGE's docstring."
  (documentation package t))


;; ------------------
;; Type name protocol
;; ------------------

(defmethod type-name ((package package))
  "Return \"package\"."
  "package")


;; ---------------------------------------
;; Definition package definitions protocol
;; ---------------------------------------

;; #### FIXME: this is wrong in at least one corner case: for aggregates which
;; #### names and slot names come from different packages (and this is a
;; #### consequence of the overly complicated pool structure).

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
    (when (eq (definition-package definition) package)
      (list definition)))
  (:method ((macro macro-definition) package)
    "Handle MACRO and its setf expander."
    (nconc (call-next-method)
	   (when (macro-definition-access-expander macro)
	     (definition-package-definitions
	      (macro-definition-access-expander macro)
	      package))))
  (:method ((accessor accessor-definition) package)
    "Handle ACCESSOR, its writer and its setf expander."
    (nconc (call-next-method)
	   (when (accessor-definition-writer accessor)
	     (definition-package-definitions
	      (accessor-definition-writer accessor)
	      package))
	   (when (accessor-definition-access-expander accessor)
	     (definition-package-definitions
	      (accessor-definition-access-expander accessor)
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
	   (when (generic-accessor-definition-access-expander generic-accessor)
	     (definition-package-definitions
	      (generic-accessor-definition-access-expander generic-accessor)
	      package)))))

(defun package-definitions (package definitions)
  "Return the subset of DEFINITIONS that belong to PACKAGE."
  (mapcan-definitions-pool
   (lambda (definition) (definition-package-definitions definition package))
   definitions))


;;; package.lisp ends here
