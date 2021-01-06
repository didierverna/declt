;;; package.lisp --- Package definitions

;; Copyright (C) 2010-2013, 2017, 2020 Didier Verna

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
;; Package Definitions
;; ==========================================================================

(defstruct package-definition
  "Structure for package definitions.
This structure holds the corresponding package, the package definitions for
its used-list and used-by-list, its exported and internal definitions, and a
slot for marking foreign packages, i.e. those which do not pertain to the
system being documented."
  package
  use-list
  used-by-list
  external-definitions
  internal-definitions
  foreignp)


;; ----------------
;; Pseudo-accessors
;; ----------------

(defun package-definition-nicknames (package-definition)
  "Return the list of nicknames for PACKAGE-DEFINITION."
  (package-nicknames (package-definition-package package-definition)))



;; ==========================================================================
;; Extraction Protocols
;; ==========================================================================

;; ---------------
;; Source protocol
;; ---------------

;; #### PORTME.
(defmethod source ((package package))
  "Return PACKAGE'source."
  (when-let ((defsrc (sb-introspect:find-definition-source package)))
    (sb-introspect:definition-source-pathname defsrc)))

(defmethod source ((package-definition package-definition))
  "Return PACKAGE-DEFINITION'source."
  (source (package-definition-package package-definition)))


;; ------------------
;; Docstring protocol
;; ------------------

(defmethod docstring ((package-definition package-definition))
  "Return PACKAGE-DEFINITION's docstring."
  (documentation (package-definition-package package-definition) t))


;; ------------------
;; Type name protocol
;; ------------------

;; #### FIXME: this needs to go away at some point, when package definitions
;; are in complete use.
(defmethod type-name ((package package))
  "Return \"package\"."
  "package")

(defmethod type-name ((package-definition package-definition))
  "Return \"package\"."
  "package")



;; ==========================================================================
;; Finalization
;; ==========================================================================

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
	   (when (access-expander-definition macro)
	     (definition-package-definitions
	      (access-expander-definition macro)
	      package))))
  (:method ((accessor accessor-definition) package)
    "Handle ACCESSOR, its writer and its setf expander."
    (nconc (call-next-method)
	   (when (writer-definition accessor)
	     (definition-package-definitions
	      (writer-definition accessor)
	      package))
	   (when (access-expander-definition accessor)
	     (definition-package-definitions
	      (access-expander-definition accessor)
	      package))))
  (:method ((accessor-method accessor-method-definition) package)
    "Handle ACCESSOR-METHOD and its writer method."
    (nconc (call-next-method)
	   (definition-package-definitions
	    (writer-definition accessor-method)
	    package)))
  (:method ((generic generic-definition) package)
    "Handle GENERIC function and its methods."
    (nconc (call-next-method)
	   (mapcan (lambda (method)
		     (definition-package-definitions method package))
		   (method-definitions generic))))
  (:method ((generic-accessor generic-accessor-definition) package)
    "Handle GENERIC-ACCESSOR, its generic writer and its setf expander."
    (nconc (call-next-method)
	   (when (writer-definition generic-accessor)
	     (definition-package-definitions
	      (writer-definition generic-accessor)
	      package))
	   (when (access-expander-definition generic-accessor)
	     (definition-package-definitions
	      (access-expander-definition generic-accessor)
	      package)))))

(defun definitions-package-definitions (definitions package)
  "Return the subset of DEFINITIONS that belong to PACKAGE."
  (mapcan-definitions-pool
   (lambda (definition) (definition-package-definitions definition package))
   definitions))

;;; package.lisp ends here
