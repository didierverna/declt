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

(defun package-symbols (package &aux symbols)
  "Return the list of symbols from home PACKAGE."
  (do-symbols (symbol package symbols)
    (when (eq (symbol-package symbol) package)
      (push symbol symbols))))



;; ==========================================================================
;; Package Definitions
;; ==========================================================================

(defclass package-definition (definition)
  ((object :initarg :package :reader definition-package) ;; slot overload
   (use-definitions
    :documentation "The corresponding use-list, as package definitions."
    :accessor use-definitions)
   (used-by-definitions
    :documentation "The corresponding used-by-list, as package definitions."
    :accessor used-by-definitions)
   (symbol-definitions
    :documentation "The corresponding symbol definitions."
    :accessor symbol-definitions))
  (:documentation "The Package Definition class."))

(defun make-package-definition (package &optional foreign)
  "Make a new PACKAGE definition, possibly FOREIGN."
  (make-instance 'package-definition :package package :foreign foreign))


;; ----------------
;; Pseudo-accessors
;; ----------------

(defmethod name ((definition package-definition))
  "Return package DEFINITION's package name."
  (package-name (definition-package definition)))

(defun nicknames (package-definition)
  "Return the list of nicknames for PACKAGE-DEFINITION."
  (package-nicknames (definition-package package-definition)))

(defmethod external-definitions
  ((package-definition package-definition)
   &aux (external-symbols
	 (package-external-symbols (definition-package package-definition))))
  "Return PACKAGE-DEFINITION's external definitions."
  (remove-if-not (lambda (symbol) (member symbol external-symbols))
      (symbol-definitions package-definition)
    :key #'definition-symbol))

(defmethod internal-definitions
  ((package-definition package-definition)
   &aux (internal-symbols
	 (package-internal-symbols (definition-package package-definition))))
  "Return PACKAGE-DEFINITION's internal definitions."
  (remove-if-not (lambda (symbol) (member symbol internal-symbols))
      (symbol-definitions package-definition)
    :key #'definition-symbol))



;; ==========================================================================
;; Extraction Protocols
;; ==========================================================================

;; ---------------
;; Source protocol
;; ---------------

;; #### PORTME.
(defmethod source ((package package))
  "Return PACKAGE'source."
  (when-let (defsrc (sb-introspect:find-definition-source package))
    (sb-introspect:definition-source-pathname defsrc)))

(defmethod source ((package-definition package-definition))
  "Return PACKAGE-DEFINITION'source."
  (source (definition-package package-definition)))


;; ------------------
;; Docstring protocol
;; ------------------

(defmethod docstring ((package-definition package-definition))
  "Return PACKAGE-DEFINITION's docstring."
  (documentation (definition-package package-definition) t))



;; ==========================================================================
;; Finalization
;; ==========================================================================

;; #### FIXME: this is wrong in at least one corner case: for aggregates which
;; #### names and slot names come from different packages.

;; #### NOTE: contrary to DEFINITION-FILE-DEFINITIONS, this function could be
;; optimized a bit. For instance, when we figure out that a generic function
;; does belong to a package, we know that all methods do to because they share
;; the same name. This means that we could save some package name comparison
;; tests.
#+()(defgeneric definition-package-definitions (definition package)
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
  (mapcan (lambda (definition)
	    (definition-package-definitions definition package))
    definitions))

;;; package.lisp ends here
