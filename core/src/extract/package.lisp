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
;; Package Definition Basics
;; ==========================================================================

(defclass package-definition (definition)
  ((object :initarg :package :reader definition-package) ;; slot overload
   (external-symbols
    :documentation "The list of corresponding external symbols."
    :accessor external-symbols)
   (internal-symbols
    :documentation "The list of corresponding internal symbols."
    :accessor internal-symbols)
   (use-definitions
    :documentation "The list of corresponding use list package definitions."
    :accessor use-definitions)
   (used-by-definitions
    :documentation "The list of corresponding used-by list package definitions."
    :accessor used-by-definitions)
   (definitions
    :documentation "The list of corresponding definitions."
    :accessor definitions))
  (:documentation "The class of package definitions."))

(defun package-external-symbols (package &aux external-symbols)
  "Return the list of PACKAGE's external symbols."
  (do-external-symbols (symbol package external-symbols)
    (when (eq (symbol-package symbol) package)
      (push symbol external-symbols))))

;; #### WARNING: evil inside. Since we need to compute the package's external
;; symbols list anyway, we might as well return it as a second value. If you
;; use both, as below, the name is misleading though.
(defun package-internal-symbols
    (package &aux (external-symbols (package-external-symbols package))
		  internal-symbols)
  "Return the lists of PACKAGE's internal and external symbols as two values."
  (do-symbols (symbol package (values internal-symbols external-symbols))
    (when (and (eq (symbol-package symbol) package)
	       (not (member symbol external-symbols)))
      (push symbol internal-symbols))))

(defmethod initialize-instance :after ((definition package-definition) &key)
  "Compute DEFINITION's package lists of external and internal symbols."
  (multiple-value-bind (internals externals)
      (package-internal-symbols (definition-package definition))
    (setf (external-symbols definition) externals)
    (setf (internal-symbols definition) internals)))

(defun make-package-definition (package &optional foreign)
  "Make a new PACKAGE definition, possibly FOREIGN."
  (make-instance 'package-definition :package package :foreign foreign))




;; ==========================================================================
;; Extraction Protocols
;; ==========================================================================

(defmethod name ((definition package-definition))
  "Return package DEFINITION's package name."
  (package-name (definition-package definition)))

(defun nicknames (package-definition)
  "Return the list of nicknames for PACKAGE-DEFINITION."
  (package-nicknames (definition-package package-definition)))



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

#+()(defun definitions-package-definitions (definitions package)
  "Return the subset of DEFINITIONS that belong to PACKAGE."
  (mapcan (lambda (definition)
	    (definition-package-definitions definition package))
    definitions))




;; ==========================================================================
;; Utilities
;; ==========================================================================

(defmethod public-definitions ((definition package-definition))
  "Return package DEFINITION's public definitions."
  (remove-if-not #'publicp (definitions definition)))

(defmethod private-definitions ((definition package-definition))
  "Return package DEFINITION's private definitions."
  (remove-if #'publicp (definitions definition)))

;;; package.lisp ends here
