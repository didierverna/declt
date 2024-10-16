;;; package.lisp --- Package definitions

;; Copyright (C) 2010-2013, 2017, 2020-2022 Didier Verna

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

(in-package :net.didierverna.declt.assess)
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
   (use-list
    :documentation "The definitions use-list for this definition's package."
    :accessor use-list)
   (used-by-list
    :documentation "The definitions used-by-list for this definition's package."
    :accessor used-by-list)
   (definitions
    :documentation "The list of corresponding definitions."
    :accessor definitions))
  (:documentation "The class of package definitions.
All package definitions respond to the following public protocols, which
see:
- `nicknames'."))

(defun package-definition-p (definition)
  "Return T if DEFINITION is a package definition."
  (typep definition 'package-definition))

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

;; #### NOTE: even foreign packages get their lists of external and internal
;; symbols computed below. I'm not completely sure it's required right now,
;; but it may turn out to be necessary one day for printing foreign symbols in
;; the form <foreign package>:[:]<symbol-name>.
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
;; Public Definition Protocols
;; ==========================================================================

(defmethod name ((definition package-definition))
  "Return package DEFINITION's package name."
  (package-name (definition-package definition)))

(defun nicknames (package-definition)
  "Return the list of nicknames for PACKAGE-DEFINITION."
  (package-nicknames (definition-package package-definition)))




;; ==========================================================================
;; Public Utility Protocols
;; ==========================================================================

;; #### NOTE: here, we can be a little faster than the default method because
;; packages only contain symbol definitions.

(defmethod public-definitions ((definition package-definition))
  "Return package DEFINITION's public definitions."
  (remove-if-not #'publicp (definitions definition)))

(defmethod private-definitions ((definition package-definition))
  "Return package DEFINITION's private definitions."
  (remove-if #'publicp (definitions definition)))

;;; package.lisp ends here
