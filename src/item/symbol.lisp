;;; symbol.lisp --- Symbol based items

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Thu Sep  9 11:31:55 2010
;; Last Revision: Thu Sep  9 11:53:54 2010

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


;; ==========================================================================
;; Rendering protocols
;; ==========================================================================

(defmethod to-string ((symbol symbol))
  "Return SYMBOL's name."
  (symbol-name symbol))



;; ==========================================================================
;; Item Protocols
;; ==========================================================================

(defmethod location ((method method))
  ;; #### PORTME.
  (let* ((defsrc (sb-introspect:find-definition-source method)))
    (when defsrc
      (sb-introspect:definition-source-pathname defsrc))))



;; ==========================================================================
;; Utilities
;; ==========================================================================

;; #### PORTME.
(define-constant +categories+
    '((:constant  "constant"          "constants")
      (:special   "special variable"  "special variables")
      (:macro     "macro"             "macros")
      (:function  "function"          "functions")
      (:generic   "generic function"  "generic functions")
      (:condition "condition"         "conditions")
      (:structure "structure"         "structures")
      (:class     "class"             "classes"))
  "The list of definition categories.")

(defun definitionp (symbol kind)
  "Return a value of some KIND defined by SYMBOL if any."
  (ecase kind
    (:constant
     (when (eql (sb-int:info :variable :kind symbol) :constant)
       (symbol-value symbol)))
    (:special
     (when (eql (sb-int:info :variable :kind symbol) :special)
       (symbol-value symbol)))
    (:macro
     (macro-function symbol))
    (:function
     (when (and (fboundp symbol)
		(not (definitionp symbol :macro))
		(not (definitionp symbol :generic)))
       (fdefinition symbol)))
    (:generic
     (when (and (fboundp symbol)
		(typep (fdefinition symbol) 'generic-function))
       (fdefinition symbol)))
    (:condition
     (let ((class (find-class symbol nil)))
       (when (and class
		  (typep class 'condition))
	 class)))
    (:structure
     (let ((class (find-class symbol nil)))
       (when (and class
		  (eq (class-of class) 'structure-class))
	 class)))
    (:class
     (let ((class (find-class symbol nil)))
       (when (and class
		  (not (definitionp symbol :condition))
		  (not (definitionp symbol :structure)))
	 class)))))

(defun symbol-needs-documenting (symbol)
  "Return t when SYMBOL needs to be documented."
  (some (lambda (category)
	  (definitionp symbol (first category)))
	+categories+))


;;; symbol.lisp ends here
