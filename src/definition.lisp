;;; definition.lisp --- Definitions rendering

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Sat Sep  4 15:27:31 2010
;; Last Revision: Sun Sep  5 21:49:36 2010

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
;; Rendering routines
;; ==========================================================================

(defun render-documentation (symbol type)
  "Render SYMBOL's TYPE documentation."
  (when (symbolp symbol)
    (format t "@table @strong~%")
    (format t "@item Package~%")
    (reference (symbol-package symbol))
    (format t "@end table~%"))
  (render-string (documentation symbol type)))

(defun render-method (method)
  "Render METHOD."
  (@defmethod
      ;; #### PORTME:
      (string-downcase (sb-mop:generic-function-name
			(sb-mop:method-generic-function method)))
      ;; #### PORTME.
      (sb-mop:method-lambda-list method)
      ;; #### PORTME.
      (sb-mop:method-specializers method)
      ;; #### PORTME.
      (sb-mop:method-qualifiers method)
    (render-documentation method 't)))

(defgeneric render-symbol (symbol kind)
  (:documentation "Render SYMBOL as a KIND.")
  (:method (symbol (kind (eql :constant)))
    "Render SYMBOL as a constant."
    (when (constant-definition-p symbol)
      (@defconstant (string-downcase symbol)
	(render-documentation symbol 'variable))))
  (:method (symbol (kind (eql :special)))
    "Render SYMBOL as a special variable."
    (when (special-definition-p symbol)
      (@defspecial (string-downcase symbol)
	(render-documentation symbol 'variable))))
  (:method (symbol (kind (eql :macro)))
    "Render SYMBOL as a macro."
    (when (macro-definition-p symbol)
      (@defmac (string-downcase symbol)
	  ;; #### PORTME.
	  (sb-introspect:function-lambda-list symbol)
	(render-documentation symbol 'function))))
  (:method (symbol (kind (eql :function)))
    "Render SYMBOL as an ordinary function."
    (when (function-definition-p symbol)
      (@defun (string-downcase symbol)
	  ;; #### PORTME.
	  (sb-introspect:function-lambda-list symbol))
      (render-documentation symbol 'function)))
  (:method (symbol (kind (eql :generic)))
    "Render SYMBOL as a generic function."
    (when (generic-definition-p symbol)
      (@defgeneric (string-downcase symbol)
	  ;; #### PORTME.
	  (sb-introspect:function-lambda-list symbol)
	(render-documentation symbol 'function)))
    ;; #### PORTME.
    (dolist (method (sb-mop:generic-function-methods (fdefinition symbol)))
      (render-method method)))
  (:method (symbol (kind (eql :condition)))
    "Render SYMBOL as a condition."
    (when (condition-definition-p symbol)
      (@defcond (string-downcase symbol)
	(render-documentation symbol 'type))))
  (:method (symbol (kind (eql :structure)))
    "Render SYMBOL as a structure."
    (when (structure-definition-p symbol)
      (@defstruct (string-downcase symbol)
	(render-documentation symbol 'type))))
  (:method (symbol (kind (eql :class)))
    "Render SYMBOL as an ordinary class."
    (when (class-definition-p symbol)
      (@defclass (string-downcase symbol)
	(render-documentation symbol 'type)))))



;; ==========================================================================
;; Definition nodes
;; ==========================================================================

(defun add-category-node (parent location category symbols)
  "Add LOCATION CATEGORY node to PARENT for SYMBOLS."
  (add-child parent
    (make-node :name (format nil "~@(~A ~A~)" location (third category))
	       :section-name (format nil "~@(~A~)" (third category))
	       :before-menu-contents
	       (render-to-string
		 (dolist (symbol (sort symbols #'string-lessp))
		   (render-symbol symbol (first category)))))))

(defun add-categories-node (parent location symbols)
  "Add all relevant category nodes to PARENT for LOCATION SYMBOLS."
  (dolist (category +categories+)
    (let ((category-symbols
	   (remove-if-not (lambda (symbol)
			    (definitionp symbol (first category)))
			  symbols)))
      (when category-symbols
	(add-category-node parent location category category-symbols)))))

(defun add-definitions-node
    (parent system
     &aux (definitions-node
	      (add-child parent
		(make-node :name "Definitions"
			   :synopsis "The symbols documentation"
			   :before-menu-contents(format nil "~
Definitions are sorted by export status, category, package, and then by
lexicographic order.")))))
  "Add the SYSTEM's definitions node to PARENT."
  (loop :for symbols :in (list (system-external-symbols system)
			       (system-internal-symbols system))
	:for location :in '("exported" "internal")
	:when symbols
	:do (let ((node (add-child definitions-node
			  (make-node :name (format nil "~@(~A~) definitions"
					     location)))))
	      (add-categories-node node location symbols))))


;;; definition.lisp ends here
