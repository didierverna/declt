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

(defmethod index ((symbol symbol) &optional relative-to)
  (declare (ignore relative-to))
  (values))

(defmethod document ((symbol symbol) &optional relative-to category)
  "Render SYMBOL's CATEGORY documentation."
  (declare (ignore relative-to))
  (@table ()
    (format t "@item Package~%")
    (reference (symbol-package symbol))
    (format t "@item Documentation~%")
    (render-string (documentation symbol (ecase category
					   ((:constant :special)
					    'variable)
					   ((:macro :function :generic)
					    'function)
					   ((:condition :structure :class)
					    'type))))))

(defmethod index ((method method) &optional relative-to)
  (declare (ignore relative-to))
  (values))

(defmethod document ((method method) &optional relative-to category)
  (declare (ignore relative-to category))
  (render-string (documentation method t)))

;; #### PORTME:
(defun render-method (method relative-to)
  (@defmethod
      (string-downcase (sb-mop:generic-function-name
			(sb-mop:method-generic-function method)))
      (sb-mop:method-lambda-list method)
      (sb-mop:method-specializers method)
      (sb-mop:method-qualifiers method)
    (document method relative-to)))

(defgeneric render-symbol (symbol relative-to category)
  (:documentation "Render SYMBOL as a CATEGORY.")
  (:method (symbol relative-to (category (eql :constant)))
    "Render SYMBOL as a constant."
    (when (definitionp symbol :constant)
      (@defconstant (string-downcase symbol)
	(document symbol relative-to category))))
  (:method (symbol relative-to (category (eql :special)))
    "Render SYMBOL as a special variable."
    (when (definitionp symbol :special)
      (@defspecial (string-downcase symbol)
	(document symbol relative-to category))))
  (:method (symbol relative-to (category (eql :macro)))
    "Render SYMBOL as a macro."
    (when (definitionp symbol :macro)
      (@defmac (string-downcase symbol)
	  ;; #### PORTME.
	  (sb-introspect:function-lambda-list symbol)
	(document symbol relative-to category))))
  (:method (symbol relative-to (category (eql :function)))
    "Render SYMBOL as an ordinary function."
    (when (definitionp symbol :function)
      (@defun (string-downcase symbol)
	  ;; #### PORTME.
	  (sb-introspect:function-lambda-list symbol))
      (document symbol relative-to category)))
  (:method (symbol relative-to (category (eql :generic)))
    "Render SYMBOL as a generic function."
    (when (definitionp symbol :generic)
      (@defgeneric (string-downcase symbol)
	  ;; #### PORTME.
	  (sb-introspect:function-lambda-list symbol)
	(document symbol relative-to category)))
    ;; #### PORTME.
    (dolist (method (sb-mop:generic-function-methods (fdefinition symbol)))
      (render-method method relative-to)))
  (:method (symbol relative-to (category (eql :condition)))
    "Render SYMBOL as a condition."
    (when (definitionp symbol :condition)
      (@defcond (string-downcase symbol)
	(document symbol relative-to category))))
  (:method (symbol relative-to (category (eql :structure)))
    "Render SYMBOL as a structure."
    (when (definitionp symbol :structure)
      (@defstruct (string-downcase symbol)
	(document symbol relative-to category))))
  (:method (symbol relative-to (category (eql :class)))
    "Render SYMBOL as an ordinary class."
    (when (definitionp symbol :class)
      (@defclass (string-downcase symbol)
	(document symbol relative-to category)))))



;; ==========================================================================
;; Definition nodes
;; ==========================================================================

(defun add-category-node (parent location category symbols relative-to)
  "Add LOCATION CATEGORY node to PARENT for SYMBOLS."
  (add-child parent
    (make-node :name (format nil "~@(~A ~A~)" location (third category))
	       :section-name (format nil "~@(~A~)" (third category))
	       :before-menu-contents
	       (render-to-string
		 (dolist (symbol (sort symbols #'string-lessp))
		   (render-symbol symbol relative-to (first category)))))))

(defun add-categories-node (parent location symbols relative-to)
  "Add all relevant category nodes to PARENT for LOCATION SYMBOLS."
  (dolist (category +categories+)
    (let ((category-symbols
	   (remove-if-not (lambda (symbol)
			    (definitionp symbol (first category)))
			  symbols)))
      (when category-symbols
	(add-category-node parent location category category-symbols
			   relative-to)))))

(defun add-definitions-node
    (parent system
     &aux (system-directory (system-directory system))
	  (definitions-node
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
	      (add-categories-node node location symbols system-directory))))


;;; definition.lisp ends here
