;;; symbol.lisp --- Symbol based documentation

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


;; ==========================================================================
;; Documentation Protocols
;; ==========================================================================

(defmethod title ((constant constant-definition) &optional relative-to)
  "Return CONSTANT's title."
  (declare (ignore relative-to))
  (format nil "The ~(~A~) constant" (name constant)))

(defmethod title ((special special-definition) &optional relative-to)
  "Return SPECIAL's title."
  (declare (ignore relative-to))
  (format nil "The ~(~A~) special variable" (name special)))

(defmethod title ((macro macro-definition) &optional relative-to)
  "Return MACRO's title."
  (declare (ignore relative-to))
  (format nil "The ~(~A~) macro" (name macro)))

;; #### NOTE: applies to setters as well
(defmethod title ((function function-definition) &optional relative-to)
  "Return FUNCTION's title."
  (declare (ignore relative-to))
  (format nil "The ~(~A~) function" (name function)))

;; #### NOTE: applies to setter methods as well
(defmethod title ((method method-definition) &optional relative-to)
  "Return METHOD's title."
  (declare (ignore relative-to))
  (format nil "The ~(~A~{ ~A~^~}~{ ~A~^~}~) method"
    (name method)
    ;; #### PORTME.
    (mapcar #'pretty-specializer
	    (sb-mop:method-specializers (method-definition-method method)))
    (sb-mop:method-qualifiers (method-definition-method method))))

;; #### NOTE: applies to generic setters as well
(defmethod title ((generic generic-definition) &optional relative-to)
  "Return GENERIC's title."
  (declare (ignore relative-to))
  (format nil "The ~(~A~) generic function" (name generic)))

(defmethod title ((condition condition-definition) &optional relative-to)
  "Return CONDITION's title."
  (declare (ignore relative-to))
  (format nil "The ~(~A~) condition" (name condition)))

(defmethod title ((structure structure-definition) &optional relative-to)
  "Return STRUCTURE's title."
  (declare (ignore relative-to))
  (format nil "The ~(~A~) structure" (name structure)))

(defmethod title ((class class-definition) &optional relative-to)
  "Return CLASS's title."
  (declare (ignore relative-to))
  (format nil "The ~(~A~) class" (name class)))

;; #### NOTE: the INDEX methods below only perform sub-indexing because the
;; main index entries are created automatically in Texinfo by the @defXXX
;; routines.

(defmethod index ((constant constant-definition) &optional relative-to)
  "Render CONSTANT's indexing command."
  (declare (ignore relative-to))
  (format t "@constantsubindex{~(~A~)}@c~%" (escape constant)))

(defmethod index ((special special-definition) &optional relative-to)
  "Render SPECIAL's indexing command."
  (declare (ignore relative-to))
  (format t "@specialsubindex{~(~A~)}@c~%" (escape special)))

(defmethod index ((macro macro-definition) &optional relative-to)
  "Render MACRO's indexing command."
  (declare (ignore relative-to))
  (format t "@macrosubindex{~(~A~)}@c~%" (escape macro)))

;; #### NOTE: applies to setters as well
(defmethod index ((function function-definition) &optional relative-to)
  "Render FUNCTION's indexing command."
  (declare (ignore relative-to))
  (format t "@functionsubindex{~(~A~)}@c~%" (escape function)))

;; #### NOTE: applies to setter methods as well
(defmethod index ((method method-definition) &optional relative-to)
  "Render METHOD's indexing command."
  (declare (ignore relative-to))
  (format t "@methodsubindex{~(~A~)}@c~%" (escape method)))

;; #### NOTE: applies to generic setters as well
(defmethod index ((generic generic-definition) &optional relative-to)
  "Render GENERIC's indexing command."
  (declare (ignore relative-to))
  (format t "@genericsubindex{~(~A~)}@c~%" (escape generic)))

(defmethod index ((condition condition-definition) &optional relative-to)
  "Render CONDITION's indexing command."
  (declare (ignore relative-to))
  (format t "@conditionsubindex{~(~A~)}@c~%" (escape condition)))

(defmethod index ((structure structure-definition) &optional relative-to)
  "Render STRUCTURE's indexing command."
  (declare (ignore relative-to))
  (format t "@structuresubindex{~(~A~)}@c~%" (escape structure)))

(defmethod index ((class class-definition) &optional relative-to)
  "Render CLASS's indexing command."
  (declare (ignore relative-to))
  (format t "@classsubindex{~(~A~)}@c~%" (escape class)))

(defmethod reference ((definition definition) &optional relative-to)
  "Render DEFINITION's reference."
  (declare (ignore relative-to))
  (format t "@ref{~A, , @t{~(~A}~)} (~A)~%"
    (escape (anchor-name definition))
    (escape definition)
    (type-name definition)))

(defun document-definition
    (definition system kind &key (name (definition-symbol definition)))
  "Render SYSTEM's DEFINITION's documentation contents as KIND."
  (anchor definition)
  (index definition)
  (@table ()
    (let ((documentation (documentation name kind)))
      (when documentation
	(@tableitem "Documentation"
	  (render-text documentation))))
    (@tableitem "Package"
      (reference (symbol-package (definition-symbol definition))))
    (render-source definition system)))

(defmethod document ((constant constant-definition) system &key)
  "Render SYSTEM's CONSTANT's documentation."
  (@defconstant (string-downcase (definition-symbol constant))
    (document-definition constant system 'variable)))

(defmethod document ((special special-definition) system &key)
  "Render SYSTEM's SPECIAL's documentation."
  (@defspecial (string-downcase (definition-symbol special))
    (document-definition special system 'variable)))

(defmethod document ((macro macro-definition) system &key)
  "Render SYSTEM's MACRO's documentation."
  (@defmac (string-downcase (definition-symbol macro))
      ;; #### PORTME.
      (sb-introspect:function-lambda-list
       (macro-definition-function macro))
    (document-definition macro system 'function)))

(defmethod document ((function function-definition) system
		     &key (name (function-definition-symbol function)))
  "Render SYSTEM's FUNCTION's documentation."
  (@defun (format nil "~(~A~)" name)
      ;; #### PORTME.
      (sb-introspect:function-lambda-list
       (function-definition-function function))
    (document-definition function system 'function :name name)))

(defmethod document
    ((setter setter-definition) system
     &key
     &aux (setter-name `(setf ,(setter-definition-symbol setter))))
  "Render SYSTEM's SETTER function's documentation."
  (call-next-method setter system :name setter-name))

(defmethod document ((method method-definition) system
		     &key (name (definition-symbol method)))
  "Render SYSTEM's METHOD's documentation."
  (@defmethod
      ;; #### PORTME:
      (format nil "~(~A~)" name)
      (sb-mop:method-lambda-list (method-definition-method method))
      (sb-mop:method-specializers (method-definition-method method))
      (sb-mop:method-qualifiers (method-definition-method method))
    (@table ()
      (anchor method)
      (index method)
      (let ((documentation
	     (documentation (method-definition-method method) t)))
	(when documentation
	  (@tableitem "Documentation"
	    (render-text documentation))))
      (render-source method system))))

(defmethod document ((generic generic-definition) system
		     &key (name (generic-definition-symbol generic)))
  "Render SYSTEM's GENERIC function's documentation."
  (@defgeneric (format nil "~(~A~)" name)
      ;; #### PORTME.
      (sb-introspect:function-lambda-list
       (generic-definition-function generic))
    (document-definition generic system 'function :name name))
  (dolist (method (generic-definition-methods generic))
    (document method system :name name)))

(defmethod document
    ((generic-setter generic-setter-definition) system
     &key
     &aux (generic-setter-name
	   `(setf ,(generic-setter-definition-symbol generic-setter))))
  "Render SYSTEM's GENERIC-SETTER function's documentation."
  (call-next-method generic-setter system :name generic-setter-name))

(defmethod document ((condition condition-definition) system &key)
  "Render SYSTEM's CONDITION's documentation."
  (@defcond (string-downcase (definition-symbol condition))
    (document-definition condition system 'type)))

(defmethod document ((structure structure-definition) system &key)
  "Render SYSTEM's STRUCTURE's documentation."
  (@defstruct (string-downcase (definition-symbol structure))
    (document-definition structure system 'type)))

(defmethod document ((class class-definition) system &key)
  "Render SYSTEM's CLASS's documentation."
  (@defclass (string-downcase (definition-symbol class))
    (document-definition class system 'type)))



;; ==========================================================================
;; Definition Nodes
;; ==========================================================================

(defun add-category-node (system parent location category definitions)
  "Add SYSTEM's LOCATION CATEGORY node to PARENT for DEFINITIONS."
  (add-child parent
    (make-node :name (format nil "~@(~A ~A~)" location category)
	       :section-name (format nil "~@(~A~)" category)
	       :before-menu-contents
	       (render-to-string
		 (dolist (definition (sort definitions #'string-lessp
					   :key #'definition-symbol))
		   (document definition system))))))

(defun add-categories-node (system parent location symbols)
  "Add SYSTEM's category nodes to PARENT for LOCATION SYMBOLS."
  (dolist (category +categories+)
    (case (first category)
      ;; #### NOTE: we use a rather ugly hack below to avoid explicit Setters
      ;; and Generic Setters sections in the documentation. We prefer to group
      ;; setters and getter together when that it possible.
      (:function
       (let ((category-definitions
	      (loop :for symbol :in symbols
		    :when (symbol-definition symbol :function)
		    :collect :it
		    :when (symbol-definition symbol :setter)
		    :collect :it)))
	 (when category-definitions
	   (add-category-node system parent location (second category)
			      category-definitions))))
      (:generic
       (let ((category-definitions
	      (loop :for symbol :in symbols
		    :when (symbol-definition symbol :generic)
		    :collect :it
		    :when (symbol-definition symbol :generic-setter)
		    :collect :it)))
	 (when category-definitions
	   (add-category-node system parent location (second category)
			      category-definitions))))
      ((:setter :generic-setter))
      (otherwise
       (let ((category-definitions
	      (loop :for symbol :in symbols
		    :when (symbol-definition symbol (first category))
		    :collect :it)))
	 (when category-definitions
	   (add-category-node system parent location (second category)
			      category-definitions)))))))

(defun add-definitions-node
    (parent system
     &aux (definitions-node
	      (add-child parent
		(make-node :name "Definitions"
			   :synopsis "The symbols documentation"
			   :before-menu-contents(format nil "~
Definitions are sorted by export status, category, package, and then by
lexicographic order.")))))
  "Add SYSTEM's definitions node to PARENT."
  (loop :for symbols :in (list (system-external-symbols system)
			       (system-internal-symbols system))
	:for status :in '("exported" "internal")
	:when symbols
	:do (let ((node (add-child definitions-node
			  (make-node :name (format nil "~@(~A~) definitions"
					     status)))))
	      (add-categories-node system node status symbols))))


;;; symbol.lisp ends here
