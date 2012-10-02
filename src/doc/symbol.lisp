;;; symbol.lisp --- Symbol based documentation

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
;; Documentation Protocols
;; ==========================================================================

(defmethod title ((constant constant-definition) &optional relative-to)
  "Return CONSTANT's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~) constant" (name constant)))

(defmethod title ((special special-definition) &optional relative-to)
  "Return SPECIAL's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~) special variable" (name special)))

(defmethod title ((macro macro-definition) &optional relative-to)
  "Return MACRO's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~) macro" (name macro)))

(defmethod title ((function function-definition) &optional relative-to)
  "Return FUNCTION's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~) function" (name function)))

(defmethod title ((method method-definition) &optional relative-to)
  "Return METHOD's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~{ ~A~^~}~{ ~A~^~}~) method"
    (name method)
    ;; #### PORTME.
    (mapcar #'pretty-specializer
	    (sb-mop:method-specializers (method-definition-method method)))
    (sb-mop:method-qualifiers (method-definition-method method))))

(defmethod title ((generic generic-definition) &optional relative-to)
  "Return GENERIC's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~) generic function" (name generic)))

(defmethod title ((condition condition-definition) &optional relative-to)
  "Return CONDITION's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~) condition" (name condition)))

(defmethod title ((structure structure-definition) &optional relative-to)
  "Return STRUCTURE's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~) structure" (name structure)))

(defmethod title ((class class-definition) &optional relative-to)
  "Return CLASS's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~) class" (name class)))

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

(defmethod index ((function function-definition) &optional relative-to)
  "Render FUNCTION's indexing command."
  (declare (ignore relative-to))
  (format t "@functionsubindex{~(~A~)}@c~%" (escape function)))

(defmethod index ((method method-definition) &optional relative-to)
  "Render METHOD's indexing command."
  (declare (ignore relative-to))
  (format t "@methodsubindex{~(~A~)}@c~%" (escape method)))

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
    (definition context kind &key (name (definition-symbol definition)))
  "Render DEFINITION's documentation contents as KIND in CONTEXT."
  (anchor definition)
  (index definition)
  (@table ()
    (let ((documentation (documentation name kind)))
      (when documentation
	(@tableitem "Documentation"
	  (render-text documentation))))
    (@tableitem "Package"
      (reference (symbol-package (definition-symbol definition))))
    (render-source definition context)))

(defmethod document ((constant constant-definition) context &key)
  "Render CONSTANT's documentation in CONTEXT."
  (@defconstant (string-downcase (definition-symbol constant))
    (document-definition constant context 'variable)))

(defmethod document ((special special-definition) context &key)
  "Render SPECIAL variable's documentation in CONTEXT."
  (@defspecial (string-downcase (definition-symbol special))
    (document-definition special context 'variable)))

;; #### PORTME.
(defmethod document ((macro macro-definition) context &key)
  "Render MACRO's documentation in CONTEXT."
  (@defmac (string-downcase (definition-symbol macro))
      (sb-introspect:function-lambda-list
       (macro-definition-function macro))
    (document-definition macro context 'function)))

;; #### PORTME.
(defmethod document ((function function-definition) context
		     &key (name (function-definition-symbol function)))
  "Render FUNCTION's documentation in CONTEXT."
  (@defun (format nil "~(~A~)" name)
      (sb-introspect:function-lambda-list
       (function-definition-function function))
    (document-definition function context 'function :name name)))

(defmethod document ((writer writer-definition) context &key)
  "Render WRITER's documentation in CONTEXT."
  (call-next-method writer context
		    :name `(setf ,(writer-definition-symbol writer))))

;; #### PORTME.
(defmethod document ((accessor accessor-definition) context &key)
  "Render ACCESSOR's documentation in CONTEXT."
  (cond ((and (equal (source accessor)
		     (source (accessor-definition-writer accessor)))
	      (equal (sb-introspect:function-lambda-list
		      (accessor-definition-function accessor))
		     ;; #### NOTE: the writer has a first additional
		     ;; lambda-list argument of NEW-VALUE that we must skip
		     ;; before comparing.
		     (cdr (sb-introspect:function-lambda-list
			   (writer-definition-function
			    (accessor-definition-writer accessor)))))
	      (let ((function-doc (documentation
				   (accessor-definition-symbol accessor)
				   'function))
		    (writer-doc (documentation
				 (writer-definition-symbol
				  (accessor-definition-writer accessor))
				 'function)))
		(or (and function-doc writer-doc
			 (string= function-doc writer-doc))
		    (null writer-doc)
		    (and (not (null writer-doc)) (null function-doc)))))
	 (@defun (format nil  "~(~A~)" (accessor-definition-symbol accessor))
	     (sb-introspect:function-lambda-list
	      (accessor-definition-function accessor))
	   (@defunx (format nil  "~(~A~)"
		      `(setf ,(writer-definition-symbol
			       (accessor-definition-writer accessor))))
		    (sb-introspect:function-lambda-list
		     (writer-definition-function
		      (accessor-definition-writer accessor))))
	   (anchor (accessor-definition-writer accessor))
	   (index (accessor-definition-writer accessor))
	   (document-definition accessor context 'function)))
	(t
	 (call-next-method)
	 (document (accessor-definition-writer accessor) context))))

;; #### PORTME:
(defmethod document ((method method-definition) context
		     &key (name (definition-symbol method)))
  "Render METHOD's documentation in CONTEXT."
  (@defmethod
      (format nil "~(~A~)" name)
      (sb-mop:method-lambda-list (method-definition-method method))
      (sb-mop:method-specializers (method-definition-method method))
      (sb-mop:method-qualifiers (method-definition-method method))
    (anchor method)
    (index method)
    (@table ()
      (let ((documentation
	      (documentation (method-definition-method method) t)))
	(when documentation
	  (@tableitem "Documentation"
	    (render-text documentation))))
      (render-source method context))))

(defmethod document ((method writer-method-definition) context &key)
  "Render writer METHOD's documentation in CONTEXT."
  (call-next-method method context
		    :name `(setf ,(writer-method-definition-symbol method))))

;; #### PORTME.
(defmethod document ((method accessor-method-definition) context &key)
  "Render accessor METHOD's documentation in CONTEXT."
  (cond ((and (equal (source method)
		     (source (accessor-method-definition-writer method)))
	      (equal (sb-mop:method-lambda-list
		      (accessor-method-definition-method method))
		     ;; #### NOTE: the writer has a first additional
		     ;; lambda-list argument of NEW-VALUE that we must skip
		     ;; before comparing.
		     (cdr (sb-mop:method-lambda-list
			   (writer-method-definition-method
			    (accessor-method-definition-writer method)))))
	      (let ((method-doc (documentation
				 (accessor-method-definition-method method)
				 t))
		    (writer-doc (documentation
				 (writer-method-definition-method
				  (accessor-method-definition-writer method))
				 t)))
		(or (and method-doc writer-doc
			 (string= method-doc writer-doc))
		    (null writer-doc)
		    (and (not (null writer-doc)) (null method-doc)))))
	 (@defmethod
	     (format nil  "~(~A~)"
	       (accessor-method-definition-symbol method))
	     (sb-mop:method-lambda-list
	      (accessor-method-definition-method method))
	     (sb-mop:method-specializers
	      (accessor-method-definition-method method))
	     (sb-mop:method-qualifiers
	      (accessor-method-definition-method method))
	   (@defmethodx (format nil  "~(~A~)"
			  `(setf ,(writer-method-definition-symbol
				   (accessor-method-definition-writer
				    method))))
			(sb-mop:method-lambda-list
			 (writer-method-definition-method
			  (accessor-method-definition-writer method)))
			(sb-mop:method-specializers
			 (writer-method-definition-method
			  (accessor-method-definition-writer method)))
			(sb-mop:method-qualifiers
			 (writer-method-definition-method
			  (accessor-method-definition-writer method))))
	   (anchor (accessor-method-definition-writer method))
	   (index (accessor-method-definition-writer method))
	   (anchor method)
	   (index method)
	   (@table ()
	     (let ((documentation
		     (documentation (accessor-method-definition-method method)
				    t)))
	       (when documentation
		 (@tableitem "Documentation"
		   (render-text documentation))))
	     (render-source method context))))
	(t
	 (call-next-method)
	 (document (accessor-method-definition-writer method) context))))

;; #### PORTME.
(defmethod document ((generic generic-definition) context
		     &key (name (generic-definition-symbol generic)))
  "Render GENERIC's documentation in CONTEXT."
  (@defgeneric (format nil "~(~A~)" name)
      (sb-introspect:function-lambda-list
       (generic-definition-function generic))
    (document-definition generic context 'function :name name))
  (dolist (method (generic-definition-methods generic))
    (document method context)))

(defmethod document ((writer generic-writer-definition) context &key)
  "Render generic WRITER's documentation in CONTEXT."
  (call-next-method writer context
		    :name `(setf ,(generic-writer-definition-symbol writer))))

;; #### PORTME.
(defmethod document ((accessor generic-accessor-definition) context &key)
  "Render generic ACCESSOR's documentation in CONTEXT."
  (cond ((and (equal (source accessor)
		     (source (generic-accessor-definition-writer accessor)))
	      (equal (sb-introspect:function-lambda-list
		      (generic-accessor-definition-function accessor))
		     ;; #### NOTE: the writer has a first additional
		     ;; lambda-list argument of NEW-VALUE that we must skip
		     ;; before comparing.
		     (cdr (sb-introspect:function-lambda-list
			   (generic-writer-definition-function
			    (generic-accessor-definition-writer accessor)))))
	      (let ((function-doc
		      (documentation
		       (generic-accessor-definition-symbol accessor)
		       'function))
		    (writer-doc
		      (documentation
		       (generic-writer-definition-symbol
			(generic-accessor-definition-writer accessor))
		       'function)))
		(or (and function-doc writer-doc
			 (string= function-doc writer-doc))
		    (null writer-doc)
		    (and (not (null writer-doc)) (null function-doc)))))
	 (@defgeneric (format nil  "~(~A~)"
			(generic-accessor-definition-symbol accessor))
	     (sb-introspect:function-lambda-list
	      (generic-accessor-definition-function accessor))
	   (@defgenericx (format nil  "~(~A~)"
			   `(setf ,(generic-writer-definition-symbol
				    (generic-accessor-definition-writer
				     accessor))))
			 (sb-introspect:function-lambda-list
			  (generic-writer-definition-function
			   (generic-accessor-definition-writer accessor))))
	   (anchor (generic-accessor-definition-writer accessor))
	   (index (generic-accessor-definition-writer accessor))
	   (document-definition accessor context 'function))
	 (dolist (method (generic-accessor-definition-methods accessor))
	   (document method context))
	 (dolist (method (generic-writer-definition-methods
			  (generic-accessor-definition-writer accessor)))
	   (document method context)))
	(t
	 (call-next-method)
	 (document (generic-accessor-definition-writer accessor) context))))

(defmethod document ((condition condition-definition) context &key)
  "Render CONDITION's documentation in CONTEXT."
  (@defcond (string-downcase (definition-symbol condition))
    (document-definition condition context 'type)))

(defmethod document ((structure structure-definition) context &key)
  "Render STRUCTURE's documentation in CONTEXT."
  (@defstruct (string-downcase (definition-symbol structure))
    (document-definition structure context 'type)))

(defmethod document ((class class-definition) context &key)
  "Render CLASS's documentation in CONTEXT."
  (@defclass (string-downcase (definition-symbol class))
    (document-definition class context 'type)))



;; ==========================================================================
;; Definition Nodes
;; ==========================================================================

(defun add-category-node (parent context status category definitions)
  "Add the STATUS CATEGORY node to PARENT for DEFINITIONS in CONTEXT."
  (add-child parent
    (make-node :name (format nil "~@(~A ~A~)" status category)
	       :section-name (format nil "~@(~A~)" category)
	       :before-menu-contents
	       (render-to-string
		 (dolist (definition (sort definitions #'string-lessp
					   :key #'definition-symbol))
		   (document definition context))))))

(defun add-categories-node (parent context status symbols)
  "Add the STATUS SYMBOLS categories nodes to PARENT in CONTEXT."
  (dolist (category +categories+)
    (let ((category-definitions
	    (loop :for symbol :in symbols
		  :when (symbol-definition symbol (first category))
		    :collect :it)))
      (when category-definitions
	(add-category-node parent context status (second category)
			   category-definitions)))))

(defun add-definitions-node
    (parent context
     &aux (definitions-node
	   (add-child parent
	     (make-node :name "Definitions"
			:synopsis "The symbols documentation"
			:before-menu-contents(format nil "~
Definitions are sorted by export status, category, package, and then by
lexicographic order.")))))
  "Add the definitions node to PARENT in CONTEXT."
  (loop	:for status :in '("exported" "internal")
	:for symbols :in (list
			  (system-external-symbols (context-system context))
			  (system-internal-symbols (context-system context)))
	:when symbols
	  :do (let ((node (add-child definitions-node
			    (make-node
			     :name (format nil "~@(~A~) definitions"
				     status)))))
		(add-categories-node node context status symbols))))


;;; symbol.lisp ends here
