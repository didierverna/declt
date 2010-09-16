;;; symbol.lisp --- Symbol based documentation

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Sat Sep  4 15:27:31 2010
;; Last Revision: Thu Sep  9 18:53:22 2010

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
  (declare (ignore relative-to))
  (format nil "The ~(~A~) constant" (escape constant)))

(defmethod title ((special special-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~(~A~) special variable" (escape special)))

(defmethod title ((macro macro-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~(~A~) macro" (escape macro)))

(defmethod title ((function function-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~(~A~) function" (escape function)))

(defmethod title ((method method-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~(~A~{ ~A~^~}~{ ~A~^~}~) method"
    (escape method)
    ;; #### PORTME.
    (mapcar #'escape (mapcar #'pretty-specializer
			     (sb-mop:method-specializers
			      (method-definition-method method))))
    (mapcar #'escape (sb-mop:method-qualifiers
		      (method-definition-method method)))))

(defmethod title ((generic generic-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~(~A~) generic function" (escape generic)))

(defmethod title ((condition condition-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~(~A~) condition" (escape condition)))

(defmethod title ((structure structure-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~(~A~) structure" (escape structure)))

(defmethod title ((class class-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~(~A~) class" (escape class)))

;; #### NOTE: the INDEX methods below only perform sub-indexing because the
;; main index entries are created automatically in Texinfo by the @defXXX
;; routines.

(defmethod index ((constant constant-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@vindex @r{Constant, }~(~A~)~%" (escape constant)))

(defmethod index ((special special-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@vindex @r{Special Variable, }~(~A~)~%" (escape special)))

(defmethod index ((macro macro-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@findex @r{Macro, }~(~A~)~%" (escape macro)))

(defmethod index ((function function-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@findex @r{Function, }~(~A~)~%" (escape function)))

(defmethod index ((method method-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@findex @r{Method, }~(~A~)~%" (escape method)))

(defmethod index ((generic generic-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@findex @r{Generic Function, }~(~A~)~%" (escape generic)))

(defmethod index ((condition condition-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@tpindex @r{Condition, }~(~A~)~%" (escape condition)))

(defmethod index ((structure structure-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@tpindex @r{Structure, }~(~A~)~%" (escape structure)))

(defmethod index ((class class-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@tpindex @r{Class, }~(~A~)~%" (escape class)))

(defmethod anchor ((definition definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "~A anchor" (title definition)))

(defmethod reference ((definition definition) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@ref{~A, , @t{~(~A}~)} (~A)~%"
    (anchor definition)
    (escape definition)
    (definition-type-name definition)))

(defun document-definition (definition relative-to kind)
  "Render DEFINITION's documentation contents as KIND."
  (@anchor (anchor definition))
  (index definition)
  (@table ()
    (let ((documentation (documentation (definition-symbol definition) kind)))
      (when documentation
	(format t "@item Documentation~%")
	(render-text documentation)))
    (format t "~&@item Package~%")
    (reference (symbol-package (definition-symbol definition)))
    (render-source definition relative-to)))

(defmethod document ((constant constant-definition) relative-to &key)
  (@defconstant (string-downcase (definition-symbol constant))
    (document-definition constant relative-to 'variable)))

(defmethod document ((special special-definition) relative-to &key)
  (@defspecial (string-downcase (definition-symbol special))
    (document-definition special relative-to 'variable)))

(defmethod document ((macro macro-definition) relative-to &key)
  (@defmac (string-downcase (definition-symbol macro))
      ;; #### PORTME.
      (sb-introspect:function-lambda-list
       (macro-definition-function macro))
    (document-definition macro relative-to 'function)))

(defmethod document ((function function-definition) relative-to &key)
  (@defun (string-downcase (definition-symbol function))
      ;; #### PORTME.
      (sb-introspect:function-lambda-list
       (function-definition-function function))
    (document-definition function relative-to 'function)))

(defmethod document ((method method-definition) relative-to &key)
  (@defmethod
      ;; #### PORTME:
      (string-downcase (definition-symbol method))
      (sb-mop:method-lambda-list (method-definition-method method))
      (sb-mop:method-specializers (method-definition-method method))
      (sb-mop:method-qualifiers (method-definition-method method))
    (@table ()
      (@anchor (anchor method))
      (index method)
      (let ((documentation
	     (documentation (method-definition-method method) t)))
	(when documentation
	  (format t "@item Documentation~%")
	  (render-text documentation)))
      (render-source method relative-to))))

(defmethod document ((generic generic-definition) relative-to &key)
  (@defgeneric (string-downcase (definition-symbol generic))
      ;; #### PORTME.
      (sb-introspect:function-lambda-list
       (generic-definition-function generic))
    (document-definition generic relative-to 'function))
  (dolist (method (generic-definition-methods generic))
    (document method relative-to)))

(defmethod document ((condition condition-definition) relative-to &key)
  (@defcond (string-downcase (definition-symbol condition))
    (document-definition condition relative-to 'type)))

(defmethod document ((structure structure-definition) relative-to &key)
  (@defstruct (string-downcase (definition-symbol structure))
    (document-definition structure relative-to 'type)))

(defmethod document ((class class-definition) relative-to &key)
  (@defclass (string-downcase (definition-symbol class))
    (document-definition class relative-to 'type)))



;; ==========================================================================
;; Definition Nodes
;; ==========================================================================

(defun add-category-node (parent location category definitions relative-to)
  "Add LOCATION CATEGORY node to PARENT for DEFINITIONS."
  (add-child parent
    (make-node :name (format nil "~@(~A ~A~)" location category)
	       :section-name (format nil "~@(~A~)" category)
	       :before-menu-contents
	       (render-to-string
		 (dolist (definition (sort definitions #'string-lessp
					   :key #'definition-symbol))
		   (document definition relative-to))))))

(defun add-categories-node (parent location symbols relative-to)
  "Add all relevant category nodes to PARENT for LOCATION SYMBOLS."
  (dolist (category +categories+)
    (let ((category-definitions
	   (loop :for symbol :in symbols
		 :when (symbol-definition symbol (first category))
		 :collect :it)))
      (when category-definitions
	(add-category-node parent location (second category)
			   category-definitions relative-to)))))

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
	:for status :in '("exported" "internal")
	:when symbols
	:do (let ((node (add-child definitions-node
			  (make-node :name (format nil "~@(~A~) definitions"
					     status)))))
	      (add-categories-node node status symbols system-directory))))


;;; symbol.lisp ends here
