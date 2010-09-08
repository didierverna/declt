;;; definition.lisp --- Definitions rendering

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Sat Sep  4 15:27:31 2010
;; Last Revision: Wed Sep  8 09:51:59 2010

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

;; #### NOTE: there are no idnexing methods for methods or symbols, because
;; indexing is done by the lower-level @defXXX routines (Texinfo does half the
;; job and we do the othe rhalf).

(defun document-method (method relative-to)
  "Render METHOD's documentation."
  (@defmethod
      ;; #### PORTME:
      (string-downcase (sb-mop:generic-function-name
			(sb-mop:method-generic-function method)))
      (sb-mop:method-lambda-list method)
      (sb-mop:method-specializers method)
      (sb-mop:method-qualifiers method)
    ;; #### PORTME.
    (let* ((defsrc (sb-introspect:find-definition-source method))
	   (pathname (when defsrc
		       (sb-introspect:definition-source-pathname defsrc))))
      (when pathname
	(@table ()
	  (render-location pathname relative-to))))
    (render-string (documentation method t))))

(defun document-symbol-1 (symbol relative-to type kind)
  "Render SYMBOL's documentation contents as KIND."
  (@table ()
    (format t "@item Package~%")
    (reference (symbol-package symbol))
    ;; #### PORTME.
    (let* ((defsrc
	       ;; #### FIXME: why a list? How can there be several sources?
	       (car
		(sb-introspect:find-definition-sources-by-name symbol type)))
	   (pathname (when defsrc
		       (sb-introspect:definition-source-pathname defsrc))))
      (when pathname
	(render-location pathname relative-to)))
    (let ((documentation (documentation symbol kind)))
      (when documentation
	(format t "@item Documentation~%")
	(render-string documentation)))))

(defun document-symbol (symbol relative-to category)
  "Render SYMBOL's documentation in CATEGORY."
  (ecase category
    (:constant
     (when (definitionp symbol :constant)
       (@defconstant (string-downcase symbol)
	 (document-symbol-1 symbol relative-to :constant 'variable))))
    (:special
     (when (definitionp symbol :special)
       (@defspecial (string-downcase symbol)
	 (document-symbol-1 symbol relative-to :variable 'variable))))
    (:macro
     (when (definitionp symbol :macro)
       (@defmac (string-downcase symbol)
	   ;; #### PORTME.
	   (sb-introspect:function-lambda-list symbol)
	 (document-symbol-1 symbol relative-to :macro 'function))))
    (:function
     (when (definitionp symbol :function)
       (@defun (string-downcase symbol)
	   ;; #### PORTME.
	   (sb-introspect:function-lambda-list symbol)
	 (document-symbol-1 symbol relative-to :function 'function))))
    (:generic
     (when (definitionp symbol :generic)
       (@defgeneric (string-downcase symbol)
	   ;; #### PORTME.
	   (sb-introspect:function-lambda-list symbol)
	 (document-symbol-1 symbol relative-to :generic-function 'function))
       ;; #### PORTME.
       (dolist (method (sb-mop:generic-function-methods (fdefinition symbol)))
	 (document-method method relative-to))))
    (:condition
     (when (definitionp symbol :condition)
       (@defcond (string-downcase symbol)
	 (document-symbol-1 symbol relative-to :condition 'type))))
    (:structure
     (when (definitionp symbol :structure)
       (@defstruct (string-downcase symbol)
	 (document-symbol-1 symbol relative-to :structure 'type))))
    (:class
     (when (definitionp symbol :class)
       (@defclass (string-downcase symbol)
	 (document-symbol-1 symbol relative-to :class 'type))))))



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
		   (document-symbol symbol relative-to (first category)))))))

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
