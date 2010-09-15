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
;; Symbol-based Items
;; ==========================================================================

;; #### NOTE: +CATEGORIES+ and SYMBOL-DEFINITION don't handle methods, because
;; those are listed directly as part of generic function definitions.
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

(defstruct definition (symbol))

(defstruct (constant-definition (:include definition)))
(defstruct (special-definition (:include definition)))

(defstruct (functional-definition (:include definition)) (function))
(defstruct (macro-definition (:include functional-definition)))
(defstruct (function-definition (:include functional-definition)))

(defstruct (method-definition (:include definition)) (method))
(defstruct (generic-definition (:include functional-definition)) (methods))

(defstruct (condition-definition (:include definition)))
(defstruct (structure-definition (:include definition)))
(defstruct (class-definition (:include definition)))

(defgeneric definition-type-name (definition)
  (:documentation "Return DEFINITION's type name.")
  (:method ((constant constant-definition)) "constant")
  (:method ((special special-definition)) "special variable")
  (:method ((macro macro-definition)) "macro")
  (:method ((function function-definition)) "function")
  (:method ((generic generic-definition)) "generic function")
  (:method ((method method-definition)) "method")
  (:method ((condition condition-definition)) "condition")
  (:method ((structure structure-definition)) "structure")
  (:method ((class class-definition)) "class"))

;; #### PORTME.
(defun symbol-definition (symbol category)
  "Return a SYMBOL definition of CATEGORY if any."
  (ecase category
    (:constant
     (when (eql (sb-int:info :variable :kind symbol) :constant)
       (make-constant-definition :symbol symbol)))
    (:special
     (when (eql (sb-int:info :variable :kind symbol) :special)
       (make-special-definition :symbol symbol)))
    (:macro
     (let ((function (macro-function symbol)))
       (when function
	 (make-macro-definition :symbol symbol :function function))))
    (:function
     (when (and (fboundp symbol)
		(not (symbol-definition symbol :macro))
		(not (symbol-definition symbol :generic)))
       (make-function-definition :symbol symbol
				 :function (fdefinition symbol))))
    (:generic
     (when (and (fboundp symbol)
		(typep (fdefinition symbol) 'generic-function))
       (make-generic-definition
	:symbol symbol
	:function (fdefinition symbol)
	:methods (mapcar (lambda (method)
			   (make-method-definition :symbol symbol
						   :method method))
			 (sb-mop:generic-function-methods
			  (fdefinition symbol))))))
    (:condition
     (let ((class (find-class symbol nil)))
       (when (and class (typep class 'sb-pcl::condition-class))
	 (make-condition-definition :symbol symbol))))
    (:structure
     (let ((class (find-class symbol nil)))
       (when (and class (typep class 'sb-pcl::structure-class))
	 (make-structure-definition :symbol symbol))))
    (:class
     (let ((class (find-class symbol nil)))
       (when (and class
		  (not (symbol-definition symbol :condition))
		  (not (symbol-definition symbol :structure)))
	 (make-class-definition :symbol symbol))))))

(defun symbol-definitions (symbol)
  "Return all definitions named by SYMBOL if any."
  (loop :for category :in (mapcar #'first +categories+)
	:when (symbol-definition symbol category)
	:collect :it))



;; ==========================================================================
;; Rendering protocols
;; ==========================================================================

(defmethod name ((definition definition))
  "Return DEFINITION's symbol name."
  (name (definition-symbol definition)))



;; ==========================================================================
;; Item Protocols
;; ==========================================================================

(defmethod location ((method method-definition))
  ;; #### PORTME.
  (let* ((defsrc (sb-introspect:find-definition-source
		  (method-definition-method method))))
    (when defsrc
      (sb-introspect:definition-source-pathname defsrc))))

;; #### PORTME.
;; #### FIXME: why does f-d-s-b-n return a list? How can there be several
;; sources?
(defun definition-source (definition type)
  (let ((defsrc (car (sb-introspect:find-definition-sources-by-name
		      (definition-symbol definition)
		      type))))
    (when defsrc
      (sb-introspect:definition-source-pathname defsrc))))

(defmethod location ((constant constant-definition))
  (definition-source constant :constant))

(defmethod location ((special special-definition))
  (definition-source special :variable))

(defmethod location ((macro macro-definition))
  (definition-source macro :macro))

(defmethod location ((function function-definition))
  (definition-source function :function))

(defmethod location ((generic generic-definition))
  (definition-source generic :generic-function))

(defmethod location ((condition condition-definition))
  (definition-source condition :condition))

(defmethod location ((structure structure-definition))
  (definition-source structure :structure))

(defmethod location ((class class-definition))
  (definition-source class :class))


;;; symbol.lisp ends here
