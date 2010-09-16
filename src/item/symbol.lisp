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
    '((:constant  "constants")
      (:special   "special variables")
      (:macro     "macros")
      (:function  "functions")
      (:generic   "generic functions")
      (:condition "conditions")
      (:structure "structures")
      (:class     "classes"))
  "The list of definition categories.")

(defstruct definition
  "Base structure for definitions named by symbols.
This structure holds the symbol naming the definition."
  (symbol)) ;; symbol naming the definition

(defstruct (constant-definition (:include definition))
  "Structure for constant definitions.")
(defstruct (special-definition (:include definition))
  "Structure for special variables definitions.")

(defstruct (functional-definition (:include definition))
  "Base structure for definitions of functional values.
This structure holds the the function, generic function or macro function
object."
  (function)) ;; function, generic function or macro function objet
(defstruct (macro-definition (:include functional-definition))
  "Structure for macro definitions.")
(defstruct (function-definition (:include functional-definition))
  "Structure for ordinary function definitions.")

(defstruct (method-definition (:include definition))
  "Structure for method definitions.
This structure holds the method object."
  (method)) ;; method object

(defstruct (generic-definition (:include functional-definition))
  "Structure for generic function definitions.
This structure holds the list of methods."
  (methods)) ;; list of method objects

(defstruct (condition-definition (:include definition))
  "Structure for condition definitions.")
(defstruct (structure-definition (:include definition))
  "Structure for structure definition.")
(defstruct (class-definition (:include definition))
  "Structure for class definitions.")

(defgeneric definition-type-name (definition)
  (:documentation "Return DEFINITION's type name.")
  (:method ((constant constant-definition))
    "Return \"constant\""
    "constant")
  (:method ((special special-definition))
    "Return \"special variable\""
    "special variable")
  (:method ((macro macro-definition))
    "Return \"macro\""
    "macro")
  (:method ((function function-definition))
    "Return \"function\""
    "function")
  (:method ((generic generic-definition))
    "Return \"generic function\""
    "generic function")
  (:method ((method method-definition))
    "Return \"method\""
    "method")
  (:method ((condition condition-definition))
    "Return \"condition\""
    "condition")
  (:method ((structure structure-definition))
    "Return \"structure\""
    "structure")
  (:method ((class class-definition))
    "Return \"class\""
    "class"))

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

(defmethod source ((method method-definition))
  "Return METHOD's definition source."
  ;; #### PORTME.
  (let* ((defsrc (sb-introspect:find-definition-source
		  (method-definition-method method))))
    (when defsrc
      (sb-introspect:definition-source-pathname defsrc))))

;; #### PORTME.
;; #### FIXME: why does f-d-s-b-n return a list? How can there be several
;; sources?
(defun definition-source (definition type)
  "Return DEFINITION's source for TYPE."
  (let ((defsrc (car (sb-introspect:find-definition-sources-by-name
		      (definition-symbol definition)
		      type))))
    (when defsrc
      (sb-introspect:definition-source-pathname defsrc))))

(defmethod source ((constant constant-definition))
  "Return CONSTANT's definition source."
  (definition-source constant :constant))

(defmethod source ((special special-definition))
  "Return SPECIAL's definition source."
  (definition-source special :variable))

(defmethod source ((macro macro-definition))
  "Return MACRO's definition source."
  (definition-source macro :macro))

(defmethod source ((function function-definition))
  "Return FUNCTION's definition source."
  (definition-source function :function))

(defmethod source ((generic generic-definition))
  "Return GENERIC's definition source."
  (definition-source generic :generic-function))

(defmethod source ((condition condition-definition))
  "Return CONDITION's definition source."
  (definition-source condition :condition))

(defmethod source ((structure structure-definition))
  "Return STRUCTURE's definition source."
  (definition-source structure :structure))

(defmethod source ((class class-definition))
  "Return CLASS's definition source."
  (definition-source class :class))


;;; symbol.lisp ends here
