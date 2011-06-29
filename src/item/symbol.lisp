;;; symbol.lisp --- Symbol based items

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
;; Symbol-based Items
;; ==========================================================================

;; #### NOTE: +CATEGORIES+ and SYMBOL-DEFINITION don't handle methods, because
;; those are listed directly as part of generic function definitions.

;; #### NOTE: the order in +CATEGORIES+ is important (see
;; ADD-CATEGORIES-NODE). Also, writer structures don't store the complete
;; function name (setf <name>) but only the original symbol. This, in
;; conjunction with the fact that definitions are sorted by symbol-name,
;; ensures that standalone writers (not associated with readers) are listed in
;; proper lexicographic order regardless of the SETF part of their name (and
;; although that part still appears in the documentation).

(define-constant +categories+
    '((:constant       "constants")
      (:special        "special variables")
      (:macro          "macros")
      (:function       "functions")
      (:generic        "generic functions")
      (:condition      "conditions")
      (:structure      "structures")
      (:class          "classes"))
  "The list of definition categories.")

(defstruct definition
  "Base structure for definitions named by symbols.
This structure holds the symbol naming the definition."
  symbol)

(defstruct (constant-definition (:include definition))
  "Structure for constant definitions.")
(defstruct (special-definition (:include definition))
  "Structure for special variables definitions.")

(defstruct (functional-definition (:include definition))
  "Base structure for definitions of functional values.
This structure holds the the function, generic function or macro function
object."
  function)

(defstruct (macro-definition (:include functional-definition))
  "Structure for macro definitions.")

(defstruct (function-definition (:include functional-definition))
  "Structure for ordinary function definitions.")
(defstruct (writer-definition (:include function-definition))
  "Structure for ordinary writer function definitions.")
(defstruct (accessor-definition (:include function-definition))
  "Structure for accessor function definitions.
This structure holds the writer function definition."
  writer)

(defstruct (method-definition (:include definition))
  "Base structure for method definitions.
This structure holds the method object."
  method)
(defstruct (writer-method-definition (:include method-definition))
  "Structure for writer method definitions.")
(defstruct (accessor-method-definition (:include method-definition))
  "Structure for accessor method definitions.
This structure holds the writer method definition."
  writer)

(defstruct (generic-definition (:include functional-definition))
  "Structure for generic function definitions.
This structure holds the list of method definitions."
  methods)
(defstruct (generic-writer-definition (:include generic-definition))
  "Structure for generic writer function definitions.")
(defstruct (generic-accessor-definition (:include generic-definition))
  "Structure for generic accessor function definitions.
This structure holds the generic writer function definition."
  writer)

(defstruct (condition-definition (:include definition))
  "Structure for condition definitions.")
(defstruct (structure-definition (:include definition))
  "Structure for structure definition.")
(defstruct (class-definition (:include definition))
  "Structure for class definitions.")


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
     (let ((function
	     (when (and (fboundp symbol)
			(not (macro-function symbol))
			(not (typep (fdefinition symbol) 'generic-function)))
	       (fdefinition symbol)))
	   (writer
	     (let ((writer-name `(setf ,symbol)))
	       (when (and (fboundp writer-name)
			  (not (typep (fdefinition writer-name)
				      'generic-function)))
		 (fdefinition writer-name)))))
       (cond ((and function writer)
	      (make-accessor-definition
	       :symbol symbol
	       :function function
	       :writer (make-writer-definition
			:symbol symbol
			:function writer)))
	     (function
	      (make-function-definition :symbol symbol :function function))
	     (writer
	      (make-writer-definition :symbol symbol :function writer)))))
    (:generic
     (let ((function
	     (when (and (fboundp symbol)
			(typep (fdefinition symbol) 'generic-function))
	       (fdefinition symbol)))
	   (writer
	     (let ((writer-name `(setf ,symbol)))
	       (when (and (fboundp writer-name)
			  (typep (fdefinition writer-name) 'generic-function))
		 (fdefinition writer-name)))))
       (cond ((and function writer)
	      ;; #### NOTE: for a generic accessor function, we store accessor
	      ;; methods in the generic accessor function definition, along
	      ;; with standard methods. Only writer-only methods are stored in
	      ;; the generic writer function definition.
	      (make-generic-accessor-definition
	       :symbol symbol
	       :function function
	       :methods
	       (mapcar
		(lambda (method)
		  (let ((writer-method
			  (find-method writer
				       (method-qualifiers method)
				       ;; #### FIXME: I'm not sure if the
				       ;; first argument (NEW-VALUE) of a
				       ;; writer method always has a
				       ;; specializer of T...
				       (cons t
					     (sb-mop:method-specializers
					      method)))))
		    (if writer-method
			(make-accessor-method-definition
			 :symbol symbol
			 :method method
			 :writer (make-writer-method-definition
				  :symbol symbol
				  :method writer-method))
		      (make-method-definition :symbol symbol :method method))))
		(sb-mop:generic-function-methods function))
	       :writer (make-generic-writer-definition
			:symbol symbol
			:function writer
			:methods
			(mapcan
			 (lambda (method)
			   (unless (find-method function
						(method-qualifiers method)
						;; #### NOTE: don't forget to
						;; remove the first
						;; (NEW-VALUE) specializer
						;; from the writer method.
						(cdr
						 (sb-mop:method-specializers
						  method)))
			     (list (make-writer-method-definition
				    :symbol symbol
				    :method method))))
			 (sb-mop:generic-function-methods writer)))))
	     (function
	      (make-generic-definition
	       :symbol symbol
	       :function function
	       :methods (mapcar (lambda (method)
				  (make-method-definition :symbol symbol
							  :method method))
				(sb-mop:generic-function-methods function))))
	     (writer
	      (make-generic-writer-definition
	       :symbol symbol
	       :function writer
	       :methods (mapcar (lambda (method)
				  (make-writer-method-definition
				   :symbol symbol
				   :method method))
				(sb-mop:generic-function-methods writer)))))))
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

;; #### NOTE: all of these methods are in fact equivalent. That's the drawback
;; of using structures instead of classes, which limits the inheritance
;; expressiveness (otherwise I could have used a writer mixin or something).
(defmethod name ((writer writer-definition))
  "Return WRITER's name, that is (setf <name>)."
  (format nil "(SETF ~A)" (name (writer-definition-symbol writer))))

(defmethod name ((writer-method writer-method-definition))
  "Return WRITER-METHOD's name, that is (setf <name>)."
  (format nil "(SETF ~A)"
    (name (writer-method-definition-symbol writer-method))))

(defmethod name ((generic-writer generic-writer-definition))
  "Return GENERIC-WRITER's name, that is (setf <name>)."
  (format nil "(SETF ~A)"
    (name (generic-writer-definition-symbol generic-writer))))



;; ==========================================================================
;; Item Protocols
;; ==========================================================================

;; ---------------
;; Source protocol
;; ---------------

;; #### NOTE: SB-INTROSPECT:FIND-DEFINITION-SOURCES-BY-NAME may return
;; multiple sources (e.g. if we were to ask it for methods) so we take the
;; first one. That is okay because we actually use it only when there can be
;; only one definition source.
;; #### PORTME.
(defun definition-source-by-name
    (definition type
     &key (name (definition-symbol definition))
     &aux (defsrc (car (sb-introspect:find-definition-sources-by-name
			name type))))
  "Return DEFINITION's source for TYPE."
  (when defsrc
    (sb-introspect:definition-source-pathname defsrc)))

(defun definition-source
    (object &aux (defsrc (sb-introspect:find-definition-source object)))
  "Return OBJECT's definition source."
  (when defsrc
    (sb-introspect:definition-source-pathname defsrc)))


(defmethod source ((constant constant-definition))
  "Return CONSTANT's definition source."
  (definition-source-by-name constant :constant))

(defmethod source ((special special-definition))
  "Return SPECIAL's definition source."
  (definition-source-by-name special :variable))

(defmethod source ((funcoid functional-definition))
  "Return FUNCOID's definition source."
  (definition-source (functional-definition-function funcoid)))

(defmethod source ((method method-definition))
  "Return METHOD's definition source."
  (definition-source (method-definition-method method)))

(defmethod source ((condition condition-definition))
  "Return CONDITION's definition source."
  (definition-source-by-name condition :condition))

(defmethod source ((structure structure-definition))
  "Return STRUCTURE's definition source."
  (definition-source-by-name structure :structure))

(defmethod source ((class class-definition))
  "Return CLASS's definition source."
  (definition-source-by-name class :class))


;; -----------------------------------
;; Definition file definition protocol
;; -----------------------------------

(defgeneric definition-file-definitions (definition file)
  (:documentation
   "Return the list of definitions from DEFINITION that belong to FILE.")
  (:method (definition file)
    "Default method for definitions not containing sub-definitions."
    (when (equal (source definition) file)
      (list definition)))
  (:method ((accessor accessor-definition) file)
    "Handle ACCESSOR and its writer function."
    (nconc (call-next-method)
	   (definition-file-definitions
	    (accessor-definition-writer accessor)
	    file)))
  (:method ((accessor-method accessor-method-definition) file)
    "Handle ACCESSOR-METHOD and its writer method."
    (nconc (call-next-method)
	   (definition-file-definitions
	    (accessor-method-definition-writer accessor-method)
	    file)))
  (:method ((generic generic-definition) file)
    "Handle GENERIC function and its methods."
    (nconc (call-next-method)
	   (mapcan (lambda (method)
		     (definition-file-definitions method file))
		   (generic-definition-methods generic))))
  (:method ((generic-accessor generic-accessor-definition) file)
    "Handle GENERIC-ACCESSOR and its generic writer function."
    (nconc (call-next-method)
	   (definition-file-definitions
	    (generic-accessor-definition-writer generic-accessor)
	    file))))


;; ------------------
;; Type name protocol
;; ------------------

(defmethod type-name ((constant constant-definition))
  "Return \"constant\""
  "constant")

(defmethod type-name ((special special-definition))
  "Return \"special variable\""
  "special variable")

(defmethod type-name ((macro macro-definition))
  "Return \"macro\""
  "macro")

(defmethod type-name ((function function-definition))
  "Return \"function\""
  "function")

(defmethod type-name ((generic generic-definition))
  "Return \"generic function\""
  "generic function")

(defmethod type-name ((method method-definition))
  "Return \"method\""
  "method")

(defmethod type-name ((condition condition-definition))
  "Return \"condition\""
  "condition")

(defmethod type-name ((structure structure-definition))
  "Return \"structure\""
  "structure")

(defmethod type-name ((class class-definition))
  "Return \"class\""
  "class")


;;; symbol.lisp ends here
