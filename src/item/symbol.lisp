;;; symbol.lisp --- Symbol based items

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
;; Definitions
;; ==========================================================================

;; ----------
;; Categories
;; ----------

;; #### NOTE: when constructing the context lists of external and internal
;; definitions, only the definitions listed in +CATEGORIES+ appear. This is
;; because these lists follow the structure of the Definitions chapter in the
;; generated manual. For instance, methods are listed under the corresponding
;; generic function, so they don't represent a category of its own.

;; #### NOTE: the order in +CATEGORIES+ is important (see
;; ADD-CATEGORIES-NODE). It conditions the order of appearance of the
;; definitions in the generated manual.

(define-constant +categories+
    '((:constant       "constants")
      (:special        "special variables")
      (:macro          "macros")
      (:function       "functions")
      (:generic        "generic functions")
      (:condition      "conditions")
      (:structure      "structures")
      (:class          "classes"))
  "The list of definition categories.
Each category is of type (:KEYWORD DESCRIPTION-STRING).")


;; -----------
;; Definitions
;; -----------

;; #### NOTE: writer structures (either regular or generic) don't store the
;; complete function name (setf <name>) but only the original symbol. This, in
;; conjunction with the fact that definitions are sorted by symbol-name,
;; ensures that standalone writers (not associated with readers) are listed in
;; proper lexicographic order regardless of the SETF part of their name
;; (although that part still appears in the documentation).

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
  "Structure for class definitions.
This structure holds the direct superclasses and direct subclasses
definitions."
  direct-superclasses
  direct-subclasses)


;; ----------------
;; Definition pools
;; ----------------

(defun make-definitions-pool ()
  "Create and return a new definitions pool.
A definitions pool is a hash table of categorized definitions.
Keys must be of the form (NAME :CATEGORY).
  - NAME is the symbol naming the definition,
  - :CATEGORY is one listed in +CATEGORIES+."
  (make-hash-table :test 'equal))

(defun definitions-pool-size (pool)
  "Return the number of elements in definitions POOL."
  (hash-table-count pool))

(defun mapcan-definitions-pool (function pool)
  "Like MAPCAN, but work on a definitions POOL."
  (loop :for definition :being :the :hash-values :in pool
	:nconc (funcall function definition)))

(defun find-definition (key pool &optional errorp)
  "Find a definition matching KEY for SYMBOL in POOL.
KEY must be of the form (NAME :CATEGORY).
If ERRORP, throw an error if not found. Otherwise, just return NIL."
  (let ((definition (gethash key pool)))
    (if (and (null definition) errorp)
	(error "~A definition not found for ~A" (first key) (second key))
      definition)))

(defun category-definitions (category pool)
  "Return all CATEGORY definitions from POOL."
  (loop :for key   :being :the :hash-keys   :in pool
	:for value :being :the :hash-values :in pool
	:when (eq (second key) category)
	  :collect value))

(defun add-definition (symbol category definition pool)
  "Add CATEGORY kind of DEFINITION for SYMBOL to POOL."
  (setf (gethash (list symbol category) pool) definition))

;; #### PORTME.
(defun add-symbol-definition (symbol category pool)
  "Add and return the CATEGORY kind of definition for SYMBOL to pool, if any."
  (or (find-definition (list symbol category) pool)
      (ecase category
	(:constant
	 (when (eql (sb-int:info :variable :kind symbol) :constant)
	   (add-definition
	    symbol category (make-constant-definition :symbol symbol) pool)))
	(:special
	 (when (eql (sb-int:info :variable :kind symbol) :special)
	   (add-definition
	    symbol category (make-special-definition :symbol symbol) pool)))
	(:macro
	 (let ((function (macro-function symbol)))
	   (when function
	     (add-definition
	      symbol
	      category
	      (make-macro-definition :symbol symbol :function function)
	      pool))))
	(:function
	 (let ((function
		 (when (and (fboundp symbol)
			    (not (macro-function symbol))
			    (not (typep (fdefinition symbol)
					'generic-function)))
		   (fdefinition symbol)))
	       (writer
		 (let ((writer-name `(setf ,symbol)))
		   (when (and (fboundp writer-name)
			      (not (typep (fdefinition writer-name)
					  'generic-function)))
		     (fdefinition writer-name)))))
	   (cond ((and function writer)
		  (add-definition
		   symbol
		   category
		   (make-accessor-definition
		    :symbol symbol
		    :function function
		    :writer (make-writer-definition
			     :symbol symbol
			     :function writer))
		   pool))
		 (function
		  (add-definition
		   symbol
		   category
		   (make-function-definition :symbol symbol
					     :function function)
		   pool))
		 (writer
		  (add-definition
		   symbol
		   category
		   (make-writer-definition :symbol symbol
					   :function writer)
		   pool)))))
	(:generic
	 (let ((function
		 (when (and (fboundp symbol)
			    (typep (fdefinition symbol) 'generic-function))
		   (fdefinition symbol)))
	       (writer
		 (let ((writer-name `(setf ,symbol)))
		   (when (and (fboundp writer-name)
			      (typep (fdefinition writer-name)
				     'generic-function))
		     (fdefinition writer-name)))))
	   (cond ((and function writer)
		  ;; #### NOTE: for a generic accessor function, we store
		  ;; accessor methods in the generic accessor function
		  ;; definition, along with standard methods. Only writer-only
		  ;; methods are stored in the generic writer function
		  ;; definition.
		  (add-definition
		   symbol
		   category
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
						   method))
					    nil)))
			 (if writer-method
			     (make-accessor-method-definition
			      :symbol symbol
			      :method method
			      :writer (make-writer-method-definition
				       :symbol symbol
				       :method writer-method))
			   (make-method-definition
			    :symbol symbol :method method))))
		     (sb-mop:generic-function-methods function))
		    :writer (make-generic-writer-definition
			     :symbol symbol
			     :function writer
			     :methods
			     (mapcan
			      (lambda (method)
				(unless (find-method function
						     (method-qualifiers method)
						     ;; #### NOTE: don't
						     ;; forget to remove the
						     ;; first (NEW-VALUE)
						     ;; specializer from the
						     ;; writer method.
						     (cdr
						      (sb-mop:method-specializers
						       method))
						     nil)
				  (list (make-writer-method-definition
					 :symbol symbol
					 :method method))))
			      (sb-mop:generic-function-methods writer))))
		   pool))
		 (function
		  (add-definition
		   symbol
		   category
		   (make-generic-definition
		    :symbol symbol
		    :function function
		    :methods (mapcar (lambda (method)
				       (make-method-definition :symbol symbol
							       :method method))
				     (sb-mop:generic-function-methods
				      function)))
		   pool))
		 (writer
		  (add-definition
		   symbol
		   category
		   (make-generic-writer-definition
		    :symbol symbol
		    :function writer
		    :methods (mapcar (lambda (method)
				       (make-writer-method-definition
					:symbol symbol
					:method method))
				     (sb-mop:generic-function-methods
				      writer)))
		   pool)))))
	(:condition
	 (let ((class (find-class symbol nil)))
	   (when (and class (typep class 'sb-pcl::condition-class))
	     (add-definition
	      symbol
	      category
	      (make-condition-definition :symbol symbol)
	      pool))))
	(:structure
	 (let ((class (find-class symbol nil)))
	   (when (and class (typep class 'sb-pcl::structure-class))
	     (add-definition
	      symbol
	      category
	      (make-structure-definition :symbol symbol)
	      pool))))
	(:class
	 (let ((class (find-class symbol nil)))
	   (when (and class
		      (not (add-symbol-definition symbol :condition pool))
		      (not (add-symbol-definition symbol :structure pool)))
	     (add-definition
	      symbol
	      category
	      (make-class-definition
	       :symbol symbol
	       ;; #### FIXME: create real definitions below once the definition
	       ;; pool is  implemented.
	       :direct-superclasses
	       (mapcar #'class-name
		       #+()(lambda (class)
			     (add-symbol-definition
			      (class-name class)
			      :class
			      pool))
		       (reverse (sb-mop:class-direct-superclasses class)))
	       :direct-subclasses
	       (mapcar #'class-name
		       #+()(lambda (class)
			     (add-symbol-definition
			      (class-name class)
			      :class
			      pool))
		       (reverse (sb-mop:class-direct-subclasses class))))
	      pool)))))))

(defun add-symbol-definitions (symbol pool)
  "Add all categorized definitions for SYMBOL to POOL."
  (dolist (category +categories+)
    (add-symbol-definition symbol (first category) pool)))



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
