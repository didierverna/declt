;;; symbol.lisp --- Symbol based documentation

;; Copyright (C) 2010-2013 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

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


;;; Code:

(in-package :com.dvlsoft.declt)
(in-readtable :com.dvlsoft.declt)


;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun render-internal-definitions-references (definitions)
  "Render references to a list of internal DEFINITIONS."
  (render-references definitions "Internal Definitions"))

(defun render-external-definitions-references (definitions)
  "Render references to a list of external DEFINITIONS."
  (render-references definitions "Exported Definitions"))

(defun render-definition-core (definition context)
  "Render DEFINITION's documentation core in CONTEXT.
The documentation core includes all common definition attributes:
  - package,
  - source location.

Each element is rendered as a table item."
  (@tableitem "Package"
    (reference (symbol-package (definition-symbol definition))))
  (render-source definition context))

(defmacro render-@defvaroid (kind varoid context &body body)
  "Render VAROID's definition of KIND in CONTEXT."
  (let ((|@defform| (intern (concatenate 'string "@DEF" (symbol-name kind))
			    :com.dvlsoft.declt))
	(the-varoid (gensym "varoid")))
    `(let ((,the-varoid ,varoid))
       (,|@defform| (string-downcase (name ,the-varoid))
	 (anchor-and-index ,the-varoid)
	 (render-docstring ,the-varoid)
	 (@table ()
	   (render-definition-core ,the-varoid ,context)
	   ,@body)))))

(defun render-@defconstant (constant context)
  "Render CONSTANT's documentation in CONTEXT."
  (render-@defvaroid :constant constant context))

(defun render-@defspecial (special context)
  "Render SPECIAL variable's documentation in CONTEXT."
  (render-@defvaroid :special special context))

;; #### NOTE: see comment on the DOCSTRING method in ../item/symbol.lisp.
(defmacro render-@defsymbolmacro (symbol-macro context &body body)
  "Render SYMBOL-MACRO's documentation in CONTEXT."
  `(render-@defvaroid :symbolmacro ,symbol-macro ,context ,@body))

(defmacro render-@defunoid (kind (funcoid &rest funcoids) context &body body)
  "Render FUNCOID's definition of KIND in CONTEXT.
When FUNCOIDS, render their definitions jointly."
  (let ((|@defform| (intern (concatenate 'string "@DEF" (symbol-name kind))
			    :com.dvlsoft.declt))
	(|@defformx| (intern (concatenate 'string
			       "@DEF" (symbol-name kind) "X")
			     :com.dvlsoft.declt))
	(the-funcoid (gensym "funcoid")))
    `(let ((,the-funcoid ,funcoid))
       (,|@defform| (string-downcase (name ,the-funcoid))
		    (lambda-list ,the-funcoid)
	 (anchor-and-index ,the-funcoid)
	 ,@(mapcar (lambda (funcoid)
		     (let ((the-funcoid (gensym "funcoid")))
		       `(let ((,the-funcoid ,funcoid))
			  (,|@defformx|
			   (string-downcase (name ,the-funcoid))
			   (lambda-list ,the-funcoid))
			  (anchor-and-index ,the-funcoid))))
		   funcoids)
	 (render-docstring ,the-funcoid)
	 (@table ()
	   (render-definition-core ,the-funcoid ,context)
	   ,@body)))))

(defun render-@defun (function context)
  "Render FUNCTION's definition in CONTEXT."
  (render-@defunoid :un (function) context))

(defun render-@defunx (reader writer context)
  "Render READER and WRITER's definitions jointly in CONTEXT."
  (render-@defunoid :un (reader writer) context))

(defun render-@defmac (macro context)
  "Render MACRO's definition in CONTEXT."
  (render-@defunoid :mac (macro) context))

(defun render-@defcompilermacro (compiler-macro context)
  "Render COMPILER-MACRO's definition in CONTEXT."
  (render-@defunoid :compilermacro (compiler-macro) context))

(defmacro %render-@defmethod ((method &rest methods) context)
  "Render METHOD's definition in CONTEXT.
When METHODS, render their definitions jointly."
  (let ((the-method (gensym "method")))
    `(let ((,the-method ,method))
       (@defmethod (string-downcase (name ,the-method))
	   (lambda-list ,the-method)
	   (specializers ,the-method)
	   (qualifiers ,the-method)
	 (anchor-and-index ,the-method)
	 ,@(mapcar (lambda (method)
		     (let ((the-method (gensym "method")))
		       `(let ((,the-method ,method))
			  (@defmethodx
			   (string-downcase (name ,the-method))
			   (lambda-list ,the-method)
			   (specializers ,the-method)
			   (qualifiers ,the-method))
			  (anchor-and-index ,the-method))))
		   methods)
	 (render-docstring ,the-method)
	 (@table ()
	   (render-source ,the-method ,context))))))

(defun render-@defmethod (method context)
  "Render METHOD's definition in CONTEXT."
  (%render-@defmethod (method) context))

(defun render-@defmethodx (reader writer context)
  "Render READER and WRITER methods'definitions jointly in CONTEXT."
  (%render-@defmethod (reader writer) context))

(defmacro render-@defgeneric (generic context &body body)
  "Render GENERIC's definition in CONTEXT."
  `(render-@defunoid :generic (,generic) ,context ,@body))

(defmacro render-@defgenericx (reader writer context &body body)
  "Render generic READER and WRITER's definitions jointly in CONTEXT."
  `(render-@defunoid :generic (,reader ,writer) ,context ,@body))

(defun render-@defsetf (expander context)
  "Render setf EXPANDER's definition in CONTEXT."
  (@defsetf (string-downcase (name expander)) (lambda-list expander)
    (anchor-and-index expander)
    (render-docstring expander)
    (@table ()
      (render-definition-core expander context)
      (when (definition-p (setf-expander-definition-expander expander))
	(@tableitem "Expansion"
	  (reference (setf-expander-definition-expander expander)))))))

(defun render-slot-property
    (slot property
	  &key (renderer (lambda (value)
			   (format t "@t{~A}~%"
			     (escape (format nil "~(~S~)" value)))))
	  &aux (value (slot-property (slot-definition-slot slot) property)))
  "Render SLOT definition's PROPERTY value as a table item."
  (when (and value
	     (not (and (eq value t) (eq property :type)))
	     (not (and (eq value :instance) (eq property :allocation))))
    (@tableitem (format nil "~@(~A~)" (symbol-name property))
      (funcall renderer value))))

(defun render-@defslot (slot)
  "Render SLOT's documentation."
  (@defslot (string-downcase (name slot))
    (index slot)
    (render-docstring slot)
    (@table ()
      (render-slot-property slot :type)
      (render-slot-property slot :allocation)
      (render-slot-property slot :initargs
	:renderer (lambda (value)
		    (let ((values (mapcar (lambda (val)
					    (escape
					     (format nil "~(~S~)" val)))
					  value)))
		      (format t "@t{~A}~{, @t{~A}~}"
			(first values)
			(rest values)))))
      (render-slot-property slot :initform)
      (render-references (slot-definition-readers slot) "Readers")
      (render-references (slot-definition-writers slot) "Writers"))))

(defun render-slots
    (classoid &aux (slots (classoid-definition-slots classoid)))
  "Render CLASSOID's direct slots documentation."
  (when slots
    (@tableitem "Direct slots"
      (dolist (slot slots)
	(render-@defslot slot)))))

(defmacro render-@defcombination (kind combination context &body body)
  "Render method COMBINATION's definition of KIND in CONTEXT."
  (let ((the-combination (gensym "combination")))
    `(let ((,the-combination ,combination))
       (@defcombination (string-downcase (name ,the-combination)) ,kind
	 (anchor-and-index ,the-combination)
	 (render-docstring ,the-combination)
	 (@table ()
	   (render-definition-core ,the-combination ,context)
	   ,@body)))))

(defmacro render-@defclassoid (kind classoid context &body body)
  "Render CLASSOID's definition of KIND in CONTEXT."
  (let ((|@defform| (intern (concatenate 'string "@DEF" (symbol-name kind))
			    :com.dvlsoft.declt))
	(the-classoid (gensym "classoid")))
    `(let ((,the-classoid ,classoid))
       (,|@defform| (string-downcase (name ,the-classoid))
	 (anchor-and-index ,the-classoid)
	 (render-docstring ,the-classoid)
	 (@table ()
	   (render-definition-core ,the-classoid ,context)
	   (render-references
	    (classoid-definition-parents ,the-classoid)
	    "Direct superclasses")
	   (render-references
	    (classoid-definition-children ,the-classoid)
	    "Direct subclasses")
	   (render-references
	    (classoid-definition-methods ,the-classoid)
	    "Direct methods")
	   (render-slots ,the-classoid)
	   ,@body)))))

;; #### PORTME.
(defun render-initargs
    (classoid
     &aux (initargs (sb-mop:class-direct-default-initargs
		     (find-class (classoid-definition-symbol classoid)))))
  "Render CLASSOID's direct default initargs."
  (when initargs
    (@tableitem "Direct Default Initargs"
      ;; #### FIXME: we should rather compute the longest initarg name and use
      ;; that as a template size for the @headitem specification.
      (@multitable (.3 .5)
	(format t "@headitem Initarg @tab Value~%")
	(dolist (initarg initargs)
	  (format t "@item @t{~A}~%@tab @t{~A}~%"
	    (escape (format nil "~(~S~)" (first initarg)))
	    (escape (format nil "~(~S~)" (second initarg)))))))))

(defun render-@defcond (condition context)
  "Render CONDITION's definition in CONTEXT."
  (render-@defclassoid :cond condition context
     (render-initargs condition)))

(defun render-@defstruct (structure context)
  "Render STRUCTURE's definition in CONTEXT."
  (render-@defclassoid :struct structure context))

(defun render-@defclass (class context)
  "Render CLASS's definition in CONTEXT."
  (render-@defclassoid :class class context
     (render-initargs class)))

(defun render-@deftype (type context)
  "Render TYPE's definition in CONTEXT."
  (@deftype ((string-downcase (name type)) (lambda-list type))
    (anchor-and-index type)
    (render-docstring type)
    (@table ()
      (render-definition-core type context))))



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

(defmethod title ((symbol-macro symbol-macro-definition) &optional relative-to)
  "Return SYMBOL-MACRO's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~) symbol macro" (name symbol-macro)))

(defmethod title ((macro macro-definition) &optional relative-to)
  "Return MACRO's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~) macro" (name macro)))

(defmethod title
    ((compiler-macro compiler-macro-definition) &optional relative-to)
  "Return COMPILER-MACRO's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~) compiler macro" (name compiler-macro)))

(defmethod title ((function function-definition) &optional relative-to)
  "Return FUNCTION's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~) function" (name function)))

(defmethod title ((method method-definition) &optional relative-to)
  "Return METHOD's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~{ ~A~^~}~{ ~A~^~}~) method"
    (name method)
    (mapcar #'pretty-specializer (specializers method))
    (qualifiers method)))

(defmethod title ((generic generic-definition) &optional relative-to)
  "Return GENERIC's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~) generic function" (name generic)))

(defmethod title ((expander setf-expander-definition) &optional relative-to)
  "Return setf EXPANDER's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~) setf expander" (name expander)))

;; #### NOTE: no TITLE method for SLOT-DEFINITION

(defmethod title
    ((combination short-combination-definition)&optional relative-to)
  "Return short method COMBINATION's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~) short method combination" (name combination)))

(defmethod title
    ((combination long-combination-definition)&optional relative-to)
  "Return long method COMBINATION's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~) long method combination" (name combination)))

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

(defmethod title ((type type-definition) &optional relative-to)
  "Return TYPE's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~) type" (name type)))

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

(defmethod index ((symbol-macro symbol-macro-definition) &optional relative-to)
  "Render SYMBOL-MACRO's indexing command."
  (declare (ignore relative-to))
  (format t "@symbolmacrosubindex{~(~A~)}@c~%" (escape symbol-macro)))

(defmethod index ((macro macro-definition) &optional relative-to)
  "Render MACRO's indexing command."
  (declare (ignore relative-to))
  (format t "@macrosubindex{~(~A~)}@c~%" (escape macro)))

(defmethod index
    ((compiler-macro compiler-macro-definition) &optional relative-to)
  "Render COMPILER-MACRO's indexing command."
  (declare (ignore relative-to))
  (format t "@compilermacrosubindex{~(~A~)}@c~%" (escape compiler-macro)))

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

(defmethod index ((expander setf-expander-definition) &optional relative-to)
  "Render setf EXPANDER's indexing command."
  (declare (ignore relative-to))
  (format t "@setfexpandersubindex{~(~A~)}@c~%" (escape expander)))

(defmethod index ((slot slot-definition) &optional relative-to)
  "Render SLOT's indexing command."
  (declare (ignore relative-to))
  (format t "@slotsubindex{~(~A~)}@c~%" (escape slot)))

(defmethod index
    ((combination short-combination-definition) &optional relative-to)
  "Render short method COMBINATION's indexing command."
  (declare (ignore relative-to))
  (format t "@shortcombinationsubindex{~(~A~)}@c~%" (escape combination)))

(defmethod index
    ((combination long-combination-definition) &optional relative-to)
  "Render long method COMBINATION's indexing command."
  (declare (ignore relative-to))
  (format t "@longcombinationsubindex{~(~A~)}@c~%" (escape combination)))

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

(defmethod index ((type type-definition) &optional relative-to)
  "Render TYPE's indexing command."
  (declare (ignore relative-to))
  (format t "@typesubindex{~(~A~)}@c~%" (escape type)))

(defmethod reference ((definition definition) &optional relative-to)
  "Render DEFINITION's reference."
  (declare (ignore relative-to))
  (format t "@ref{~A, , @t{~(~A}~)} (~A)~%"
    (escape (anchor-name definition))
    (escape definition)
    (type-name definition)))

(defmethod reference ((function function-definition) &optional relative-to)
  "Render FUNCTION's reference."
  (declare (ignore relative-to))
  (if (function-definition-foreignp function)
      (format t "@t{~(~A}~)~%" (escape function))
    (call-next-method)))

(defmethod reference ((method method-definition) &optional relative-to)
  "Render METHOD's reference."
  (declare (ignore relative-to))
  (if (method-definition-foreignp method)
      (format t "@t{~(~A}~)~%" (escape method))
    (call-next-method)))

(defmethod reference ((generic generic-definition) &optional relative-to)
  "Render GENERIC function's reference."
  (declare (ignore relative-to))
  (if (generic-definition-foreignp generic)
      (format t "@t{~(~A}~)~%" (escape generic))
    (call-next-method)))

(defmethod reference
    ((combination combination-definition) &optional relative-to)
  "Render COMBINATION's reference."
  (declare (ignore relative-to))
  (if (combination-definition-foreignp combination)
      (format t "@t{~(~A}~)~%" (escape combination))
    (call-next-method)))

(defmethod reference ((classoid classoid-definition) &optional relative-to)
  "Render CLASSOID's reference."
  (declare (ignore relative-to))
  (if (classoid-definition-foreignp classoid)
      (format t "@t{~(~A}~)~%" (escape classoid))
    (call-next-method)))

(defmethod document ((constant constant-definition) context)
  "Render CONSTANT's documentation in CONTEXT."
  (render-@defconstant constant context))

(defmethod document ((special special-definition) context)
  "Render SPECIAL variable's documentation in CONTEXT."
  (render-@defspecial special context))

(defmethod document ((symbol-macro symbol-macro-definition) context)
  "Render SYMBOL-MACRO's documentation in CONTEXT."
  (render-@defsymbolmacro symbol-macro context
    (@tableitem "Expansion"
      (format t "@t{~(~A~)}~%"
	(escape
	 (format nil "~S" (macroexpand (definition-symbol symbol-macro))))))))

(defmethod document ((function function-definition) context)
  "Render FUNCTION's documentation in CONTEXT."
  (render-@defun function context))

(defmethod document ((macro macro-definition) context)
  "Render MACRO's documentation in CONTEXT."
  (render-@defmac macro context)
  (when (macro-definition-expander macro)
    (document (macro-definition-expander macro) context)))

(defmethod document ((compiler-macro compiler-macro-definition) context)
  "Render COMPILER-MACRO's documentation in CONTEXT."
  (render-@defcompilermacro compiler-macro context))

(defmethod document ((accessor accessor-definition) context)
  "Render ACCESSOR's documentation in CONTEXT."
  (cond ((and (accessor-definition-writer accessor)
	      (equal (source accessor)
		     (source (accessor-definition-writer accessor)))
	      (equal (docstring accessor)
		     (docstring (accessor-definition-writer accessor))))
	 (render-@defunx
	  accessor (accessor-definition-writer accessor) context))
	(t
	 (call-next-method)
	 (when (accessor-definition-writer accessor)
	   (document (accessor-definition-writer accessor) context))))
  (when (accessor-definition-expander accessor)
    (document (accessor-definition-expander accessor) context)))


(defmethod document ((method method-definition) context)
  "Render METHOD's documentation in CONTEXT."
  (render-@defmethod method context))

(defmethod document ((method accessor-method-definition) context)
  "Render accessor METHOD's documentation in CONTEXT."
  (cond ((and (equal (source method)
		     (source (accessor-method-definition-writer method)))
	      (equal (docstring method)
		     (docstring (accessor-method-definition-writer method))))
	 (render-@defmethodx
	  method (accessor-method-definition-writer method) context))
	(t
	 (call-next-method)
	 (document (accessor-method-definition-writer method) context))))

;; #### PORTME.
(defun render-method-combination (generic)
  "Render GENERIC definition's method combination documentation.
The standard method combination is not rendered."
  (unless (eq (definition-symbol (generic-definition-combination generic))
	      'standard)
    (@tableitem "Method Combination"
      (reference (generic-definition-combination generic))
      (terpri)
      (let ((options (mapcar (lambda (option)
			       (escape (format nil "~(~S~)" option)))
			     (sb-pcl::method-combination-options
			      (sb-mop:generic-function-method-combination
			       (generic-definition-function generic))))))
	(when options
	  (format t "@b{Options:} @t{~A}~{, @t{~A}~}"
	    (first options)
	    (rest options)))))))


(defmethod document ((generic generic-definition) context)
  "Render GENERIC's documentation in CONTEXT."
  (render-@defgeneric generic context
    (render-method-combination generic)
    (let ((methods (generic-definition-methods generic)))
      (when methods
	(@tableitem "Methods"
	  (dolist (method methods)
	    (document method context)))))))

(defmethod document ((accessor generic-accessor-definition) context)
  "Render generic ACCESSOR's documentation in CONTEXT."
  (cond ((and (generic-accessor-definition-writer accessor)
	      (equal (source accessor)
		     (source (generic-accessor-definition-writer accessor)))
	      (eq (definition-symbol (generic-definition-combination accessor))
		  (definition-symbol
		   (generic-definition-combination
		    (generic-accessor-definition-writer accessor))))
	      (equal (sb-pcl::method-combination-options
		      (sb-mop:generic-function-method-combination
		       (generic-definition-function accessor)))
		     (sb-pcl::method-combination-options
		      (sb-mop:generic-function-method-combination
		       (generic-definition-function
			(generic-accessor-definition-writer accessor)))))
	      (equal (docstring accessor)
		     (docstring
		      (generic-accessor-definition-writer accessor))))
	 (render-@defgenericx
	     accessor (generic-accessor-definition-writer accessor) context
	   (render-method-combination accessor)
	   (let ((reader-methods
		   (generic-accessor-definition-methods accessor))
		 (writer-methods
		   (generic-writer-definition-methods
		    (generic-accessor-definition-writer accessor))))
	     (when (or reader-methods writer-methods)
	       (@tableitem "Methods"
		 ;; #### FIXME: this is not the best order to advertise
		 ;; methods. We should group them by specializers instead, so
		 ;; that readers and writers are documented together, just as
		 ;; generic readers and writers.
		 (dolist (method reader-methods)
		   (document method context))
		 (dolist (method writer-methods)
		   (document method context)))))))
	(t
	 (call-next-method)
	 (when (generic-accessor-definition-writer accessor)
	   (document (generic-accessor-definition-writer accessor) context))))
  (when (generic-accessor-definition-expander accessor)
    (document (generic-accessor-definition-expander accessor) context)))

(defmethod document ((expander setf-expander-definition) context)
  "Render setf EXPANDER's documentation in CONTEXT."
  (render-@defsetf expander context))

;; #### NOTE: no DOCUMENT method for SLOT-DEFINITION

;; #### PORTME.
(defmethod document ((combination short-combination-definition) context)
  "Render short method COMBINATION's documentation in CONTEXT."
  (render-@defcombination :short combination context
    (@tableitem "Operator"
      (reference (short-combination-definition-operator combination)))
    (@tableitem "Indentity with one argument"
      (format t "@t{~(~A~)}"
	(sb-pcl::short-combination-identity-with-one-argument
	 (combination-definition-combination combination))))
    (render-references (combination-definition-users combination) "Users")))

(defmethod document ((combination long-combination-definition) context)
  "Render long method COMBINATION's documentation in CONTEXT."
  (render-@defcombination :long combination context
    (render-references (combination-definition-users combination) "Users")))

(defmethod document ((condition condition-definition) context)
  "Render CONDITION's documentation in CONTEXT."
  (render-@defcond condition context))

(defmethod document ((structure structure-definition) context)
  "Render STRUCTURE's documentation in CONTEXT."
  (render-@defstruct structure context))

(defmethod document ((class class-definition) context)
  "Render CLASS's documentation in CONTEXT."
  (render-@defclass class context))

(defmethod document ((type type-definition) context)
  "Render TYPE's documentation in CONTEXT."
  (render-@deftype type context))



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

(defun add-categories-node (parent context status definitions)
  "Add the STATUS DEFINITIONS categories nodes to PARENT in CONTEXT."
  (dolist (category +categories+)
    (let ((category-definitions
	    (category-definitions (first category) definitions)))
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
	:for definitions :in (list
			      (context-external-definitions context)
			      (context-internal-definitions context))
	:unless (zerop (definitions-pool-size definitions))
	  :do (let ((node (add-child definitions-node
			    (make-node
			     :name (format nil "~@(~A~) definitions"
				     status)))))
		(add-categories-node node context status definitions))))


;;; symbol.lisp ends here
