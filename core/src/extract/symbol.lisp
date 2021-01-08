;;; symbol.lisp --- Symbol definitions

;; Copyright (C) 2010-2013, 2017, 2020 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of Declt.

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


;;; Commentary:

;; #### WARNING: currently, we don't fill in the FUNCTION slot for foreign
;; #### funcoid definitions. That's because we don't care, since we only print
;; #### foreign names. There is one exception however: the case of setf
;; #### expanders update functions. See comment in the SOURCE method about
;; #### that. Also, we may need to change this policy globally when we start
;; #### cross-referencing systems for Quickref.


;;; Code:

(in-package :net.didierverna.declt)
(in-readtable :net.didierverna.declt)


;; ==========================================================================
;; Definitions
;; ==========================================================================

;; ----------
;; Categories
;; ----------

;; #### NOTE: when constructing the extract lists of definitions, only the
;; definitions listed in *CATEGORIES* appear. This is because these lists
;; follow the structure of the Definitions chapter in the generated manual.
;; For instance, methods are listed under the corresponding generic function,
;; so they don't represent a category of its own.

;; #### NOTE: the order in *CATEGORIES* is important (see
;; ADD-CATEGORIES-NODE). It conditions the order of appearance of the
;; definitions in the generated manual.

(defparameter *categories*
  '((constant-definition       "constants")
    (special-definition        "special variables")
    (symbol-macro-definition   "symbol macros")
    (macro-definition          "macros")
    (compiler-macro-definition "compiler macros")
    (function-definition       "functions")
    (generic-definition        "generic functions")
    (combination-definition    "method combinations")
    (condition-definition      "conditions")
    (structure-definition      "structures")
    (class-definition          "classes")
    (type-definition           "types"))
  "The list of definition categories.
Each category is of type (TYPE DESCRIPTION-STRING).")


;; ------------------
;; Symbol Definitions
;; ------------------

;; #### FIXME: many things are wrong in this model. Several slots are called
;; #### "access expander", which is misleading because they contain setf
;; #### expander definitions. Some things are called "accessors" but really
;; #### are readers, with back pointers to the corresponding writer
;; #### definitions. Finally, the whole thing is too tied up to the way I
;; #### originally thought a reference manual should be organized. What needs
;; #### to be done is:
;; #### - restore balance between readers and writers (i.e., make accessors
;; ####   orthogonal structures with reader and writer slots, instead of
;; ####   claiming that accessors "inherit" from readers).
;; #### - See about making accessors for macros as well as [generic]
;; ####   functions.
;; #### - Flatten the whole thing apart maybe from slots which indeed do
;; ####   belong to their respective condition / structure / class. In any
;; ####   case, stop having writers belonging to accessors as they are now.
;; #### That way, in the future, people will have the ability to organize
;; #### their manuals as they wish from the provided extraction, and I can for
;; #### example update the nodes structure to reflect what I'm currently
;; #### doing.

;; #### NOTE: writer structures (either regular or generic) don't store the
;; complete function name (setf <name>) but only the original symbol. This, in
;; conjunction with the fact that definitions are sorted by symbol-name,
;; ensures that standalone writers (not associated with readers) are listed in
;; proper lexicographic order regardless of the SETF part of their name
;; (although that part still appears in the documentation).

;; #### FIXME: abstract.
(defclass symbol-definition (definition)
  ((symbol :documentation "The symbol naming this definition."
	   :initarg :symbol :reader definition-symbol))
  (:documentation "The SYMBOL-DEFINITION class.
This is the base class for definitions named by symbols."))

(defun symbol-definition-p (object)
  "Return T if OBJECT is a symbol definition."
  (eq (type-of object) 'symbol-definition))

(defmethod definition-package ((symbol-definition symbol-definition))
  "Return SYMBOL-DEFINITION's home package."
  (symbol-package (definition-symbol symbol-definition)))


(defclass constant-definition (symbol-definition)
  ()
  (:documentation "The class for constant definitions."))

(defun make-constant-definition (symbol)
  "Make a new constant definition for SYMBOL."
  (make-instance 'constant-definition :symbol symbol))


(defclass special-definition (symbol-definition)
  ()
  (:documentation "The class for special variable definitions."))

(defun make-special-definition (symbol)
  "Make a new special variable definition for SYMBOL."
  (make-instance 'special-definition :symbol symbol))


(defclass symbol-macro-definition (symbol-definition)
  ()
  (:documentation "The class for symbol macro definitions."))

(defun make-symbol-macro-definition (symbol)
  "Make a new symbol macro definition for SYMBOL."
  (make-instance 'symbol-macro-definition :symbol symbol))


;; #### FIXME: abstract.
(defclass funcoid-definition (symbol-definition)
  ((funcoid :documentation "The corresponding functional object.
Depending on the exact class, this may be a generic, ordinary, or macro
function."
	    :reader funcoid))
  (:documentation "Base class for functional value definitions."))


(defclass macro-definition (funcoid-definition)
  ;; #### NOTE: you may notice that contrary to (generic) functions, macro
  ;; definitions don't have a WRITER slot. That is because a setf function
  ;; cannot be directly associated with a macro so there's no point in trying
  ;; to concatenate their documentation. See sections 5.1.2.7 and 5.1.2.9 of
  ;; the Hyperref for more information.
  ;; #### NOTE: the reason for an ACCESS-EXPANDER-DEFINITION here is that
  ;; Declt will attempt to concatenate the definitions for FOO and (SETF FOO)
  ;; into a single documentation item when possible. It makes the reference
  ;; manual more readable IMO, but that's assuming that FOO and (SETF FOO) are
  ;; indeed a no-nonsense reader/writer pair of macros...
  ((funcoid :initarg :macro :reader macro) ;; slot overload
   (access-expander-definition
    :documentation "Definition for a setf expander that expands this macro."
    :initform nil :accessor access-expander-definition)
   (update-expander-definition
    :documentation "Definition for a setf expander that expands to this macro."
    :initform nil :accessor update-expander-definition))
  (:documentation "The class for macro definitions."))

(defun make-macro-definition (symbol macro)
  "Make a new MACRO definition for SYMBOL."
  (make-instance 'macro-definition :symbol symbol :macro macro))


(defclass compiler-macro-definition (funcoid-definition)
  ((funcoid ;; slot overload
    :initarg :compiler-macro :reader definition-compiler-macro))
  (:documentation "The class for compiler macro definitions."))

(defun make-compiler-macro-definition (symbol compiler-macro)
  "Make a new COMPILER-MACRO definition for SYMBOL."
  (make-instance 'compiler-macro-definition
    :symbol symbol :compiler-macro compiler-macro))


(defclass function-definition (funcoid-definition)
  ((funcoid :initarg :function :reader definition-function) ;; slot overload
   (update-expander-definition
    :documentation
    "Definition for a setf expander that expands to this function."
    :initform nil :accessor update-expander-definition))
  (:documentation "The class for ordinary function definitions."))

(defun make-function-definition (symbol function &optional foreign)
  "Make a new FUNCTION definition for SYMBOL, optionally FOREIGN."
  (make-instance 'function-definition
    :symbol symbol :function function :foreign foreign))


;; #### FIXME: writer definitions can't have an associated update expander so
;; it's not clean that they have that slot. I need to refine the structures
;; hierarchy.
(defclass writer-definition (function-definition)
  ((reader-definition :documentation "Corresponding reader definition."
		      :initform nil
		      :initarg :reader-definition
		      :accessor reader-definition))
  (:documentation "The class for ordinary writer function definitions."))

(defun writer-definition-p (object)
  "Return T if OBJECT is a writer definition."
  (eq (type-of object) 'writer-definition))

(defun make-writer-definition
    (symbol &rest keys &key function reader-definition foreign)
  "Make a new writer definition for SYMBOL.
Unless FOREIGN, this definition is for FUNCTION and may have a corresponding
READER-DEFINITION."
  (declare (ignore function reader-definition foreign))
  (apply #'make-instance 'writer-definition :symbol symbol keys))


(defclass accessor-definition (function-definition)
  ;; #### NOTE: the reason for a WRITER and an ACCESS-EXPANDER slots here is
  ;; that Declt will attempt to concatenate the definitions for FOO and (SETF
  ;; FOO) into a single documentation item when possible. It makes the
  ;; reference manual more readable IMO, but that's assuming that FOO and
  ;; (SETF FOO) are indeed a no-nonsense reader/writer pair of functions...
  ((writer-definition
    :documentation "Definition for a corresponding writer."
    :initform nil :accessor writer-definition)
   (access-expander-definition
    :documentation "Definition for a setf expander that expands this function."
    :initform nil :accessor access-expander-definition))
  (:documentation "The class for ordinary accessor function definitions."))

(defun accessor-definition-p (object)
  "Return T if OBJECT is an accessor definition."
  (eq (type-of object) 'accessor-definition))

(defun make-accessor-definition (symbol function)
  "Make a new accessor definition for SYMBOL with FUNCTION."
  (make-instance 'accessor-definition :symbol symbol :function function))

;; #### NOTE: inheriting from SYMBOL-DEFINITION here is slightly redundant,
;; since methods always belong to a single generic function, so the symbol is
;; the same. We're still doing it this way in case some people would like to
;; document methods as standalone items, or advertise their name as
;; PACKAGE::SYMBOL, etc.
;; #### FIXME: in the same spirit, I need to add a back pointer to the
;; corresponding generic function definition.
(defclass method-definition (symbol-definition)
  ((method ;; #### NOTE: not filled in for foreign definitions.
    :documentation "The corresponding method object."
    :initarg :method :reader definition-method))
  (:documentation "Base class for method definitions."))

(defun make-method-definition (symbol &rest keys &key method foreign)
  "Make a new METHOD definition for SYMBOL, optionally FOREIGN."
  (declare (ignore method foreign))
  (apply #'make-instance 'method-definition :symbol symbol keys))


(defclass writer-method-definition (method-definition)
  ()
  (:documentation "The class  for writer method definitions."))

(defun make-writer-method-definition (symbol method)
  "Make a new writer method definition for SYMBOL with METHOD."
  (make-instance 'writer-method-definition :symbol symbol :method method))


(defclass accessor-method-definition (method-definition)
  ((writer-definition :documentation "The corresponding writer definition."
		      :initarg :writer-definition :reader writer-definition))
  (:documentation "The class for accessor method definitions."))

(defun accessor-method-definition-p (object)
  "Return T if OBJECT is an accessor method definition."
  (eq (type-of object) 'accessor-method-definition))

(defun make-accessor-method-definition (symbol method writer-definition)
  "Make a new accessor METHOD definition for SYMBOL.
This definition has an corresponding WRITER-DEFINITION."
  (make-instance 'accessor-method-definition
    :symbol symbol :method method :writer-definition writer-definition))


(defclass generic-definition (funcoid-definition)
  ((funcoid :initarg :generic :reader generic) ;; slot overload
   ;; #### NOTE: slots not filled in for foreign definitions.
   (method-definitions
    :documentation "The list of corresponding method definitions."
    :initarg :method-definitions :reader method-definitions)
   (update-expander-definition
    :documentation
    "Definition for a setf expander that expands to this function."
    :initform nil
    :accessor update-expander-definition)
   (combination-definition :documentation "The method combination definition."
			   :accessor combination-definition))
  (:documentation "Class for generic function definitions."))

(defun make-generic-definition
    (symbol &rest keys &key generic method-definitions foreign)
  (declare (ignore generic method-definitions foreign))
  "Make a new generic function definition for SYMBOL.
Unless FOREIGN, this definition is for GENERIC function, and has the
corresponding METHOD-DEFINITIONS."
  (apply #'make-instance 'generic-definition :symbol symbol keys))


;; #### FIXME: generic writer definitions can't have an associated update
;; expander so it's not clean that they have that slot. I need to refine the
;; structures hierarchy.
(defclass generic-writer-definition (generic-definition)
  ((reader-definition :documentation "The corresponding reader definition."
		      :initarg :reader-definition :reader reader-definition))
  (:documentation "Class for generic writer function definitions."))

(defun generic-writer-definition-p (object)
  "Return T if OBJECT is a generic writer definition."
  (eq (type-of object) 'generic-writer-definition))

(defun make-generic-writer-definition
    (symbol
     &rest keys &key generic method-definitions reader-definition foreign)
  (declare (ignore generic method-definitions reader-definition foreign))
  "Make a new generic writer function definition for SYMBOL.
Unless FOREIGN, this definition is for GENERIC function, and has the
corresponding METHOD-DEFINITIONS."
  (apply #'make-instance 'generic-writer-definition :symbol symbol keys))


(defclass generic-accessor-definition (generic-definition)
  ;; #### NOTE: the reason for a WRITER and an ACCESS-EXPANDER slots here is
  ;; that Declt will attempt to concatenate the definitions for FOO and (SETF
  ;; FOO) into a single documentation item when possible. It makes the
  ;; reference manual more readable IMO, but that's assuming that FOO and
  ;; (SETF FOO) are indeed a no-nonsense reader/writer pair of functions...
  ((writer-definition :documentation "The corresponding writer definition."
		      :accessor writer-definition)
   (access-expander-definition
    :documentation
    "Definition for a setf expander that expands this function."
    :initform nil
    :accessor access-expander-definition))
  (:documentation "Class for generic accessor function definitions."))

(defun generic-accessor-definition-p (object)
  "Return T if OBJECT is a generic accessor definition."
  (eq (type-of object) 'generic-accessor-definition))

(defun make-generic-accessor-definition (symbol generic method-definitions)
  "Make a new GENERIC accessor definition for SYMBOL, with METHOD-DEFINITIONS."
  (make-instance 'generic-accessor-definition
    :symbol symbol :generic generic :method-definitions method-definitions))


;; #### FIXME: so no. The UPDATE slot has two different kinds of value, so
;; make two subclasses.
(defclass setf-expander-definition (symbol-definition)
  ((access-definition :documentation "The access functional definition."
		      :initarg :access-definition :accessor access-definition)
   (update :documentation "The update object.
For short forms, it is a macro or (generic) function definition. For long
forms, it's a function."
	   :initarg :update :accessor update))
   (:documentation "Class for setf expander definitions."))

(defun make-setf-expander-definition (symbol access-definition update)
  "Make a new setf expander definition for SYMBOL."
  (make-instance 'setf-expander-definition
    :symbol symbol :access-definition access-definition :update update))


(defclass slot-definition (symbol-definition)
  ((slot :documentation "The corresponding slot object."
	 :initarg :slot :reader slot)
   (reader-definitions :documentation "The slot's reader definitions."
		       :accessor reader-definitions)
   (writer-definitions :documentation "The slot's writer definitions."
		       :accessor writer-definitions))
  (:documentation "The class for slot definitions."))

(defun make-slot-definition (symbol slot)
  "Make a new SLOT definition for SYMBOL."
  (make-instance 'slot-definition :symbol symbol :slot slot))


;; #### FIXME: abstract.
(defclass combination-definition (symbol-definition)
  ;; #### NOTE: slots not filled in for foreign definitions.
  ((combination :documentation "The corresponding combination object."
		:initarg :combination :reader combination)
   (users :documentation
	  "The list of definitions of generic functions using this combination."
	  :accessor users))
  (:documentation "The class for method combination definitions."))


(defclass short-combination-definition (combination-definition)
  ;; #### NOTE: slot not filled in for foreign combinations.
  ((operator-definition
    :documentation "The corresponding operator definition."
    :initarg :operator-definition :accessor operator-definition))
  (:documentation "The class for short method combination definitions."))

(defun short-combination-definition-p (object)
  "Return T if OBJECT is a short combination definition."
  (eq (type-of object) 'short-combination-definition))

(defun make-short-combination-definition
    (symbol &rest keys &key combination operator-definition foreign)
  "Make a new short method combination definition.
Unless FOREIGN, this definition has a corresponding COMBINATION object,
and an OPERATOR-DEFINITION."
  (declare (ignore combination operator-definition foreign))
  (apply #'make-instance 'short-combination-definition :symbol symbol keys))


(defclass long-combination-definition (combination-definition)
  ()
  (:documentation "Class for long method combination definitions."))

(defun long-combination-definition-p (object)
  "Return T if OBJECT is a long combination definition."
  (eq (type-of object) 'long-combination-definition))

(defun make-long-combination-definition
    (symbol &rest keys &key combination foreign)
  "Make a new long method combination definition for SYMBOL.
Unless FOREIGN, this definition has a corresponding COMBINATION object."
  (declare (ignore combination foreign))
  (apply #'make-instance 'long-combination-definition :symbol symbol keys))


;; #### FIXME: abstract.
(defclass classoid-definition (symbol-definition)
  ((slot-definitions :documentation "The classoid's direct slot definitions."
		     :initarg :slot-definitions :reader slot-definitions)
   (method-definitions
    :documentation
    "The list of definitions for direct methods on this classoid."
    :accessor method-definitions)
   (superclassoid-definitions
    :documentation
    "The list of definitions for the classoid's superclassoids."
    :accessor superclassoid-definitions)
   (subclassoid-definitions
    :documentation
    "The list of definitions for the classoid's subclassoids."
    :accessor subclassoid-definitions))
  (:documentation "Base class for classoid definitions.
Conditions, structures, and classes are classoids."))


(defclass condition-definition (classoid-definition)
  ((superclassoid-definitions ;; slot overload
    :accessor supercondition-definitions)
   (subclassoid-definitions ;; slot overload
    :accessor subcondition-definitions))
  (:documentation "The class for condition definitions."))

(defun make-condition-definition
    (symbol &rest keys &key slot-definitions foreign)
  "Make a new condition definition for SYMBOL.
Unless FOREIGN, this definition has a list of SLOT-DEFINTIIONS."
  (declare (ignore slot-definitions foreign))
  (apply #'make-instance 'condition-definition :symbol symbol keys))


(defclass structure-definition (classoid-definition)
  ((superclassoid-definitions ;; slot overload
    :accessor superstructure-definitions)
   (subclassoid-definitions ;; slot overload
    :accessor substructure-definitions))
  (:documentation "The class for structure definitions."))

(defun make-structure-definition
    (symbol &rest keys &key slot-definitions foreign)
  "Make a new structure definition for SYMBOL.
Unless FOREIGN, this definition has a list of SLOT-DEFINTIIONS."
  (declare (ignore slot-definitions foreign))
  (apply #'make-instance 'structure-definition :symbol symbol keys))


(defclass class-definition (classoid-definition)
  ((superclassoid-definitions ;; slot overload
    :accessor superclass-definitions)
   (subclassoid-definitions ;; slot overload
    :accessor subclass-definitions))
  (:documentation "The class for class definitions."))

(defun make-class-definition
    (symbol &rest keys &key slot-definitions foreign)
  "Make a new class definition for SYMBOL.
Unless FOREIGN, this definition has a list of SLOT-DEFINTIIONS."
  (declare (ignore slot-definitions foreign))
  (apply #'make-instance 'class-definition :symbol symbol keys))


(defclass type-definition (symbol-definition)
  ()
  (:documentation "The class for type definitions."))

(defun make-type-definition (symbol)
  "Make a new type definition for SYMBOL."
  (make-instance 'type-definition :symbol symbol))


;; #### PORTME.
(defgeneric lambda-list (object)
  (:documentation "Return OBJECT's lambda-list.")
  (:method ((function function))
    "Return FUNCTION's lambda-list."
    (sb-introspect:function-lambda-list function))
  (:method ((funcoid funcoid-definition))
    "Return FUNCOID's lambda-list."
    (lambda-list (funcoid funcoid)))
  (:method ((expander setf-expander-definition) &aux (update (update expander)))
    "Return setf EXPANDER's lambda-list."
    (sb-introspect:function-lambda-list
     (etypecase update
       (list (cdr update))
       (function update)
       (funcoid-definition (funcoid update)))))
  (:method ((method method-definition))
    "Return METHOD's lambda-list."
    (sb-mop:method-lambda-list (definition-method method)))
  (:method ((type type-definition))
    "Return TYPE's lambda-list."
    (sb-introspect:deftype-lambda-list (definition-symbol type))))

;; #### PORTME.
(defun specializers (method)
  "Return METHOD's specializers."
  (sb-mop:method-specializers (definition-method method)))

;; #### PORTME.
(defun qualifiers (method)
  "Return METHOD's qualifiers."
  (method-qualifiers (definition-method method)))


;; ------------
;; Finalization
;; ------------

;; #### FIXME: this function doesn't seem to be used anywhere else than in the
;; #### finalization process anymore, so the comment below needs action. Also,
;; #### the ERRORP argument seems obsolete (probably for the same reason).
;; #### FIXME: the finalize process needs to find standalone writers and uses
;; this function. However, this function returns all writers; not only
;; standalone ones. This is not normally a problem because at that time, we're
;; resolving heterogeneous accessors so we shouldn't find leaf writers. To be
;; really pedantic, we could check that it is actually the case.
(defgeneric find-definition (name type definitions &optional errorp)
  (:documentation "Find a definition of TYPE for NAME in DEFINITIONS.
If ERRORP, throw an error if not found. Otherwise, just return NIL.")
  (:method (name type definitions
	    &optional errorp
	    &aux (definition
		  (find-if (lambda (definition)
			     (and (eq name (definition-symbol definition))
				  (typep definition type)))
			   definitions)))
    "Default method used for root TYPEs"
    (or definition
	(when errorp
	  (error "No ~A definition found for symbol ~A" type name))))
  (:method
      (name (type (eql 'accessor-definition)) definitions
       &optional errorp
       &aux (definition
	     (find-definition name 'function-definition definitions errorp)))
    "Method used to find accessor definitions."
    (or (when (accessor-definition-p definition)
	  definition)
	(when errorp
	  (error "No accessor definition found for symbol ~A." name))))
  (:method
      (name (type (eql 'writer-definition)) definitions
       &optional errorp
       &aux (definition
	     (find-definition name 'function-definition definitions errorp)))
    "Method used to find writer definitions.
Name must be that of the reader (not the SETF form)."
    (or (typecase definition
	  (writer-definition
	   definition)
	  (accessor-definition
	   ;; #### FIXME: can this be NIL? Shouldn't we check ERRORP as below?
	   (writer-definition definition)))
	(when errorp
	  (error "No writer definition found for symbol ~A." name))))
  (:method
      (name (type (eql 'generic-accessor)) definitions
       &optional errorp
       &aux (definition
	     (find-definition name 'generic-definition definitions errorp)))
    "Method used to find generic accessor definitions."
    (or (when (generic-accessor-definition-p definition)
	  definition)
	(when errorp
	  (error "No generic accessor definition found for symbol ~A." name))))
  (:method
      (name (type (eql 'generic-writer-definition)) definitions
       &optional errorp
       &aux (definition
	     (find-definition name 'generic-definition definitions errorp)))
    "Method used to find generic writer definitions.
Name must be that of the reader (not the SETF form)."
    (or (typecase definition
	  (generic-writer-definition
	   definition)
	  (generic-accessor-definition
	   ;; #### FIXME: can this be NIL? Shouldn't we check ERRORP as below?
	   (writer-definition definition)))
	(when errorp
	  (error "No generic writer definition found for symbol ~A" name)))))

;; #### PORTME.
(defun method-name (method
		    &aux (name (sb-mop:generic-function-name
				(sb-mop:method-generic-function method))))
  "Return METHOD's name.
Return a second value of T if METHOD is a writer method."
  (if (listp name)
      (values (second name) t)
    name))

;; #### NOTE: this function is used only for finding methods specialized on
;; classoids in the finalization process, so it may encounter a foreign
;; generic function.
(defun find-method-definition (method definitions)
  "Find a method definition for METHOD in DEFINITIONS.
Return NIL if not found."
  (multiple-value-bind (name writerp) (method-name method)
    (when-let ((generic (find-definition name 'generic-definition definitions)))
      (if writerp
	(etypecase generic
	  (generic-writer-definition
	   (find method (method-definitions generic) :key #'definition-method))
	  (generic-accessor-definition
	   (find method (method-definitions (writer-definition generic))
		 :key #'definition-method)))
	(find method (method-definitions generic) :key #'definition-method)))))

(defgeneric type-definitions (type definitions)
  (:documentation "Return all definitions of TYPE from DEFINITIONS.")
  (:method (type definitions)
    "Default method used for most types."
    (remove-if-not (lambda (definition) (typep definition type))
	definitions))
  (:method ((type (eql 'setf-expander-definition)) definitions)
    "Method used for setf expanders."
    (mapcan (lambda (definition)
	      ;; #### NOTE: do you see why dropping structures and using mixin
	      ;; classes would help here ? ;-)
	      (cond ((and (eq (type-of definition) 'macro-definition)
			  (access-expander-definition definition))
		     (list (access-expander-definition definition)))
		    ((and (eq (type-of definition) 'function-definition)
			  (accessor-definition-p definition)
			  (access-expander-definition definition))
		     (list (access-expander-definition definition)))
		    ((and (eq (type-of definition) 'generic-definition)
			  (generic-accessor-definition-p definition)
			  (access-expander-definition definition))
		     (list (access-expander-definition definition)))))
      definitions)))

(defun make-slot-definitions (class)
  "Return a list of direct slot definitions for CLASS."
  (mapcar (lambda (slot)
	    (make-slot-definition (sb-mop:slot-definition-name slot) slot))
    (sb-mop:class-direct-slots class)))


;; 3 functions stolen/adapted from Slime:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun boolean-to-feature-expression (value)
    "Convert boolean VALUE to a form suitable for feature testing."
    (if value '(:and) '(:or)))
  ;; #### PORTME.
  (defun sbcl-has-setf-inverse-meta-info ()
    (boolean-to-feature-expression
     ;; going through FIND-SYMBOL since META-INFO was renamed from
     ;; TYPE-INFO in 1.2.10.
     (let ((sym (find-symbol "META-INFO" "SB-C")))
       (and sym
	    (fboundp sym)
	    (funcall sym :setf :inverse ()))))))
;; #### PORTME.
(defun setf-expander-p (symbol)
  "Return whether SYMBOL defines a setf-expander."
  (or
   #+#.(net.didierverna.declt::sbcl-has-setf-inverse-meta-info)
   (sb-int:info :setf :inverse symbol)
   (sb-int:info :setf :expander symbol)))

;; #### PORTME.
(defun make-symbol-definition (symbol type)
  "Make a definition of TYPE for SYMBOL, if any."
  (ecase type
    (constant-definition
     (when (eql (sb-int:info :variable :kind symbol) :constant)
       (make-constant-definition symbol)))
    (special-definition
     (when (eql (sb-int:info :variable :kind symbol) :special)
       (make-special-definition symbol)))
    (symbol-macro-definition
     (when (eql (sb-int:info :variable :kind symbol) :macro)
       (make-symbol-macro-definition symbol)))
    (macro-definition
     (when-let* ((function (macro-function symbol))
		 (macro-definition (make-macro-definition symbol function)))
       (when-let ((expander (setf-expander-p symbol)))
	 (let ((expander-definition
		 (make-setf-expander-definition
		  symbol macro-definition expander)))
	   (setf (access-expander-definition macro-definition)
		 expander-definition)))
       macro-definition))
    (compiler-macro-definition
     (when-let ((function (compiler-macro-function symbol)))
       (make-compiler-macro-definition symbol function)))
    ;; #### NOTE: As mentionned earlier, the WRITER slot in (generic)
    ;; functions helps to attempt concatenation of the reader and writer
    ;; documentation. However, we won't attempt concatenation when the reader
    ;; and writer are of different nature, that is, one a simple function and
    ;; the other a generic one. If any, those cases should be extremely rare
    ;; and it doesn't really make sense to merge heterogeneous documentations
    ;; (for instance, there would be methods for only one of the definitions).
    ;; Besides, regular and generic functions appear in different sections in
    ;; the manual.
    (function-definition
     (let* ((function (when (and (fboundp symbol)
				 (not (macro-function symbol))
				 (not (typep (fdefinition symbol)
					     'generic-function)))
			(fdefinition symbol)))
	    ;; #### NOTE: an accessor's writer definition is created here only
	    ;; if it's also an ordinary function. Cross-references between
	    ;; heterogeneous accessors will be resolved when the definitions
	    ;; are finalized.
	    (writer (let ((writer-name `(setf ,symbol)))
		      (when (fboundp writer-name)
			(fdefinition writer-name))))
	    (ordinary-writer-p
	      (and writer (not (typep writer 'generic-function))))
	    (expander (setf-expander-p symbol)))
       (cond ((and function (or writer expander))
	      (let ((accessor-definition
		      (make-accessor-definition symbol function)))
		(when ordinary-writer-p
		  (let ((writer-definition
			  (make-writer-definition symbol
			    :function writer
			    :reader-definition accessor-definition)))
		    (setf (writer-definition accessor-definition)
			  writer-definition)))
		(when expander
		  (let ((expander-definition
			  (make-setf-expander-definition
			   symbol accessor-definition expander)))
		    (setf (access-expander-definition
			   accessor-definition)
			  expander-definition)))
		accessor-definition))
	     (function
	      (make-function-definition symbol function))
	     (ordinary-writer-p
	      (make-writer-definition symbol :function writer)))))
    (generic-definition
     (let* ((function
	      (when (and (fboundp symbol)
			 (typep (fdefinition symbol) 'generic-function))
		(fdefinition symbol)))
	    ;; #### NOTE: an accessor's writer definition is created here
	    ;; only if it's also a generic function. Cross-references
	    ;; between heterogeneous accessors will be resolved when the
	    ;; definitions are finalized.
	    (writer
	      (let ((writer-name `(setf ,symbol)))
		(when (fboundp writer-name)
		  (fdefinition writer-name))))
	    (generic-writer-p (typep writer 'generic-function))
	    (expander (setf-expander-p symbol)))
       (cond ((and function (or writer expander))
	      (let ((generic-definition
		      (make-generic-accessor-definition
		       symbol
		       function
		       ;; #### NOTE: for a generic accessor function, we store
		       ;; accessor methods in the generic accessor function
		       ;; definition, along with standard methods. Only
		       ;; writer-only methods are stored in the generic writer
		       ;; function definition.
		       (mapcar
			   (lambda (method)
			     (let ((writer-method
				     (and generic-writer-p
					  (equal (cdr (lambda-list writer))
						 (lambda-list function))
					  (find-method
					   writer
					   (method-qualifiers method)
					   ;; #### FIXME: I'm not sure if the
					   ;; first argument (NEW-VALUE) of a
					   ;; writer method always has a
					   ;; specializer of T...
					   (cons t (sb-mop:method-specializers
						    method))
					   nil))))
			       (if writer-method
				 (make-accessor-method-definition
				  symbol
				  method
				  (make-writer-method-definition
				   symbol writer-method))
				 (make-method-definition symbol
							 :method method))))
			 (sb-mop:generic-function-methods function)))))
		(when generic-writer-p
		  (let ((writer-definition
			  (make-generic-writer-definition symbol
			    :generic writer
			    :method-definitions
			    (mapcan
				(lambda (method)
				  (unless
				      (and
				       (equal (cdr (lambda-list writer))
					      (lambda-list function))
				       (find-method
					function
					(method-qualifiers method)
					;; #### NOTE: don't forget to remove
					;; the first (NEW-VALUE) specializer
					;; from the writer method.
					(cdr (sb-mop:method-specializers
					      method))
					nil))
				    (list (make-writer-method-definition
					   symbol method))))
			      (sb-mop:generic-function-methods writer))
			    :reader-definition generic-definition)))
		    (setf (writer-definition generic-definition)
			  writer-definition)))
		(when expander
		  (let ((expander-definition
			  (make-setf-expander-definition
			   symbol generic-definition expander)))
		    (setf (access-expander-definition generic-definition)
			  expander-definition)))
		generic-definition))
	     (function
	      (make-generic-definition symbol
		:generic function
		:method-definitions
		(mapcar (lambda (method)
			  (make-method-definition symbol :method method))
		  (sb-mop:generic-function-methods function))))
	     (generic-writer-p
	      (make-generic-writer-definition symbol
		:generic writer
		:method-definitions
		(mapcar (lambda (method)
			  (make-writer-method-definition symbol method))
		  (sb-mop:generic-function-methods
		   writer)))))))
    ;; #### WARNING: method combinations in CL don't have a real namespace
    ;; (Cf. this blog:
    ;; http://www.didierverna.net/blog/index.php?post/2013/08/16/Lisp-Corner-Cases%3A-Method-Combinations).
    ;; As a consequence, in order to be 100% correct (and also 200% pedantic),
    ;; I should normally document every single generic function's method
    ;; combination separately. However, I will make the assumption that the
    ;; programmer has some sanity and only defines one method combination for
    ;; every name. The corresponding object will be documented like the other
    ;; ones. In generic function documentations, there will be a reference to
    ;; the method combination and only the method combination options will be
    ;; documented there, as they may be generic function specific.
    (combination-definition
     (let* ((method (find-method #'sb-mop:find-method-combination
				 nil
				 `(,(find-class 'generic-function)
				   (eql ,symbol)
				   t)
				 nil))
	    (combination (when method
			   (sb-mop:find-method-combination
			    ;; #### NOTE: we could use any generic function
			    ;; instead of DOCUMENTATION here. Also, NIL
			    ;; options don't matter because they are not
			    ;; advertised as part of the method combination,
			    ;; but as part of the generic functions that use
			    ;; them.
			    #'documentation symbol nil))))
       (when combination
	 (etypecase combination
	   (sb-pcl::short-method-combination
	    (make-short-combination-definition symbol
	      :combination combination))
	   (sb-pcl::long-method-combination
	    (make-long-combination-definition symbol
	      :combination combination))))))
    (condition-definition
     (let ((class (find-class symbol nil)))
       (when (and class (typep class 'sb-pcl::condition-class))
	 (make-condition-definition symbol
	   :slot-definitions (make-slot-definitions class)))))
    (structure-definition
     (let ((class (find-class symbol nil)))
       (when (and class (typep class 'sb-pcl::structure-class))
	 (make-structure-definition symbol
	   :slot-definitions (make-slot-definitions class)))))
    (class-definition
     (let ((class (find-class symbol nil)))
       (when (and class
		  (not (typep class 'sb-pcl::condition-class))
		  (not (typep class 'sb-pcl::structure-class)))
	 (make-class-definition symbol
	   :slot-definitions (make-slot-definitions class)))))
    (type-definition
     (when (eql (sb-int:info :type :kind symbol) :defined)
       (make-type-definition symbol)))))

(defun make-symbol-definitions (symbol)
  "Make and return a list of all categorized definitions for SYMBOL."
  (mapcan (lambda (category
		   &aux (definition
			 (make-symbol-definition symbol (first category))))
	    (when definition (list definition)))
    *categories*))

;; #### PORTME.
(defun slot-property (slot property)
  "Return SLOT definition's PROPERTY value."
  (funcall
   (intern (concatenate 'string "SLOT-DEFINITION-" (symbol-name property))
	   :sb-mop)
   slot))

(defgeneric slot-reader-definitions (slot definitions)
  (:documentation "Return a list of reader definitions for SLOT.")
  (:method (slot definitions)
    "Defaut method for class and condition slots."
    (mapcar
	(lambda (reader-name)
	  (or (find-definition reader-name 'generic-definition definitions)
	      (make-generic-definition reader-name :foreign t)))
      (slot-property slot :readers)))
  ;; #### PORTME.
  (:method ((slot sb-pcl::structure-direct-slot-definition) definitions)
    "Method for structure slots."
    (list
     (let ((reader-name
	     (sb-pcl::slot-definition-defstruct-accessor-symbol slot)))
       (or (find-definition reader-name 'function-definition definitions)
	   (make-generic-definition reader-name :foreign t))))))

(defgeneric slot-writer-definitions (slot definitions)
  (:documentation "Return a list of writer definitions for SLOT.")
  (:method (slot definitions)
    "Default method for class and condition slots."
    (mapcar
	(lambda (writer-name &aux (setfp (listp writer-name)))
	  (cond (setfp
		 ;; A SETF form is identified and stored in the definitions,
		 ;; either as a standalone (toplevel) generic writer, or as
		 ;; part of a generic accessor definition.
		 (setq writer-name (second writer-name))
		 (or (find-definition writer-name 'generic-writer-definition
				      definitions)
		     (make-generic-writer-definition writer-name :foreign t)))
		(t
		 ;; #### FIXME: the comment below sounds like we should
		 ;; perform a CHANGE-CLASS on the generic! But then, we need
		 ;; two kinds of writers: regular ones and setf ones. A non
		 ;; SETF form is stored in the definitions, as a plain generic
		 ;; definition (neither a generic writer, nor a generic
		 ;; accessor) because there's no way to tell that the function
		 ;; is actually a writer (until now).
		 (or (find-definition writer-name 'generic-definition
				      definitions)
		     (make-generic-definition writer-name :foreign t)))))
      (slot-property slot :writers)))
  ;; #### PORTME.
  (:method ((slot sb-pcl::structure-direct-slot-definition) definitions)
    "Method for structure slots."
    (list
     (let ((writer-name
	     (sb-pcl::slot-definition-defstruct-accessor-symbol slot)))
       (or (find-definition writer-name 'writer-definition definitions)
	   (make-writer-definition writer-name :foreign t))))))

;; #### PORTME.
(defgeneric definition-combination-users (definition combination)
  (:documentation "Return a list of definitions using method COMBINATION.
The list may boil down to a generic function definition, but may also contain
both a reader and a writer.")
  ;; #### FIXME: wtf? When do we try to access a combination somewhere it
  ;; doesn't exist? Oh... answer: probably for hybrid accessors. Hopefully,
  ;; this mess will go away soon.
  (:method (definition combination)
    "Default method, for non generic function definitions.
Return nil."
    nil)
  (:method ((definition generic-definition) combination)
    "Method for simple generic and writer definitions."
    (when (eq (sb-pcl::method-combination-type-name
	       (sb-mop:generic-function-method-combination
		(generic definition)))
	      combination)
      (list definition)))
  (:method ((definition generic-accessor-definition) combination)
    "Method for generic accessor definitions."
    (nconc (call-next-method)
	   (definition-combination-users
	    ;; #### NOTE: a null writer is caught by the default method.
	    (writer-definition definition) combination))))

(defun definitions-combination-users (definitions combination)
  "Return a list of all generic DEFINITIONS using method COMBINATION."
  (mapcan (lambda (definition)
	    (definition-combination-users definition combination))
    definitions))

;; #### NOTE: this finalization step is required for two reasons:
;;   1. it makes it easier to handle cross references (e.g. class inheritance)
;;      because at that time, we know that all definitions have been created,
;;   2. it also makes it easier to handle foreign definitions (that we don't
;;      want to add in the definitions list) because at that time, we know
;;      that if a definition doesn't exist in the list, then it is foreign.
;; #### PORTME.
(defun finalize-definitions (definitions)
  "Finalize the definitions in DEFINITIONS.
Currently, this means resolving:
- classes subclasses,
- classes superclasses,
- classes direct methods,
- slots readers,
- slots writers,
- generic functions method combinations,
- method combinations operators (for short ones) and users (for both),
- heterogeneous accessors,
- (generic) functions and macros (short form) setf expanders definitions."
  (labels ((classoids-definitions (classoids type)
	     (mapcar
		 (lambda (name)
		   (or  (find-definition name type definitions)
			(ecase type
			  (class-definition
			   (make-class-definition name :foreign t))
			  (structure-definition
			   (make-structure-definition name :foreign t))
			  (condition-definition
			   (make-condition-definition name :foreign t)))))
	       (reverse (mapcar #'class-name classoids))))
	   (methods-definitions (methods)
	     (mapcar
		 (lambda (method)
		   (or  (find-method-definition method definitions)
			(make-method-definition (method-name method)
						:foreign t)))
	       methods))
	   (compute-combination (generic-definition)
	     (let* ((combination (sb-mop:generic-function-method-combination
				  (generic generic-definition)))
		    (name (sb-pcl::method-combination-type-name combination)))
	       (setf (combination-definition generic-definition)
		     (or (find-definition name 'combination-definition
					  definitions)
			 (if (sb-pcl::short-method-combination-p combination)
			   (make-short-combination-definition name
			     :foreign t)
			   (make-long-combination-definition name
			     :foreign t)))))))
    (dolist (type '(class-definition structure-definition condition-definition))
      (dolist (definition (type-definitions type definitions))
	(let ((class (find-class (definition-symbol definition))))
	  (setf (superclassoid-definitions definition)
		(classoids-definitions
		 (sb-mop:class-direct-superclasses class)
		 type))
	  (setf (subclassoid-definitions definition)
		(classoids-definitions
		 (sb-mop:class-direct-subclasses class)
		 type))
	  (setf (method-definitions definition)
		(methods-definitions
		 (sb-mop:specializer-direct-methods class)))
	  (dolist (slot-definition (slot-definitions definition))
	    (setf (reader-definitions slot-definition)
		  (slot-reader-definitions (slot slot-definition) definitions))
	    (setf (writer-definitions slot-definition)
		  (slot-writer-definitions (slot slot-definition) definitions))))))
    (dolist (generic-definition
	     (type-definitions 'generic-definition definitions))
      (compute-combination generic-definition)
      (when (and (generic-accessor-definition-p generic-definition)
		 (writer-definition generic-definition))
	(compute-combination (writer-definition generic-definition))))
    (dolist (combination-definition
	     (type-definitions 'short-combination-definition definitions))
      (let ((operator (sb-pcl::short-combination-operator
		       (combination combination-definition))))
	(setf (operator-definition combination-definition)
	      (or (find-definition operator 'function-definition definitions)
		  (find-definition operator 'macro-definition definitions)
		  ;; #### NOTE: a foreign operator is not necessarily a
		  ;; regular function. However, since we don't actually
		  ;; document those (only print their name), we can just use a
		  ;; function definition here (it's out of laziness).
		  (make-function-definition operator t))))
      (setf (users combination-definition)
	    (definitions-combination-users
	     definitions (definition-symbol combination-definition))))
    (dolist (combination-definition
	     (type-definitions 'long-combination-definition definitions))
      (setf (users combination-definition)
	    (definitions-combination-users
	     definitions (definition-symbol combination-definition))))
    (dolist (accessor-definition
	     (type-definitions 'accessor-definition definitions))
      (unless (writer-definition accessor-definition)
	(setf (writer-definition accessor-definition)
	      (find-definition (definition-symbol accessor-definition)
			       'generic-writer-definition definitions))))
    (dolist (writer-definition
	     (type-definitions 'writer-definition definitions))
      (assert (null (reader-definition writer-definition)))
      (setf (reader-definition writer-definition)
	    (find-definition (definition-symbol writer-definition)
			     'generic-accessor-definition definitions)))
    (dolist (accessor-definition
	     (type-definitions 'generic-accessor-definition definitions))
      (unless (writer-definition accessor-definition)
	(setf (writer-definition accessor-definition)
	      (find-definition (definition-symbol accessor-definition)
			       'writer-definition definitions))))
    (dolist (writer-definition
	     (type-definitions 'generic-writer-definition definitions))
      (assert (null (reader-definition writer-definition)))
      (setf (reader-definition writer-definition)
	    (find-definition (definition-symbol writer-definition)
			     'accessor-definition definitions)))
    ;; At that point, a short form setf expander definition contains a symbol
    ;; naming the update object. We now need to transform that into an actual
    ;; (and possibly foreign) definition.
    ;; #### FIXME: this is bad because we traverse long forms as
    ;; well, but this will be fixed when adding to setf expander
    ;; subclasses.
    (dolist (expander (type-definitions 'setf-expander-definition definitions))
      (let ((name (update expander)))
	(when (symbolp name)
	  (let ((update-definition
		  (or (find-definition name 'function-definition definitions)
		      (find-definition name 'generic-definition definitions)
		      (find-definition name 'macro-definition definitions)
		      ;; #### NOTE: a foreign expander is not necessarily a
		      ;; regular function. However, since we don't actually
		      ;; document those (only print their name), we can just
		      ;; use a function definition here (it's out of
		      ;; laziness).
		      ;; #### FIXME: is using FDEFINITION correct? Or do we
		      ;; risk missing a closure or something like that? Also,
		      ;; see comment at the top of the file, and in the SOURCE
		      ;; method, about the need to fill the FUNCTION slot.
		      (make-function-definition
		       name (fdefinition name) t))))
	    (setf (update-expander-definition update-definition)
		  expander)
	    (setf (update expander) update-definition)))))))



;; ==========================================================================
;; Extraction Protocols
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
    (definition type &key (name (definition-symbol definition)))
  "Return DEFINITION's source for TYPE."
  (when-let ((defsrc (car (sb-introspect:find-definition-sources-by-name
			   name type))))
    (sb-introspect:definition-source-pathname defsrc)))

;; #### PORTME.
(defun definition-source (object)
  "Return OBJECT's definition source."
  (when-let ((defsrc (sb-introspect:find-definition-source object)))
    (sb-introspect:definition-source-pathname defsrc)))

(defmethod source ((constant constant-definition))
  "Return CONSTANT's definition source."
  (definition-source-by-name constant :constant))

(defmethod source ((special special-definition))
  "Return SPECIAL's definition source."
  (definition-source-by-name special :variable))

(defmethod source ((symbol-macro symbol-macro-definition))
  "Return SYMBOL-MACRO's definition source."
  (definition-source-by-name symbol-macro :symbol-macro))

(defmethod source ((funcoid funcoid-definition))
  "Return FUNCOID's definition source."
  (definition-source (funcoid funcoid)))

(defmethod source
    ((expander setf-expander-definition)  &aux (update (update expander)))
  ;; #### NOTE: looking at how sb-introspect does it, it seems that the
  ;; "source" of a setf expander is the source of the function object. For
  ;; long forms, this should be OK. For short forms however, what we get is
  ;; the source of the update function, which may be different from where
  ;; DEFSETF was called, hence incorrect. There is an additional problem when
  ;; the update function is foreign: we don't normally fill in the FUNCTION
  ;; slot in foreign funcoid definitions because we don't care (we only print
  ;; their names). In the case of setf expanders however, we need to do so
  ;; because Declt will try to find the definition source for it, and will
  ;; attempt to locate the source of the foreign function. This triggered a
  ;; bug in a previous version (with the package cl-stdutils, which uses
  ;; RPLACA as an update function for the stdutils.gds::vknode-value
  ;; expander).
  (etypecase update
    (list       (definition-source (cdr update)))
    (function   (definition-source update))
    (symbol-definition (source update))))

(defmethod source ((method method-definition))
  "Return METHOD's definition source."
  (definition-source (definition-method method)))

;; #### NOTE: no SOURCE method for SLOT-DEFINITION.

(defmethod source ((combination combination-definition))
  "Return method COMBINATION's definition source."
  (definition-source-by-name combination :method-combination))

(defmethod source ((condition condition-definition))
  "Return CONDITION's definition source."
  (definition-source-by-name condition :condition))

(defmethod source ((structure structure-definition))
  "Return STRUCTURE's definition source."
  (definition-source-by-name structure :structure))

(defmethod source ((class class-definition))
  "Return CLASS's definition source."
  (definition-source-by-name class :class))

(defmethod source ((type type-definition))
  "Return TYPE's definition source."
  (definition-source-by-name type :type))


;; ------------------
;; Docstring protocol
;; ------------------

(defmethod docstring ((constant constant-definition))
  "Return CONSTANT's docstring."
  (documentation (definition-symbol constant) 'variable))

(defmethod docstring ((special special-definition))
  "Return SPECIAL variable's docstring."
  (documentation (definition-symbol special) 'variable))

;; #### NOTE: normally, we shouldn't have to define this because the DOCUMENT
;; method on symbol macros should just not try to get the documentation.
;; However, we do because it allows us to reuse existing code, notably
;; RENDER-@DEFVAROID and hence RENDER-DEFINITION-CORE, and perform the same
;; stuff as for constants and variables.
(defmethod docstring ((symbol-macro symbol-macro-definition))
  "Return NIL because symbol macros don't have a docstring."
  (declare (ignore symbol-macro))
  nil)

(defmethod docstring ((funcoid funcoid-definition))
  "Return FUNCOID's docstring."
  (documentation (definition-symbol funcoid) 'function))

(defmethod docstring ((compiler-macro compiler-macro-definition))
  "Return COMPILER-MACRO's docstring."
  (documentation (definition-symbol compiler-macro) 'compiler-macro))

(defmethod docstring ((writer writer-definition))
  "Return WRITER's docstring."
  (documentation `(setf ,(definition-symbol writer)) 'function))

(defmethod docstring ((method method-definition))
  "Return METHOD's docstring."
  (documentation (definition-method method) t))

(defmethod docstring ((writer generic-writer-definition))
  "Return generic WRITER's docstring."
  (documentation `(setf ,(definition-symbol writer)) 'function))

(defmethod docstring ((expander setf-expander-definition))
  "Return setf EXPANDER's docstring."
  (documentation (definition-symbol expander) 'setf))

;; #### PORTME.
(defmethod docstring ((slot slot-definition))
  "Return SLOT's docstring."
  (sb-pcl::%slot-definition-documentation (slot slot)))

(defmethod docstring ((combination combination-definition))
  "Return method COMBINATION's docstring."
  (documentation (definition-symbol combination) 'method-combination))

(defmethod docstring ((classoid classoid-definition))
  "Return CLASSOID's docstring."
  (documentation (definition-symbol classoid) 'type))

(defmethod docstring ((type type-definition))
  "Return TYPE's docstring."
  (documentation (definition-symbol type) 'type))


;; ------------------
;; Type name protocol
;; ------------------

(defmethod type-name ((constant constant-definition))
  "Return \"constant\""
  "constant")

(defmethod type-name ((special special-definition))
  "Return \"special variable\""
  "special variable")

(defmethod type-name ((symbol-macro symbol-macro-definition))
  "Return \"symbol macro\""
  "symbol macro")

(defmethod type-name ((macro macro-definition))
  "Return \"macro\""
  "macro")

(defmethod type-name ((compiler-macro compiler-macro-definition))
  "Return \"compiler macro\""
  "compiler macro")

(defmethod type-name ((function function-definition))
  "Return \"function\""
  "function")

(defmethod type-name ((generic generic-definition))
  "Return \"generic function\""
  "generic function")

(defmethod type-name ((method method-definition))
  "Return \"method\""
  "method")

(defmethod type-name ((expander setf-expander-definition))
  "Return \"setf expander\""
  "setf expander")

;; #### NOTE: no TYPE-NAME method for SLOT-DEFINITION

(defmethod type-name ((combination short-combination-definition))
  "Return \"short method combination\"."
  "short method combination")

(defmethod type-name ((combination long-combination-definition))
  "Return \"long method combination\"."
  "long method combination")

(defmethod type-name ((condition condition-definition))
  "Return \"condition\""
  "condition")

(defmethod type-name ((structure structure-definition))
  "Return \"structure\""
  "structure")

(defmethod type-name ((class class-definition))
  "Return \"class\""
  "class")

(defmethod type-name ((type type-definition))
  "Return \"type\""
  "type")

;;; symbol.lisp ends here
