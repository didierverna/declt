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

;; #### WARNING: Declt is currently SBCL only. This has some implications on
;; the design of the symbol definitions below. For example, SBCL implements
;; slot writers for structures as setf functions, whereas the standard allows
;; for them to be defined as setf expanders. Structures are classes, and so
;; are conditions, etc. If we ever port Declt to other Lisps, this kind of
;; details could impact the design of the symbol definitions hierarchy
;; implemented below, as well as their creation and finalization processes.


;;; Code:

(in-package :net.didierverna.declt)
(in-readtable :net.didierverna.declt)


;; #### NOTE: SB-INTROSPECT:FIND-DEFINITION-SOURCES-BY-NAME may return
;; multiple sources (e.g. if we were to ask it for methods) so we take the
;; first one. This is okay because we actually use it only when there can be
;; only one definition source.
;; #### PORTME.
(defun definition-source-by-name
    (definition type &key (name (name definition)))
  "Return DEFINITION's source for TYPE."
  (when-let (sources (sb-introspect:find-definition-sources-by-name name type))
    (sb-introspect:definition-source-pathname (first sources))))




;; ==========================================================================
;; Symbol Definition Basics
;; ==========================================================================

(defabstract symbol-definition (definition)
  ((symbol :documentation "The symbol naming this definition."
	   :initarg :symbol :reader definition-symbol)
   (package-definition :documentation "The corresponding package definition."
		       :initform nil :accessor package-definition))
  (:documentation "Abstract root class for all definitions named by symbols."))

(defun symbol-definition-p (object)
  "Return T if OBJECT is a symbol definition."
  (eq (type-of object) 'symbol-definition))

(defmethod name ((definition symbol-definition))
  "Return symbol DEFINITION's symbol."
  (definition-symbol definition))

(defun publicp (definition)
  "Return T is DEFINITION is public.
A definition is public when the symbol naming it is exported from its home
package."
  (member (definition-symbol definition)
	  (external-symbols (package-definition definition))))




;; ==========================================================================
;; Varoids
;; ==========================================================================

(defabstract varoid-definition (symbol-definition)
  ()
  (:documentation "Abstract root class for simply valued symbol definitions.
These are constants, special variables, and symbol macros."))



;; ---------
;; Variables
;; ---------

(defabstract variable-definition (varoid-definition)
  ()
  (:documentation "Abstract root class for constant and special variables."))

(defmethod docstring ((definition variable-definition))
  "Return variable DEFINITION's docstring."
  (documentation (definition-symbol definition) 'variable))



;; Constants

(defclass constant-definition (variable-definition)
  ()
  (:documentation "The class of constant definitions."))

(defun make-constant-definition (symbol)
  "Make a new constant definition for SYMBOL."
  (make-instance 'constant-definition :symbol symbol))

(defmethod source-pathname ((definition constant-definition))
  "Return constant DEFINITION's source pathname."
  (definition-source-by-name definition :constant))



;; Special variables

(defclass special-definition (varoid-definition)
  ()
  (:documentation "The class of special variable definitions."))

(defun make-special-definition (symbol)
  "Make a new special variable definition for SYMBOL."
  (make-instance 'special-definition :symbol symbol))

(defmethod source-pathname ((definition special-definition))
  "Return special DEFINITION's source pathname."
  (definition-source-by-name definition :variable))



;; -------------
;; Symbol macros
;; -------------
(defclass symbol-macro-definition (varoid-definition)
  ()
  (:documentation "The class of symbol macro definitions."))

(defun make-symbol-macro-definition (symbol)
  "Make a new symbol macro definition for SYMBOL."
  (make-instance 'symbol-macro-definition :symbol symbol))

(defmethod source-pathname ((definition symbol-macro-definition))
  "Return symbol macro DEFINITION's source pathname."
  (definition-source-by-name definition :symbol-macro))

;; #### TODO: implement the trick of putting a symbol macro docstring in the
;; symbol's plist. This will make the note below obsolete.
(defmethod docstring ((definition symbol-macro-definition))
  "Return NIL (symbol macros don't have a docstring)."
  (declare (ignore definition))
  nil)




;; ==========================================================================
;; Funcoids
;; ==========================================================================

(defabstract funcoid-definition (symbol-definition)
  ((object :reader funcoid)) ;; slot overload
  (:documentation "Abstract root class for functional definitions.
These are (compiler) macros, (generic) functions, methods, setf expanders,
method combinations, and types."))

(defgeneric lambda-list (definition)
  (:documentation "Return DEFINITION's lambda-list.")
  ;; #### PORTME.
  (:method ((definition funcoid-definition))
    "Return funcoid DEFINITION's function lambda-list.
This is the default method."
    (sb-introspect:function-lambda-list (funcoid definition))))

(defabstract setf-mixin ()
  ()
  (:documentation "Mixin for setf funcoid definitions.
This mixin should be put before the funcoid superclass."))

(defmethod name ((definition setf-mixin))
  "Return the list (setf <setf mixin DEFINITION's symbol>)."
  (list 'setf (definition-symbol definition)))

(defabstract expander-mixin ()
  ((expander-for
    :documentation "A setf expander definition for this funcoid, or NIL.
This is the definition of a setf expander that expands forms identical to this
funcoid's signature. There can be only one. Note that the Common Lisp standard
does not impose any actual relation between the setf expander and its
access-fn. In fact, the access-fn may not even exist at all. However, if it
does, it is very likely that it is a reader for the place updated by this setf
expander."
    :initform nil :accessor expander-for)
   (expanders-to
    :documentation "The list of setf expander definitions to this funcoid.
This is a list of definitions for short form setf expanders that have this
funcoid as their update-fn. There might be more than one."
    :accessor expanders-to))
  (:documentation "Mixin class for funcoids relatable to setf expanders.
These are (generic) functions and macros. A funcoid is relatable to a setf
expander when its signature is the same as that of an access-fn, or when a
short form setf expander expands to it (i.e., it has this funcoid as its
update-fn)."))

(defabstract accessor-mixin ()
  ((slot-definition :documentation "The corresponding slot definition."
		    :initarg :slot-definition :reader slot-definition))
  (:documentation "Mixin class for accessor definitions.
An accessor is a funcoid which reads or writes a slot in a classoid.
More specifically, these are ordinary functions for structure slots,
and methods for classes or conditions slots."))



;; ---------------
;; Simple funcoids
;; ---------------

;; Macros
(defclass macro-definition (funcoid-definition expander-mixin)
  ((object :initarg :macro :reader macro)) ;; slot overload
  (:documentation "The class of macro definitions."))

(defun make-macro-definition (symbol macro &optional foreign)
  "Make a new MACRO definition for SYMBOL."
  (make-instance 'macro-definition
    :symbol symbol :macro macro :foreign foreign))



;; Compiler macros
(defclass compiler-macro-definition (funcoid-definition)
  ((object ;; slot overload
    :initarg :compiler-macro :reader definition-compiler-macro))
  (:documentation "The class of compiler macro definitions."))

(defmethod docstring ((definition compiler-macro-definition))
  "Return compiler macro DEFINITION's docstring."
  (documentation (definition-symbol definition) 'compiler-macro))

(defclass setf-compiler-macro-definition (setf-mixin compiler-macro-definition)
  ()
  (:documentation "The class of setf compiler macro definitions."))

(defmethod docstring ((definition setf-compiler-macro-definition))
  "Return setf compiler macro DEFINITION's docstring."
  (documentation `(setf ,(definition-symbol definition)) 'compiler-macro))

(defun make-compiler-macro-definition (symbol compiler-macro &optional setf)
  "Make a new COMPILER-MACRO definition for SYMBOL."
  (make-instance (if setf
		   'compiler-macro-definition
		   'setf-compiler-macro-definition)
    :symbol symbol :compiler-macro compiler-macro))



;; Types
(defclass type-definition (funcoid-definition)
  ()
  (:documentation "The class of type definitions."))

(defun make-type-definition (symbol)
  "Make a new type definition for SYMBOL."
  (make-instance 'type-definition :symbol symbol))

(defmethod source-pathname ((definition type-definition))
  "Return type DEFINITION's source pathname."
  (definition-source-by-name definition :type))

(defmethod docstring ((definition type-definition))
  "Return type DEFINITION's docstring."
  (documentation (definition-symbol definition) 'type))

;; #### PORTME.
(defmethod lambda-list ((definition type-definition))
  "Return type DEFINITION's type lambda-list."
  (sb-introspect:deftype-lambda-list (definition-symbol definition)))



;; --------------
;; Setf expanders
;; --------------

(defabstract %expander-definition (setf-mixin funcoid-definition)
  ((object :initarg :expander :reader expander) ;; slot overload
   (access-definition
    :documentation
    "A corresponding access-fn definition, or NIL.
If it exists, it's a definition for a function or macro with the same
signature as that of the expander's access-fn. Note that the Common Lisp
standard does not impose any actual relation between the setf expander and its
access-fn. In fact, the access-fn may not even exist at all. However, if it
does, it is very likely that it is a reader for the place updated by this setf
expander."
    :initform nil :accessor access-definition))
  (:documentation "Abstract root class for setf expander definitions."))

(defmethod docstring ((definition %expander-definition))
  "Return setf expander DEFINITION's docstring."
  (documentation (definition-symbol definition) 'setf))


(defclass short-expander-definition (%expander-definition)
  ((object :reader update-fn-name) ;; slot overload
   (update-definition
    :documentation "The corresponding update-fn definition.
This is a function or macro definition. It always exists."
    :initform nil :accessor update-definition))
  (:documentation "The class of short form setf expanders definitions.
Short form setf expanders simply expand to a globally defined function or
macro."))

(defmethod source-pathname ((definition short-expander-definition))
  "Return the source pathname of short setf expander DEFINITION's update-fn."
  ;; #### NOTE: looking at how sb-introspect does it, it seems that the
  ;; "source" of a setf expander is the source of the function object. For
  ;; long forms, this should be OK. For short forms however, what we get is
  ;; the source of the update function, which may be different from where
  ;; DEFSETF was called, hence incorrect.
  (object-source-pathname (fdefinition (update-fn-name definition))))

;; #### PORTME.
(defmethod lambda-list ((definition short-expander-definition))
  "Return the \"butlast\" lambda-list of setf expander DEFINITION's update-fn.
This is because short form setf expanders pass the new value as the last
argument to their update-fn."
  (butlast (sb-introspect:function-lambda-list
	    (fdefinition (update-fn-name definition)))))


(defclass long-expander-definition (%expander-definition)
  ()
  (:documentation "The class of long form setf expanders definitions.
This class is shared by expanders created with either the long form of
DEFSETF, or DEFINE-SETF-EXPANDER."))

(defmethod source-pathname ((definition long-expander-definition)
			    &aux (expander (expander definition)))
  "Return the source pathname of long setf expander DEFINITION's function."
  (object-source-pathname
   (etypecase expander
     (list (cdr expander))
     (function expander))))

;; #### PORTME.
(defmethod lambda-list ((definition long-expander-definition)
			&aux (expander (expander definition)))
  "Return long setf expander DEFINITION's expander function's lambda-list."
  (etypecase expander
    (list (sb-introspect:function-lambda-list (cdr expander)))
    (function (sb-introspect:function-lambda-list expander))))


(defun make-expander-definition (symbol expander)
  "Make a new setf EXPANDER definition for SYMBOL."
  (make-instance (if (symbolp expander)
		   'short-expander-definition
		   'long-expander-definition)
    :symbol symbol :expander expander))



;; -------------------------------
;; (Generic) functions and methods
;; -------------------------------

;; #### NOTE: only basic function definitions are created. Reader and writer
;; definitions are created during the finalization process by upgrading the
;; class of the concerned definitions. The same goes for methods.

(defabstract %function-definition (funcoid-definition)
  ((object :initarg :function :reader definition-function)) ;; slot overload
  (:documentation "Abstract root class for functions."))

;; #### NOTE: this is a general constructor used in MAKE-SYMBOLS-DEFINITIONS.
;; It is used to create both ordinary and generic functions. In the case of
;; generic functions, both SYMBOL and SETF could be deduced from the generic
;; function object, but that information has already been figured out anyway.
#i(make-function-definition 2)
(defun make-function-definition (symbol function &key setf foreign)
  "Make a new FUNCTION definition for (SETF) SYMBOL, possibly FOREIGN.
The concrete class of the new definition depends on the kind of FUNCTION, and
whether it is a SETF one."
  (make-instance
      (typecase function
	(generic-function
	 (if setf 'generic-setf-definition 'generic-definition))
	(otherwise
	 (if setf 'setf-function-definition 'function-definition)))
    :symbol symbol :function function :foreign foreign))



;; Ordinary functions
(defabstract ordinary-function-definition (%function-definition)
  ()
  (:documentation "Abstract root class for ordinary functions."))

(defclass function-definition (ordinary-function-definition expander-mixin)
  ()
  (:documentation "The class of ordinary, non-setf function definitions."))

(defclass setf-function-definition
    (setf-mixin ordinary-function-definition)
  ()
  (:documentation "The class of ordinary setf function definitions."))

(defclass reader-definition (function-definition accessor-mixin)
  ()
  (:documentation "The class of ordinary reader definitions.
An ordinary reader is an ordinary function that reads a slot in a
structure."))

;; #### WARNING: see comment at the top of the file.
(defclass writer-definition (setf-function-definition accessor-mixin)
  ()
  (:documentation "The class of ordinary writer definitions.
An ordinary writer is an ordinary function that writes a slot in a
structure."))



;; Methods

;; #### PORTME.
(defun method-name (method
		    &aux (name (sb-mop:generic-function-name
				(sb-mop:method-generic-function method))))
  "Return METHOD's canonical name.
Return a second value of T if METHOD is in fact a SETF one."
  (if (listp name)
    (values (second name) t)
    name))

(defabstract %method-definition (funcoid-definition)
  ((object :initarg :method :reader definition-method) ;; slot overload
   (generic-definition :documentation "The corresponding generic definition."
		       :initarg :generic-definition))
  (:documentation "Abstract root class for method definitions."))

;; #### PORTME.
(defmethod lambda-list ((definition %method-definition))
  "Return method DEFINITION's method lambda-list."
  (sb-mop:method-lambda-list (definition-method definition)))


(defclass method-definition (%method-definition)
  ()
  (:documentation "The class of non-setf method definitions."))

(defclass setf-method-definition (setf-mixin %method-definition)
  ()
  (:documentation "The class of setf method definitions."))

(defun make-method-definition (method definition &optional foreign)
  "Make a new METHOD definition for generic DEFINITION, possibly FOREIGN.
The concrete class of the new definition depends on whether it is a SETF one."
  (multiple-value-bind (symbol setf)
      (method-name method)
    (make-instance (if setf 'setf-method-definition 'method-definition)
      :symbol symbol :method method :generic-definition definition
      :foreign foreign)))

;; #### NOTE: the situation for readers and writers methods (on class and
;; condition slots) is different from that of ordinary readers and writers (on
;; structure slots). Indeed, an :accessor specification will create a setf
;; function, but a :writer specification allows you to do whatever you like,
;; not necessarily a setf form, so the hierarchy needs to be a bit different.

(defclass reader-method-definition (method-definition accessor-mixin)
  ()
  (:documentation "The class of reader method definitions.
A reader method is a method that reads a slot in a class or condition."))

(defabstract %writer-method-definition (accessor-mixin)
  ()
  (:documentation "Abstract root class for writer method definitions."))

(defclass writer-method-definition
    (method-definition %writer-method-definition)
  ()
  (:documentation "The class of non-setf writer method definitions.
A non-setf writer method is a non-setf method that writes a slot in a class
or a condition."))

(defclass setf-writer-method-definition
    (setf-method-definition %writer-method-definition)
  ()
  (:documentation "The class of setf writer method definitions.
A setf writer method is a setf method that writes a slot in a class
or a condition."))



;; Method combinations

;; #### NOTE: the root class is not abstract, because it is used for foreign
;; definitions.

(defclass combination-definition (funcoid-definition)
  ((object :initarg :combination :reader combination) ;; slot overload
   (user-definitions
    :documentation
    "The list of corresponding method combination user definitions.
This is a list of generic function definitions for generic functions using
this combination."
    :accessor user-definitions))
  (:documentation "Root class for method combination definitions."))

;; #### WARNING: the CLHS specifies that the :operator argument to
;; DEFINE-METHOD-COMBINATION is an /operator/, but later on claims that it is
;; a /symbol/. Experimentation seems to demonstrate that it must be a symbol.
;; #### NOTE: even the short form is a funcoid, because of the optional ORDER
;; option which defaults to :most-specific-first.
(defclass short-combination-definition (combination-definition)
  ((operator-definition :documentation "The corresponding operator definition."
			:initform nil :accessor operator-definition))
  (:documentation "The class of short method combination definitions."))

(defclass long-combination-definition (combination-definition)
  ()
  (:documentation "Class for long method combination definitions."))

;; #### PORTME.
(defun make-combination-definition (symbol combination &optional foreign)
  "Make a new method COMBINATION definition for SYMBOL, possibly FOREIGN.
The concrete class of the new definition depends on the COMBINATION type."
  (make-instance
      (etypecase combination
	(sb-pcl::short-method-combination 'short-combination-definition)
	(sb-pcl::long-method-combination 'long-combination-definition)
	;; this one had better be foreign...
	(sb-pcl::standard-method-combination 'combination-definition))
    :symbol symbol :combination combination :foreign foreign))



;; Generic functions

;; #### TODO: we could think of creating classes of reader and writer generic
;; functions. The condition to upgrade the class would be that all methods are
;; indeed readers / writers.

(defabstract generic-function-definition (%function-definition)
  ((object :initarg :generic :reader generic) ;; slot overload
   (method-definitions
    :documentation "The list of corresponding method definitions."
    :initform nil :accessor method-definitions)
   (combination-definition
    :documentation "The corresponding method combination definition."
    :initform nil :accessor combination-definition))
  (:documentation "Abstract root class for generic function definitions."))

(defmethod initialize-instance :after
    ((definition generic-function-definition) &key foreign)
  "Create all generic DEFINTION's method definitions, unless FOREIGN."
  (unless foreign
    (setf (method-definitions definition)
	  (mapcar (lambda (method)
		    (make-method-definition method definition))
	    (sb-mop:generic-function-methods (generic definition))))))

(defclass generic-definition (generic-function-definition expander-mixin)
  ()
  (:documentation "The class of non-setf, generic function definitions."))

(defclass generic-setf-definition (setf-mixin generic-function-definition)
  ()
  (:documentation "The class of ordinary setf function definitions."))




;; ==========================================================================
;; Classoids
;; ==========================================================================

;; -----
;; Slots
;; -----

;; #### NOTE: structure slots only have one associated reader / writer, so
;; it's slightly overkill to use a list of those. On the other hand, it allows
;; the documentation rendering code to be applicable to all classoids.
(defclass slot-definition (symbol-definition)
  ((object :initarg :slot :reader slot) ;; slot overload
   (classoid-definition :documentation "The corresponding classoid definition."
			:initarg :classoid-definition
			:reader classoid-definition)
   (reader-definitions :documentation "The list of slot reader definitions."
		       :initform nil :accessor reader-definitions)
   (writer-definitions :documentation "The list of slot writer definitions."
		       :initform nil :accessor writer-definitions))
  (:documentation "The class of slot definitions."))

;; #### PORTME.
(defun make-slot-definition (slot definition &optional foreign)
  "Make a new SLOT definition for classoid DEFINITION."
  (make-instance 'slot-definition
    :symbol (sb-mop:slot-definition-name slot)
    :slot slot
    :classoid-definition definition
    :foreign foreign))

;; #### PORTME.
(defmethod docstring ((definition slot-definition))
  "Return slot DEFINITION's docstring."
  (sb-pcl::%slot-definition-documentation (slot definition)))



;; -----------------------------------
;; Conditions, structures, and classes
;; -----------------------------------

;; #### FIXME: typed structures are a whole kind of different beast and are
;; not supported yet.

(defabstract classoid-definition (symbol-definition)
  ((object :initarg :classoid :reader classoid) ;; slot overload
   (superclassoid-definitions
    :documentation "The list of corresponding direct superclassoid definitions."
    :accessor superclassoid-definitions)
   (subclassoid-definitions
    :documentation "The list of corresponding direct subclassoid definitions."
    :accessor subclassoid-definitions)
   (slot-definitions
    :documentation "The list of corresponding direct slot definitions."
    :initform nil :accessor slot-definitions)
   (method-definitions
    :documentation "The list of corresponding direct method definitions."
    :accessor method-definitions))
  (:documentation "Abstract root class for classoid definitions.
These are conditions, structures, and classes."))

(defmethod initialize-instance :after
    ((definition classoid-definition) &key foreign)
  "Create all classoid DEFINITION's slot definitions, unless FOREIGN."
  (unless foreign
    (setf (slot-definitions definition)
	  (mapcar (lambda (slot) (make-slot-definition slot definition))
	    (sb-mop:class-direct-slots (classoid definition))))))

(defclass condition-definition (classoid-definition)
  ((object :reader definition-condition) ;; slot overload
   (superclassoid-definitions ;; slot overload
    :accessor supercondition-definitions)
   (subclassoid-definitions ;; slot overload
    :accessor subcondition-definitions))
  (:documentation "The class of condition definitions."))

(defclass structure-definition (classoid-definition)
  ((object :reader definition-structure) ;; slot overload
   (superclassoid-definitions ;; slot overload
    :accessor superstructure-definitions)
   (subclassoid-definitions ;; slot overload
    :accessor substructure-definitions))
  (:documentation "The class of structure definitions."))

(defclass class-definition (classoid-definition)
  ((object :reader definition-class) ;; slot overload
   (superclassoid-definitions ;; slot overload
    :accessor superclass-definitions)
   (subclassoid-definitions ;; slot overload
    :accessor subclass-definitions))
  (:documentation "The class for class definitions."))

;; #### PORTME.
(defun make-classoid-definition (symbol classoid &optional foreign)
  "Make a new CLASSOID definition for SYMBOL, possibly FOREIGN.
The concrete class of the new definition (structure, class, or condition)
depends on the kind of CLASSOID."
  (make-instance
      (typecase classoid
	(sb-pcl::condition-class 'condition-definition)
	(sb-pcl::structure-class 'structure-definition)
	(otherwise 'class-definition))
    :symbol symbol :classoid classoid :foreign foreign))




;; ==========================================================================
;; Extraction
;; ==========================================================================

;; #### PORTME.
(defun make-symbol-definitions
    (symbol &aux (setf-symbol `(setf ,symbol)) definitions)
  "Make and return a list of all existing definitions for SYMBOL."
  ;; Constants.
  (when (eql (sb-int:info :variable :kind symbol) :constant)
    (push (make-constant-definition symbol) definitions))
  ;; Special variables.
  (when (eql (sb-int:info :variable :kind symbol) :special)
    (push (make-special-definition symbol) definitions))
  ;; Symbol macros.
  (when (eql (sb-int:info :variable :kind symbol) :macro)
    (push (make-symbol-macro-definition symbol) definitions))
  ;; Macros.
  (when-let (macro (macro-function symbol))
    (push (make-macro-definition symbol macro) definitions))
  ;; Compiler macros.
  (when-let (compiler-macro (compiler-macro-function symbol))
    (push (make-compiler-macro-definition symbol compiler-macro) definitions))
  ;; Setf compiler macros.
  (when-let (compiler-macro (compiler-macro-function setf-symbol))
    (push (make-compiler-macro-definition symbol compiler-macro t)
	  definitions))
  ;; Setf expanders
  (when-let (expander (sb-int:info :setf :expander symbol))
    (push (make-expander-definition symbol expander) definitions))
  ;; (Generic) functions.
  (when-let (function (and (fboundp symbol)
			   (not (macro-function symbol))
			   (fdefinition symbol)))
    ;; #### NOTE: technically, the symbol can be extracted from the generic
    ;; function object. However, using this general constructor is more
    ;; homogeneous with the rest.
    (push (make-function-definition symbol function) definitions))
  ;; (Generic) setf functions.
  (when-let (function (and (fboundp setf-symbol) (fdefinition setf-symbol)))
    ;; #### NOTE: technically, the symbol can be extracted from the generic
    ;; function object. However, using this general constructor is more
    ;; homogeneous with the rest.
    (push (make-function-definition symbol function :setf t) definitions))
  ;; Method combinations.
  ;; #### WARNING: method combinations are ill-defined in the Common Lisp
  ;; standard. In particular, they are not necessarily global objects and
  ;; don't have an actual namespace. This has been explained in a blog first
  ;; (https://cutt.ly/AjIJXwA) and then in a ELS paper
  ;; (http://www.doi.org/10.5281/zenodo.3247610). As a consequence, in order
  ;; to be 100% correct (and also 200% pedantic), we should normally document
  ;; every single generic function's method combination as a local object. We
  ;; will assume, however, that the programmer has some sanity, and only
  ;; defines one method combination for every name. The corresponding object
  ;; will be documented like the other ones, and generic functions using it
  ;; will provide a cross-reference to it, also advertising the options in
  ;; use.
  (when-let*
      ;; #### FIXME: I think this is not needed anymore, after Christophe's
      ;; modifications following my ELS paper.
      ((method
	(find-method #'sb-mop:find-method-combination
		     nil
		     `(,(find-class 'generic-function) (eql ,symbol) t)
		     nil))
       (combination
	;; #### NOTE: we could use any generic function instead of
	;; DOCUMENTATION here. Also, NIL options don't matter because they are
	;; not advertised as part of the method combination, but as part of
	;; the generic functions that use them.
	(sb-mop:find-method-combination #'documentation symbol nil)))
    (push (make-combination-definition symbol combination) definitions))
  ;; Structures, classes, and conditions,
  (when-let (classoid (find-class symbol nil))
    (push (make-classoid-definition symbol classoid) definitions))
  ;; Types
  (when (eql (sb-int:info :type :kind symbol) :defined)
    (push (make-type-definition symbol) definitions))

  definitions)






;; #### PORTME.
(defun specializers (method)
  "Return METHOD's specializers."
  (sb-mop:method-specializers (definition-method method)))

;; #### PORTME.
(defun qualifiers (method)
  "Return METHOD's qualifiers."
  (method-qualifiers (definition-method method)))

;;; symbol.lisp ends here
