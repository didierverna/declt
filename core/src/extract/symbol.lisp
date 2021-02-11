;;; symbol.lisp --- Symbol definitions

;; Copyright (C) 2010-2013, 2017, 2020, 2021 Didier Verna

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


;; ==========================================================================
;; Local Utilities
;; ==========================================================================

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
   (home-package
    :documentation "The home package definition for this definition's symbol."
    :initform nil :accessor home-package))
  (:documentation "Abstract root class for all definitions named by symbols.
All symbol definitions respond to the following public protocols, which see:
- `publicp'."))

(defun symbol-definition-p (definition)
  "Return T if DEFINITION is a symbol definition."
  (typep definition 'symbol-definition))



;; ---------------------------
;; Public definition protocols
;; ---------------------------

(defmethod name ((definition symbol-definition))
  "Return symbol DEFINITION's symbol."
  (definition-symbol definition))

(defun publicp (definition)
  "Return T is DEFINITION is public.
A definition is public when the symbol naming it is exported from its home
package."
  (member (definition-symbol definition)
	  (external-symbols (home-package definition))))




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

(defclass special-definition (variable-definition)
  ()
  (:documentation "The class of special variable definitions."))

(defun make-special-definition (symbol)
  "Make a new special variable definition for SYMBOL."
  (make-instance 'special-definition :symbol symbol))

(defmethod source-pathname ((definition special-definition))
  "Return special DEFINITION's source pathname."
  (definition-source-by-name definition :variable))



;; Symbol macros

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

;; #### NOTE: slots are defined as varoids, but they will appear later on,
;; along with classoid definitions.




;; ==========================================================================
;; Funcoids
;; ==========================================================================

(defabstract funcoid-definition (symbol-definition)
  ((object :reader funcoid)) ;; slot overload
  (:documentation "Abstract root class for functional definitions.
These are (compiler) macros, (generic) functions, methods, setf expanders,
method combinations, and types.
All funcoid definitions respond to the following public protocols, which see:
- `lambda-list'."))

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
  ((target-slot
    :documentation "The target slot definition for this definition's accessor."
    :initarg :target-slot :reader target-slot))
  (:documentation "Mixin class for accessor definitions.
An accessor is a funcoid which reads or writes a target slot in a classoid.
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

(defabstract expander-definition (setf-mixin funcoid-definition)
  ((object :initarg :expander :reader expander) ;; slot overload
   (standalone-reader
    :documentation
    "A standalone reader definition for this definition's expander, or NIL.
If it exists, it's a definition for a function or macro with the same
signature as that of the expander's access-fn. Note that the Common Lisp
standard does not impose any actual relation between the setf expander and its
access-fn. In fact, the access-fn may not even exist at all. However, if it
does, it is very likely that it is a reader for the place updated by this setf
expander."
    :initform nil :accessor standalone-reader))
  (:documentation "Abstract root class for setf expander definitions."))

(defmethod docstring ((definition expander-definition))
  "Return setf expander DEFINITION's docstring."
  (documentation (definition-symbol definition) 'setf))


(defclass short-expander-definition (expander-definition)
  ((object :reader update-fn-name) ;; slot overload
   (standalone-writer
    :documentation
    "A standalone writer definition for this definition's expander, or NIL.
This is a function or macro definition. Note that if this definition
is unavailable, it means that the expander itself cannot be used (yet)."
    :initform nil :accessor standalone-writer))
  (:documentation "The class of short form setf expanders definitions.
Short form setf expanders simply expand to a globally defined function or
macro."))

(defun short-expander-definition-p (definition)
  "Return T if DEFINITION is a short expander definition."
  (typep definition 'short-expander-definition))

(defmethod source-pathname ((definition short-expander-definition))
  "Return the source pathname of short setf expander DEFINITION's update-fn."
  ;; #### FIXME: looking at how sb-introspect does it, it seems that the
  ;; "source" of a setf expander is the source of the function object. For
  ;; long forms, this should be OK. For short forms however, what we get is
  ;; the source of the update function, which may be different from where
  ;; DEFSETF was called, hence incorrect.
  (when (fboundp (update-fn-name definition))
    (object-source-pathname (fdefinition (update-fn-name definition)))))

;; #### PORTME.
(defmethod lambda-list
    ((definition short-expander-definition)
     &aux (update-fn-name (update-fn-name definition))
	  (fdefinition (when (fboundp update-fn-name)
			 (fdefinition update-fn-name))))
  "Return the \"butlast\" lambda-list of setf expander DEFINITION's update-fn.
This is because short form setf expanders pass the new value as the last
argument to their update-fn.
If the expander's update-fn is not defined, return two values: NIL and T."
  (if fdefinition
    (butlast (sb-introspect:function-lambda-list fdefinition))
    (values nil t)))


(defclass long-expander-definition (expander-definition)
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



;; -------------------
;; (Generic) functions
;; -------------------

(defabstract function-definition (funcoid-definition)
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
	 (if setf 'generic-setf-definition 'simple-generic-definition))
	(otherwise
	 (if setf 'setf-function-definition 'simple-function-definition)))
    :symbol symbol :function function :foreign foreign))



;; Ordinary functions

;; #### NOTE: only basic function definitions are created. Reader and writer
;; definitions are created during the finalization process by upgrading the
;; class of the concerned definitions.

(defabstract ordinary-function-definition (function-definition)
  ()
  (:documentation "Abstract root class for ordinary functions."))

(defclass simple-function-definition
    (ordinary-function-definition expander-mixin)
  ()
  (:documentation "The class of ordinary, non-setf function definitions."))

(defclass setf-function-definition
    (setf-mixin ordinary-function-definition)
  ()
  (:documentation "The class of ordinary setf function definitions."))

(defclass reader-definition (simple-function-definition accessor-mixin)
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



;; Generic functions

;; #### TODO: we could think of creating classes of reader and writer generic
;; functions. The condition to upgrade the class would be that all methods are
;; indeed readers / writers.

(defabstract generic-function-definition (function-definition)
  ((object :initarg :generic :reader generic) ;; slot overload
   (methods
    :documentation
    "The list of method definitions for this definition's generic function."
    :initform nil :accessor methods)
   (combination
    :documentation
    "The method combination definition for this definition's generic function."
    :initform nil :accessor combination))
  (:documentation "Abstract root class for generic function definitions."))

(defmethod initialize-instance :after
    ((definition generic-function-definition) &key foreign)
  "Create all generic DEFINTION's method definitions, unless FOREIGN."
  (unless foreign
    (setf (methods definition)
	  (mapcar (lambda (method)
		    (make-method-definition method definition))
	    (generic-function-methods (generic definition))))))

;; #### PORTME.
(defun combination-options (definition)
  "Return generic function DEFINITION's method combination options."
  (sb-pcl::method-combination-options
   (generic-function-method-combination (generic definition))))


(defclass simple-generic-definition
    (generic-function-definition expander-mixin)
  ()
  (:documentation "The class of non-setf, generic function definitions."))

(defclass generic-setf-definition (setf-mixin generic-function-definition)
  ()
  (:documentation "The class of ordinary setf function definitions."))



;; Method combinations

;; #### NOTE: the root class is not abstract, because it is used for the
;; standard method combination.

(defclass combination-definition (funcoid-definition)
  ((object :initarg :combination :reader combination) ;; slot overload
   (clients
    :documentation
    "The list of client definitions for this definition's method combnination.
These are generic functions using this combination."
    :accessor clients))
  (:documentation "Root class for method combination definitions."))

;; #### PORTME.
(defmethod lambda-list ((definition combination-definition))
  "Return method combination DEFINITION's lambda-list."
  (sb-introspect:method-combination-lambda-list (combination definition)))


;; #### WARNING: the CLHS specifies that the :operator argument to
;; DEFINE-METHOD-COMBINATION is an /operator/, but later on claims that it is
;; a /symbol/. Experimentation seems to demonstrate that it must be a symbol.
(defclass short-combination-definition (combination-definition)
  ((standalone-combinator
    :documentation "The corresponding standalone combinator definition, or NIL.
This is a function or macro definition. Note that if this definition
is unavailable, it means that the method combination itself cannot be used
(yet)."
    :initform nil :accessor standalone-combinator))
  (:documentation "The class of short method combination definitions."))

;; #### PORTME.
(defun identity-with-one-argument (definition)
  "Return short combination DEFINITION's :identity-with-one-argument option."
  (sb-pcl::short-combination-identity-with-one-argument
   (combination definition)))


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



;; -------
;; Methods
;; -------

(defabstract method-definition (funcoid-definition)
  ((object :initarg :method :reader definition-method) ;; slot overload
   (owner :documentation
	  "The generic function definition for this definition's method."
	  :initarg :owner :reader owner)
   (specializers :documentation "The specializers of this definition's method.
This is a list of either class definitions (for regular specializers),
or raw EQL specializers."
		 :accessor specializers))
  (:documentation "Abstract root class for method definitions."))

(defmethod lambda-list ((definition method-definition))
  "Return method DEFINITION's method lambda-list."
  (method-lambda-list (definition-method definition)))


(defclass simple-method-definition (method-definition)
  ()
  (:documentation "The class of non-setf method definitions."))

(defclass setf-method-definition (setf-mixin method-definition)
  ()
  (:documentation "The class of setf method definitions."))

(defun method-name
    (method
     &aux (name (generic-function-name (method-generic-function method))))
  "Return METHOD's canonical name.
Return a second value of T if METHOD is in fact a SETF one."
  (if (listp name)
    (values (second name) t)
    name))

(defun make-method-definition (method definition &optional foreign)
  "Make a new METHOD definition for generic DEFINITION, possibly FOREIGN.
The concrete class of the new definition depends on whether it is a SETF one."
  (multiple-value-bind (symbol setf)
      (method-name method)
    (make-instance (if setf 'setf-method-definition 'simple-method-definition)
      :symbol symbol :method method :owner definition :foreign foreign)))


;; #### NOTE: only basic method definitions are created. Reader and writer
;; definitions are created during the finalization process by upgrading the
;; class of the concerned definitions.

;; #### NOTE: the situation for readers and writers methods (on class and
;; condition slots) is different from that of ordinary readers and writers (on
;; structure slots). Indeed, an :accessor specification will create a setf
;; function, but a :writer specification allows you to do whatever you like,
;; not necessarily a setf form, so the hierarchy needs to be a bit different.

(defclass reader-method-definition (simple-method-definition accessor-mixin)
  ()
  (:documentation "The class of reader method definitions.
A reader method is a method that reads a slot in a class or condition."))


(defabstract writer-method-definition (method-definition accessor-mixin)
  ()
  (:documentation "Abstract root class for writer method definitions."))

(defclass simple-writer-method-definition (writer-method-definition)
  ()
  (:documentation "The class of non-setf writer method definitions.
A non-setf writer method is a non-setf method that writes a slot in a class
or a condition."))

(defclass setf-writer-method-definition (setf-mixin writer-method-definition)
  ()
  (:documentation "The class of setf writer method definitions.
A setf writer method is a setf method that writes a slot in a class
or a condition."))




;; ==========================================================================
;; Classoids
;; ==========================================================================

;; -----------------------------------
;; Conditions, structures, and classes
;; -----------------------------------

(defabstract classoid-definition (symbol-definition)
  ((object :initarg :classoid :reader classoid) ;; slot overload
   (direct-slots
    :documentation
    "The list of direct slot definitions for this definition's classoid."
    :initform nil :accessor direct-slots))
  (:documentation "Abstract root class for classoid definitions.
These are conditions, structures, and classes."))

;; #### PORTME.
(defun make-classoid-definition (symbol classoid &optional foreign)
  "Make a new CLASSOID definition for SYMBOL, possibly FOREIGN.
The concrete class of the new definition (structure, class, or condition)
depends on the kind of CLASSOID."
  (make-instance
      (typecase classoid
	(sb-pcl::condition-class 'condition-definition)
	(sb-pcl::structure-class 'clos-structure-definition)
	(sb-kernel:defstruct-description 'typed-structure-definition)
	(otherwise 'class-definition))
    :symbol symbol :classoid classoid :foreign foreign))



;; CLOS classoids

;; #### FIXME: it should be possible to reconstruct super- and sub-structure
;; information even in the case of typed structures. So some of the slots
;; below are bound to move.

(defabstract clos-classoid-mixin ()
  ((direct-superclassoids
    :documentation
    "The list of direct superclassoid definitions for this definition's classoid."
    :accessor direct-superclassoids)
   (direct-subclassoids
    :documentation
    "The list of direct subclassoid definitions for this definition's classoid."
    :accessor direct-subclassoids)
   (direct-methods
    :documentation
    "The list of direct method definitions for this definition's classoid."
    :accessor direct-methods))
  (:documentation "Mixin for CLOS-based classoids.
These are conditions, ordinary structures, and classes."))

(defmethod initialize-instance :after
    ((definition clos-classoid-mixin) &key foreign)
  "Create all CLOS classoid DEFINITION's slot definitions, unless FOREIGN."
  (unless foreign
    (setf (direct-slots definition)
	  (mapcar (lambda (slot) (make-clos-slot-definition slot definition))
	    (class-direct-slots (classoid definition))))))

(defun direct-default-initargs (definition)
  "Return CLOS classoid mixin DEFINITION's direct default initargs."
  (class-direct-default-initargs (classoid definition)))


(defclass condition-definition (classoid-definition clos-classoid-mixin)
  ((object :reader definition-condition) ;; slot overload
   (direct-superclassoids ;; slot overload
    :accessor direct-superconditions)
   (direct-subclassoids ;; slot overload
    :accessor direct-subconditions))
  (:documentation "The class of condition definitions."))

(defclass class-definition (classoid-definition clos-classoid-mixin)
  ((object :reader definition-class) ;; slot overload
   (direct-superclassoids ;; slot overload
    :accessor direct-superclasses)
   (direct-subclassoids ;; slot overload
    :accessor direct-subclasses))
  (:documentation "The class for class definitions."))


(defabstract structure-definition (classoid-definition)
  ((object :reader definition-structure)) ;; slot overload
  (:documentation "Abstract root class for structures."))

(defclass clos-structure-definition (structure-definition clos-classoid-mixin)
  ((direct-superclassoids ;; slot overload
    :accessor direct-superstructures)
   (direct-subclassoids ;; slot overload
    :accessor direct-substructures))
  (:documentation "The class of CLOS structure definitions."))



;; Typed structures

(defclass typed-structure-definition (structure-definition)
  ((type :documentation "The structure type, either LIST or VECTOR."
	 :initarg :type :accessor structure-type)
   (element-type :documentation "The structure's element type.
It is T for list structures, but may be something else for vector ones."
		 :initarg :element-type :accessor element-type))
  (:documentation "The class of typed structure definitions."))

;; #### PORTME.
(defmethod initialize-instance :after
    ((definition typed-structure-definition)
     &key foreign
     &aux (structure (definition-structure definition)))
  "Compute typed structure DEFINITION's type and element type.
Unless FOREIGN, also compute its slot definitions."
  (setf (structure-type definition) (sb-kernel:dd-type structure))
  (setf (element-type definition) (sb-kernel::dd-element-type structure))
  (unless foreign
    (setf (direct-slots definition)
	  (mapcar (lambda (slot)
		    (make-typed-structure-slot-definition slot definition))
	    (sb-kernel:dd-slots (definition-structure definition))))))

;; #### PORTME.
(defmethod docstring ((definition typed-structure-definition))
  "Return typed structure DEFINITION's docstring."
  (sb-kernel::dd-doc (definition-structure definition)))



;; -----
;; Slots
;; -----

;; #### NOTE: structure slots only have one associated reader / writer, so
;; it's slightly overkill to use a list of those. On the other hand, it allows
;; the documentation rendering code to be applicable to all classoids.

(defabstract slot-definition (varoid-definition)
  ((object :initarg :slot :reader slot) ;; slot overload
   (owner
    :documentation "The definition for the owner of this definition's slot."
    :initarg :owner :reader owner)
   (readers
    :documentation "The list of definitions for this definition's slot readers."
    :initform nil :accessor readers)
   (writers
    :documentation "The list of definitions for this definition's slot writers."
    :initform nil :accessor writers))
  (:documentation "Abstract root class for slots."))

(defgeneric value-type (definition)
  (:documentation "Return slot DEFINITION's value type."))


(defclass clos-slot-definition (slot-definition)
  ()
  (:documentation "The class of CLOS slot definitions."))

(defun make-clos-slot-definition (slot owner &optional foreign)
  "Make a new CLOS SLOT definition for classoid OWNER."
  (make-instance 'clos-slot-definition
    :symbol (slot-definition-name slot)
    :slot slot
    :owner owner
    :foreign foreign))

;; #### PORTME.
(defmethod docstring ((definition clos-slot-definition))
  "Return CLOS slot DEFINITION's docstring."
  (sb-pcl::%slot-definition-documentation (slot definition)))

(defmethod value-type ((definition clos-slot-definition))
  "Return CLOS slot DEFINITION's value type."
  (slot-definition-type (slot definition)))

(defun allocation (definition)
  "Return CLOS slot DEFINITION's allocation type."
  (slot-definition-allocation (slot definition)))

(defun initform (definition)
  "Return CLOS slot DEFINITION's initform."
  (slot-definition-initform (slot definition)))

(defun initargs (definition)
  "Return CLOS slot DEFINITION's initargs."
  (slot-definition-initargs (slot definition)))


(defclass typed-structure-slot-definition (slot-definition)
  ()
  (:documentation "The class of typed structure slot definitions."))

;; #### PORTME.
(defun make-typed-structure-slot-definition (slot owner &optional foreign)
  "Make a new typed structure SLOT definition for classoid OWNER."
  (make-instance 'typed-structure-slot-definition
    :symbol (sb-kernel:dsd-name slot)
    :slot slot
    :owner owner
    :foreign foreign))

(defmethod docstring ((definition typed-structure-slot-definition))
  "Return NIL."
  nil)

;; #### PORTME.
(defmethod value-type ((definition typed-structure-slot-definition))
  "Return typed structure slot DEFINITION's value type."
  (sb-kernel:dsd-type (slot definition)))

;;; symbol.lisp ends here
