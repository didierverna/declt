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
		       :accessor package-definition))
  (:documentation "Abstract root class for all definitions named by symbols."))

(defun symbol-definition-p (object)
  "Return T if OBJECT is a symbol definition."
  (eq (type-of object) 'symbol-definition))

(defmethod name ((definition symbol-definition))
  "Return symbol DEFINITION's symbol.
Note that even in the case of setf expanders or functions, only the canonical
symbol is returned. More specifically, the name of (SETF FOO) is just FOO,
and the name of a setf expander is its ACCESS-FN symbol. The advantage of this
policy is that if a documentation engine chooses to sort the definitions by
lexicographic order, related accessors end up being located next to each
other."
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

(defmethod source ((definition constant-definition))
  "Return constant DEFINITION's source."
  (definition-source-by-name definition :constant))



;; Special variables

(defclass special-definition (varoid-definition)
  ()
  (:documentation "The class of special variable definitions."))

(defun make-special-definition (symbol)
  "Make a new special variable definition for SYMBOL."
  (make-instance 'special-definition :symbol symbol))

(defmethod source ((definition special-definition))
  "Return special DEFINITION's source."
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

(defmethod source ((definition symbol-macro-definition))
  "Return symbol macro DEFINITION's source."
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

(defabstract expander-mixin ()
  ((expander-for
    :documentation "A setf expander definition for this funcoid, or NIL.
This is the definition of a setf expander that expands forms identical to this
funcoid's signature. There can be only one. Note that the Common Lisp standard
does not impose any actual relation between the setf expander and its
access-fn. In fact, the access-fn may not even exist at all. However, if it
does, it is very likely that it is a reader for the place updated by this setf
expander."
    :accessor expander-for)
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

(defun make-macro-definition (symbol macro)
  "Make a new MACRO definition for SYMBOL."
  (make-instance 'macro-definition :symbol symbol :macro macro))



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

(defmethod source ((definition type-definition))
  "Return type DEFINITION's source."
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
    :accessor access-definition))
  (:documentation "Abstract root class for setf expander definitions."))

(defmethod docstring ((definition %expander-definition))
  "Return setf expander DEFINITION's docstring."
  (documentation (definition-symbol definition) 'setf))


(defclass short-expander-definition (%expander-definition)
  ((object :reader update-fn-name) ;; slot overload
   (update-definition
    :documentation "The corresponding update-fn definition.
This is a function or macro definition. It always exists."
    :accessor update-definition))
  (:documentation "The class of short form setf expanders definitions.
Short form setf expanders simply expand to a globally defined function or
macro."))

(defmethod source ((definition short-expander-definition))
  "Return the source pathname of short setf expander DEFINITION's update-fn."
  ;; #### NOTE: looking at how sb-introspect does it, it seems that the
  ;; "source" of a setf expander is the source of the function object. For
  ;; long forms, this should be OK. For short forms however, what we get is
  ;; the source of the update function, which may be different from where
  ;; DEFSETF was called, hence incorrect.

  ;; #### FIXME: the comment below may not be relevant anymore, after the
  ;; current overhaul.
  ;; There is an additional problem when the update function is foreign: we
  ;; don't normally fill in the FUNCTION slot in foreign funcoid definitions
  ;; because we don't care (we only print their names). In the case of setf
  ;; expanders however, we need to do so because Declt will try to find the
  ;; definition source for it, and will attempt to locate the source of the
  ;; foreign function. This triggered a bug in a previous version (with the
  ;; package cl-stdutils, which uses RPLACA as an update function for the
  ;; stdutils.gds::vknode-value expander).
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

(defmethod source ((definition long-expander-definition)
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

;; #### NOTE: all functions are created with MAKE-FUNCTION-DEFINITION. This
;; looks nicer in MAKE-SYMBOL-DEFINITIONS, with one drawback: the :setf
;; argument is not really necessary in the case of generic functions, because
;; (as for methods), the function's name could be deduced from the function
;; object.
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
(defmethod lambda-list ((definition method-definition))
  "Return method DEFINITION's method lambda-list."
  (sb-mop:method-lambda-list (definition-method definition)))


(defclass method-definition (%method-definition)
  ()
  (:documentation "The class of non-setf method definitions."))

(defclass setf-method-definition (setf-mixin %method-definition)
  ()
  (:documentation "The class of setf method definitions."))

(defun make-method-definition (method definition &key foreign)
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

;; #### NOTE: even the short form is a funcoid, because of the optional ORDER
;; option which defaults to :most-specific-first.

(defabstract combination-definition (funcoid-definition)
  ((object :initarg :combination :reader combination) ;; slot overload
   (user-definitions
    :documentation
    "The list of corresponding method combination user definitions.
This is a list of generic function definitions for generic functions using
this combination."
    :accessor user-definitions))
  (:documentation "Abstract root class for method combination definitions."))

(defclass short-combination-definition (combination-definition)
  ((operator-definition :documentation "The corresponding operator definition."
			:accessor operator-definition))
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
	(sb-pcl::long-method-combination 'long-combination-definition))
    :symbol symbol :combination combination :foreign foreign))



;; Generic functions

;; #### TODO: we could think of creating classes of reader and writer generic
;; functions. The condition to upgrade the class would be that all methods are
;; indeed readers / writers.

(defabstract generic-function-definition (%function-definition)
  ((object :reader generic) ;; slot overload
   (method-definitions
    :documentation "The list of corresponding method definitions."
    :accessor method-definitions)
   (combination-definition
    :documentation "The corresponding method combination definition."
    :accessor combination-definition))
  (:documentation "Abstract root class for generic function definitions."))

(defmethod initialize-instance :after
    ((definition generic-function-definition) &key foreign)
  "Create generic DEFINTION's method definitions."
  (setf (method-definitions definition)
	(mapcar (lambda (method)
		  (make-method-definition method definition :foreign foreign))
	  (sb-mop:generic-function-methods (generic definition)))))

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

(defclass slot-definition (symbol-definition)
  ((object :initarg :slot :reader slot) ;; slot overload
   (classoid-definition :documentation "The corresponding classoid definition."
			:initarg :classoid-definition
			:reader classoid-definition)
   (reader-definitions :documentation "The list of slot reader definitions."
		       :accessor reader-definitions)
   (writer-definitions :documentation "The list of slot writer definitions."
		       :accessor writer-definitions))
  (:documentation "The class of slot definitions."))

;; #### PORTME.
(defun make-slot-definition (slot definition)
  "Make a new SLOT definition for classoid DEFINITION."
  (make-instance 'slot-definition
    :symbol (sb-mop:slot-definition-name slot)
    :slot slot
    :classoid-definition definition))

;; #### PORTME.
(defmethod docstring ((definition slot-definition))
  "Return slot DEFINITION's docstring."
  (sb-pcl::%slot-definition-documentation (slot definition)))



;; -----------------------------------
;; Conditions, structures, and classes
;; -----------------------------------

(defabstract classoid-definition (symbol-definition)
  ((object :initarg :classoid :reader classoid) ;; slot overload
   (slot-definitions
    :documentation "The list of corresponding direct slot definitions."
    :accessor slot-definitions)
   (superclassoid-definitions
    :documentation "The list of corresponding direct superclassoid definitions."
    :accessor superclassoid-definitions)
   (subclassoid-definitions
    :documentation "The list of corresponding direct subclassoid definitions."
    :accessor subclassoid-definitions)
   (method-definitions
    :documentation "The list of corresponding direct method definitions."
    :accessor method-definitions))
  (:documentation "Abstract root class for classoid definitions.
These are conditions, structures, and classes."))

(defmethod initialize-instance :after ((definition classoid-definition) &key)
  (setf (slot-definitions definition)
	(mapcar (lambda (slot) (make-slot-definition slot definition))
	  (sb-mop:class-direct-slots (classoid definition)))))

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

;; #### NOTE: sb-introspect does funny stuff for typed structures, so it's
;; better to use it.
(defmethod source ((definition structure-definition))
  "Return structure DEFINITION's source."
  (definition-source-by-name definition :structure))

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
#+()(defgeneric find-definition (name type definitions &optional errorp)
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

;; #### NOTE: this function is used only for finding methods specialized on
;; classoids in the finalization process, so it may encounter a foreign
;; generic function.
#+()(defun find-method-definition (method definitions)
  "Find a method definition for METHOD in DEFINITIONS.
Return NIL if not found."
  (multiple-value-bind (name writerp) (method-name method)
    (when-let (generic (find-definition name 'generic-definition definitions))
      (if writerp
	(etypecase generic
	  (generic-writer-definition
	   (find method (method-definitions generic) :key #'definition-method))
	  (generic-accessor-definition
	   (find method (method-definitions (writer-definition generic))
		 :key #'definition-method)))
	(find method (method-definitions generic) :key #'definition-method)))))

#+()(defgeneric type-definitions (type definitions)
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

#+()
(defun make-symbol-definition (symbol type)
  ;; #### NOTE: for a generic accessor function, we store accessor methods in
  ;; the generic accessor function definition, along with standard methods.
  ;; Only writer-only methods are stored in the generic writer function
  ;; definition.
  (mapcar (lambda (method)
	    (let ((writer-method (and generic-writer-p
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
		 method
		 (make-writer-method-definition writer-method))
		(make-method-definition method))))
    (sb-mop:generic-function-methods function)))

;; #### PORTME.
(defun slot-property (slot property)
  "Return SLOT definition's PROPERTY value."
  (funcall
   (intern (concatenate 'string "SLOT-DEFINITION-" (symbol-name property))
	   :sb-mop)
   slot))

#+()(defgeneric slot-reader-definitions (slot definitions)
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

#+()(defgeneric slot-writer-definitions (slot definitions)
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
#+()(defgeneric definition-combination-users (definition combination)
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
#+()(defun finalize-definitions (definitions)
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
			(make-method-definition method t)))
	       methods))
	   (compute-combination (generic-definition)
	     (let* ((combination (sb-mop:generic-function-method-combination
				  (generic generic-definition)))
		    (name (sb-pcl::method-combination-type-name combination)))
	       (setf (combination-definition generic-definition)
		     (or (find-definition name 'combination-definition
					  definitions)
			 (if (sb-pcl::short-method-combination-p combination)
			   (make-short-combination-definition
			    name combination t)
			   (make-long-combination-definition
			    name combination t)))))))
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

;;; symbol.lisp ends here
