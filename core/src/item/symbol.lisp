;;; symbol.lisp --- Symbol based items

;; Copyright (C) 2010-2013, 2017 Didier Verna

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

;; #### FIXME: the setf expander code needs to be redone. Currently we only
;; #### look for setf expanders as part of other categories (macros, functions
;; #### etc). But it seems that it is possible to define standalone
;; #### expanders. For example:
;; #### (defgeneric set-state (state chain)
;; ####   (:method (state chain)))
;; ####
;; #### (defsetf state (object) (store)
;; ####   `(set-state ,store ,object))
;; #### In such a case, there is no accessor. Only an updater.



;;; Code:

(in-package :net.didierverna.declt)
(in-readtable :net.didierverna.declt)


;; ==========================================================================
;; Definitions
;; ==========================================================================

;; ----------
;; Categories
;; ----------

;; #### NOTE: when constructing the context lists of external and internal
;; definitions, only the definitions listed in *CATEGORIES* appear. This is
;; because these lists follow the structure of the Definitions chapter in the
;; generated manual. For instance, methods are listed under the corresponding
;; generic function, so they don't represent a category of its own.

;; #### NOTE: the order in *CATEGORIES* is important (see
;; ADD-CATEGORIES-NODE). It conditions the order of appearance of the
;; definitions in the generated manual.

(defparameter *categories*
  '((:constant       "constants")
    (:special        "special variables")
    (:symbol-macro   "symbol macros")
    (:macro          "macros")
    (:compiler-macro "compiler macros")
    (:function       "functions")
    (:generic        "generic functions")
    (:combination    "method combinations")
    (:condition      "conditions")
    (:structure      "structures")
    (:class          "classes")
    (:type           "types"))
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

(defun definition-package (definition)
  "Return DEFINITION's symbol home package."
  (symbol-package (definition-symbol definition)))

(defun definition-package-name (definition)
  "Return DEFINITION's symbol home package name."
  (name (definition-package definition)))


(defstruct (constant-definition (:include definition))
  "Structure for constant definitions.")
(defstruct (special-definition (:include definition))
  "Structure for special variables definitions.")
(defstruct (symbol-macro-definition (:include definition))
  "Structure for symbol macro definitions.")

(defstruct (funcoid-definition (:include definition))
  "Base structure for definitions of functional values.
This structure holds the generic, ordinary or macro function."
  function)

(defstruct (macro-definition (:include funcoid-definition))
  "Structure for macro definitions.
This structure holds a setf expander definition that expands this macro and a
setf expander definition that expands to this macro."
  ;; #### NOTE: you may notice that contrary to (generic) functions, macro
  ;; definitions don't have a WRITER slot. That is because a setf function
  ;; cannot be directly associated with a macro so there's no point in trying
  ;; to concatenate their documentation. See sections 5.1.2.7 and 5.1.2.9 of
  ;; the Hyperref for more information.
  ;; #### NOTE: the reason for an ACCESS-EXPANDER slot here is that Declt will
  ;; attempt to concatenate the definitions for FOO and (SETF FOO) into a
  ;; single documentation item when possible. It makes the reference manual
  ;; more readable IMO, but that's assuming that FOO and (SETF FOO) are indeed
  ;; a no-nonsense reader/writer pair of macros...
  access-expander
  update-expander)

(defstruct (compiler-macro-definition (:include funcoid-definition))
  "Structure for compiler macro definitions.")

(defstruct (function-definition (:include funcoid-definition))
  "Structure for ordinary function definitions.
This structure holds a slot for marking foreign definitions, i.e. those which
do not pertain to the system being documented, and a setf expander definition
that expands to this function."
  foreignp
  update-expander)
;; #### FIXME: writer definitions can't have an associated update expander so
;; it's not clean that they have that slot. I need to refine the structures
;; hierarchy.
(defstruct (writer-definition (:include function-definition))
  "Structure for ordinary writer function definitions.
This structure holds the corresponding reader definition."
  reader)
(defstruct (accessor-definition (:include function-definition))
  "Structure for accessor function definitions.
This structure holds a writer and a setf expander definition that expands
this function."
  ;; #### NOTE: the reason for a WRITER and an ACCESS-EXPANDER slots here is
  ;; that Declt will attempt to concatenate the definitions for FOO and (SETF
  ;; FOO) into a single documentation item when possible. It makes the
  ;; reference manual more readable IMO, but that's assuming that FOO and
  ;; (SETF FOO) are indeed a no-nonsense reader/writer pair of functions...
  writer
  access-expander)

(defstruct (method-definition (:include definition))
  "Base structure for method definitions.
This structure holds the method object and also a slot for marking foreign
definitions, i.e. those which do pertain to the system being documented."
  foreignp
  method)
(defstruct (writer-method-definition (:include method-definition))
  "Structure for writer method definitions.")
(defstruct (accessor-method-definition (:include method-definition))
  "Structure for accessor method definitions.
This structure holds the writer method definition."
  writer)

(defstruct (generic-definition (:include funcoid-definition))
  "Structure for generic function definitions.
This structure holds a setf expander definition that expands to this function,
the combination definition, the list of method definitions and also a slot for
marking foreign definitions, i.e. those which do pertain to the system being
documented."
  foreignp
  update-expander
  combination
  methods)
;; #### FIXME: generic writer definitions can't have an associated update
;; expander so it's not clean that they have that slot. I need to refine the
;; structures hierarchy.
(defstruct (generic-writer-definition (:include generic-definition))
  "Structure for generic writer function definitions.
This structure holds the corresponding reader definition."
  reader)
(defstruct (generic-accessor-definition (:include generic-definition))
  "Structure for generic accessor function definitions.
This structure holds a generic writer and a setf expander definition that
expands this function."
  ;; #### NOTE: the reason for a WRITER and an ACCESS-EXPANDER slots here is
  ;; that Declt will attempt to concatenate the definitions for FOO and (SETF
  ;; FOO) into a single documentation item when possible. It makes the
  ;; reference manual more readable IMO, but that's assuming that FOO and
  ;; (SETF FOO) are indeed a no-nonsense reader/writer pair of functions...
  writer
  access-expander)

(defstruct (setf-expander-definition (:include definition))
  "Structure for setf expander definitions.
This structure holds the access definition and the update object. For
short forms, this object is a macro or (generic) function definition. For long
forms, it's a function."
  access
  update)

(defstruct (slot-definition (:include definition))
  "Structure for slot definitions.
This structure holds the slot object and the readers and writers definitions."
  slot
  readers
  writers)

(defstruct (combination-definition (:include definition))
  "Structure for method combination definitions.
This structure holds the method combination object and a list of users, that
is, generic functions using this method combination."
  foreignp
  combination
  users)

(defstruct (short-combination-definition (:include combination-definition))
  "Structure for short method combination definitions.
This structure holds the operator definition."
  operator)

(defstruct (long-combination-definition (:include combination-definition))
  "Structure for long method combination definitions.")


(defstruct (classoid-definition (:include definition))
  "Base structure for class-like (supporting inheritance) values.
This structure holds links to the direct ancestors and descendants
definitions, direct methods definitions, direct slots, and also a slot for
marking foreign definitions, i.e. those which do pertain to the system being
documented. Foreign definitions may appear as part of an inheritance
documentation."
  foreignp
  parents
  children
  methods
  slots)
(defstruct (condition-definition (:include classoid-definition))
  "Structure for condition definitions.")
(defstruct (structure-definition (:include classoid-definition))
  "Structure for structure definition.")
(defstruct (class-definition (:include classoid-definition))
  "Structure for class definitions.
This structure holds the direct superclasses and direct subclasses
definitions.")

(defstruct (type-definition (:include definition))
  "Structure for type definitions.")

;; #### PORTME.
(defgeneric lambda-list (object)
  (:documentation "Return OBJECT's lambda-list.")
  (:method ((function function))
    "Return FUNCTION's lambda-list."
    (sb-introspect:function-lambda-list function))
  (:method ((funcoid funcoid-definition))
    "Return FUNCOID's lambda-list."
    (lambda-list (funcoid-definition-function funcoid)))
  (:method ((expander setf-expander-definition)
	    &aux (update (setf-expander-definition-update expander)))
    "Return setf EXPANDER's lambda-list."
    (sb-introspect:function-lambda-list
     (etypecase update
       (list (cdr update))
       (function update)
       (funcoid-definition (funcoid-definition-function update)))))
  (:method ((method method-definition))
    "Return METHOD's lambda-list."
    (sb-mop:method-lambda-list (method-definition-method method)))
  (:method ((type type-definition))
    "Return TYPE's lambda-list."
    (sb-introspect:deftype-lambda-list (definition-symbol type))))

;; #### PORTME.
(defun specializers (method)
  "Return METHOD's specializers."
  (sb-mop:method-specializers (method-definition-method method)))

;; #### PORTME.
(defun qualifiers (method)
  "Return METHOD's qualifiers."
  (method-qualifiers (method-definition-method method)))


;; ----------------
;; Definition pools
;; ----------------

(defun make-definitions-pool ()
  "Create and return a new definitions pool.
A definitions pool is a hash table of categorized definitions.
Keys must be of the form (NAME :CATEGORY).
  - NAME is the symbol naming the definition,
  - :CATEGORY is one listed in *CATEGORIES*."
  (make-hash-table :test 'equal))

(defun definitions-pool-size (pool)
  "Return the number of elements in definitions POOL."
  (hash-table-count pool))

(defun mapcan-definitions-pool (function pool)
  "Like MAPCAN, but work on a definitions POOL."
  (loop :for definition :being :the :hash-values :in pool
	:nconc (funcall function definition)))

;; #### FIXME: the finalize process needs to find standalone writers and uses
;; this function. However, this function returns all writers; not only
;; standalone ones. This is not normally a problem because at that time, we're
;; resolving heterogeneous accessors so we shouldn't find leaf writers. To be
;; really pedantic, we could check that it is actually the case.
(defgeneric find-definition (name category pool &optional errorp)
  (:documentation "Find a CATEGORY definition for NAME in POOL.
If ERRORP, throw an error if not found. Otherwise, just return NIL.")
  (:method (name category pool
	    &optional errorp
	    &aux (definition (gethash (list name category) pool)))
    "Default method used for root CATEGORYs"
    (or definition
	(when errorp
	  (error "No ~A definition found for symbol ~A" category name))))
  (:method (name (category (eql :accessor)) pool
	    &optional errorp
	    &aux (definition (find-definition name :function pool errorp)))
    "Method used to find accessor definitions."
    (or (when (accessor-definition-p definition)
	  definition)
	(when errorp
	  (error "No accessor definition found for symbol ~A." name))))
  (:method (name (category (eql :writer)) pool
	    &optional errorp
	    &aux (definition (find-definition name :function pool errorp)))
    "Method used to find writer definitions.
Name must be that of the reader (not the SETF form)."
    (or (typecase definition
	  (writer-definition
	   definition)
	  (accessor-definition
	   (accessor-definition-writer definition)))
	(when errorp
	  (error "No writer definition found for symbol ~A." name))))
  (:method (name (category (eql :generic-accessor)) pool
	    &optional errorp
	    &aux (definition
		  (find-definition name :generic pool errorp)))
    "Method used to find generic accessor definitions."
    (or (when (generic-accessor-definition-p definition)
	  definition)
	(when errorp
	  (error "No generic accessor definition found for symbol ~A." name))))
  (:method (name (category (eql :generic-writer)) pool
	    &optional errorp
	    &aux (definition (find-definition name :generic pool errorp)))
    "Method used to find generic writer definitions.
Name must be that of the reader (not the SETF form)."
    (or (typecase definition
	  (generic-writer-definition
	   definition)
	  (generic-accessor-definition
	   (generic-accessor-definition-writer definition)))
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

(defun find-method-definition (method pool)
  "Find a method definition for METHOD in POOL.
Return NIL if not found."
  (multiple-value-bind (name writerp) (method-name method)
    (when-let ((generic (find-definition name :generic pool)))
      (cond (writerp
	     (etypecase generic
	       (generic-writer-definition
		(find method (generic-definition-methods generic)
		      :key #'method-definition-method))
	       (generic-accessor-definition
		(find method (generic-definition-methods
			      (generic-accessor-definition-writer generic))
		      :key #'method-definition-method))))
	    (t
	     (find method (generic-definition-methods generic)
		   :key #'method-definition-method))))))

(defgeneric category-definitions (category pool)
  (:documentation "Return all CATEGORY definitions from POOL.")
  (:method (category pool)
    "Default method used for root CATEGORYs."
    (loop :for key   :being :the :hash-keys   :in pool
	    :using (:hash-value value)
	  :when (eq (second key) category)
	    :collect value))
  (:method ((category (eql :accessor)) pool)
    "Method used for ordinary accessors."
    (loop :for key   :being :the :hash-keys   :in pool
	    :using (:hash-value value)
	  :when (and (eq (second key) :function)
		     (accessor-definition-p value))
	    :collect value))
  (:method ((category (eql :writer)) pool)
    "Method used for ordinary writers.
Note that this only returns standalone (toplevel) writers."
    (loop :for key   :being :the :hash-keys   :in pool
	    :using (:hash-value value)
	  :when (and (eq (second key) :function)
		     (writer-definition-p value))
	    :collect value))
  (:method ((category (eql :generic-accessor)) pool)
    "Method used for generic accessors."
    (loop :for key   :being :the :hash-keys   :in pool
	    :using (:hash-value value)
	  :when (and (eq (second key) :generic)
		     (generic-accessor-definition-p value))
	    :collect value))
  (:method ((category (eql :generic-writer)) pool)
    "Method used for generic writers.
Note that this only returns standalone (toplevel) generic writers."
    (loop :for key   :being :the :hash-keys   :in pool
	    :using (:hash-value value)
	  :when (and (eq (second key) :generic)
		     (generic-writer-definition-p value))
	    :collect value))
  (:method ((category (eql :short-combination)) pool)
    "Method used for short method combinations."
    (loop :for key   :being :the :hash-keys   :in pool
	    :using (:hash-value value)
	  :when (and (eq (second key) :combination)
		     (short-combination-definition-p value))
	    :collect value))
  (:method ((category (eql :long-combination)) pool)
    "Method used for long method combinations."
    (loop :for key   :being :the :hash-keys   :in pool
	    :using (:hash-value value)
	  :when (and (eq (second key) :combination)
		     (long-combination-definition-p value))
	    :collect value))
  (:method ((category (eql :setf-expander)) pool)
    "Method used for setf expanders."
    (loop :for key   :being :the :hash-keys   :in pool
	    :using (:hash-value value)
	  ;; #### NOTE: do you see why dropping structures and using mixin
	  ;; classes would help here ? ;-)
	  :when (and (eq (second key) :macro)
		     (macro-definition-access-expander value))
	    :collect (macro-definition-access-expander value)
	  :when (and (eq (second key) :function)
		     (accessor-definition-p value)
		     (accessor-definition-access-expander value))
	    :collect (accessor-definition-access-expander value)
	  :when (and (eq (second key) :generic)
		     (generic-accessor-definition-p value)
		     (generic-accessor-definition-access-expander value))
	    :collect (generic-accessor-definition-access-expander value))))

(defun add-definition (symbol category definition pool)
  "Add CATEGORY kind of DEFINITION for SYMBOL to POOL."
  (setf (gethash (list symbol category) pool) definition))

(defun make-slot-definitions (class)
  "Return a list of direct slot definitions for CLASS."
  (mapcar (lambda (slot)
	    (make-slot-definition
	     :symbol (sb-mop:slot-definition-name slot)
	     :slot slot))
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
(defun add-symbol-definition (symbol category pool)
  "Add and return the CATEGORY kind of definition for SYMBOL to pool, if any."
  (or (find-definition symbol category pool)
      (ecase category
	(:constant
	 (when (eql (sb-int:info :variable :kind symbol) :constant)
	   (add-definition
	    symbol category (make-constant-definition :symbol symbol) pool)))
	(:special
	 (when (eql (sb-int:info :variable :kind symbol) :special)
	   (add-definition
	    symbol category (make-special-definition :symbol symbol) pool)))
	(:symbol-macro
	 (when (eql (sb-int:info :variable :kind symbol) :macro)
	   (add-definition
	    symbol category
	    (make-symbol-macro-definition :symbol symbol) pool)))
	(:macro
	 (when-let* ((macro (macro-function symbol))
		     (macro-definition
		      (make-macro-definition :symbol symbol :function macro)))
	   (when-let ((expander (setf-expander-p symbol)))
	     (let ((expander-definition
		     (make-setf-expander-definition
		      :symbol symbol
		      :access macro-definition
		      :update expander)))
	       (setf (macro-definition-access-expander macro-definition)
		     expander-definition)))
	   (add-definition symbol category macro-definition pool)))
	(:compiler-macro
	 (when-let ((compiler-macro (compiler-macro-function symbol)))
	   (add-definition
	    symbol
	    category
	    (make-compiler-macro-definition
	     :symbol symbol :function compiler-macro)
	    pool)))
	;; #### NOTE: As mentionned earlier, the WRITER slot in (generic)
	;; functions helps to attempt concatenation of the reader and writer
	;; documentation. However, we won't attempt concatenation when the
	;; reader and writer are of different nature, that is, one a simple
	;; function and the other a generic one. If any, those cases should be
	;; extremely rare and it doesn't really make sense to merge
	;; heterogeneous documentations (for instance, there would be methods
	;; for only one of the definitions). Besides, regular and generic
	;; functions appear in different sections in the manual.
	(:function
	 (let* ((function (when (and (fboundp symbol)
				     (not (macro-function symbol))
				     (not (typep (fdefinition symbol)
						 'generic-function)))
			    (fdefinition symbol)))
		;; #### NOTE: an accessor's writer definition is created here
		;; only if it's also an ordinary function. Cross-references
		;; between heterogeneous accessors will be resolved when the
		;; pools are finalized.
		(writer (let ((writer-name `(setf ,symbol)))
			  (when (fboundp writer-name)
			    (fdefinition writer-name))))
		(ordinary-writer-p
		  (and writer (not (typep writer 'generic-function))))
		(expander (setf-expander-p symbol)))
	   (cond ((and function (or writer expander))
		  (let ((accessor-definition (make-accessor-definition
					      :symbol symbol
					      :function function)))
		    (when ordinary-writer-p
		      (let ((writer-definition (make-writer-definition
						:symbol symbol
						:function writer
						:reader accessor-definition)))
			(setf (accessor-definition-writer accessor-definition)
			      writer-definition)))
		    (when expander
		      (let ((expander-definition
			      (make-setf-expander-definition
			       :symbol symbol
			       :access accessor-definition
			       :update expander)))
			(setf (accessor-definition-access-expander
			       accessor-definition)
			      expander-definition)))
		    (add-definition symbol category accessor-definition pool)))
		 (function
		  (add-definition
		   symbol
		   category
		   (make-function-definition :symbol symbol
					     :function function)
		   pool))
		 (ordinary-writer-p
		  (add-definition
		   symbol
		   category
		   (make-writer-definition :symbol symbol :function writer)
		   pool)))))
	(:generic
	 (let* ((function
		  (when (and (fboundp symbol)
			     (typep (fdefinition symbol) 'generic-function))
		    (fdefinition symbol)))
		;; #### NOTE: an accessor's writer definition is created here
		;; only if it's also a generic function. Cross-references
		;; between heterogeneous accessors will be resolved when the
		;; pools are finalized.
		(writer
		  (let ((writer-name `(setf ,symbol)))
		    (when (fboundp writer-name)
		      (fdefinition writer-name))))
		(generic-writer-p (typep writer 'generic-function))
		(expander (setf-expander-p symbol)))
	   (cond ((and function (or writer expander))
		  (let ((generic-definition
			  (make-generic-accessor-definition
			   :symbol symbol
			   :function function
			   ;; #### NOTE: for a generic accessor function, we
			   ;; store accessor methods in the generic accessor
			   ;; function definition, along with standard
			   ;; methods. Only writer-only methods are stored in
			   ;; the generic writer function definition.
			   :methods
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
				     :symbol symbol
				     :method method
				     :writer (make-writer-method-definition
					      :symbol symbol
					      :method writer-method))
				    (make-method-definition
				     :symbol symbol :method method))))
			    (sb-mop:generic-function-methods function)))))
		    (when generic-writer-p
		      (let ((writer-definition
			      (make-generic-writer-definition
			       :symbol symbol
			       :function writer
			       :methods
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
					   :symbol symbol
					   :method method))))
				(sb-mop:generic-function-methods
				 writer))
			       :reader generic-definition)))
			(setf (generic-accessor-definition-writer
			       generic-definition)
			      writer-definition)))
		    (when expander
		      (let ((expander-definition
			      (make-setf-expander-definition
			       :symbol symbol
			       :access generic-definition
			       :update expander)))
			(setf (generic-accessor-definition-access-expander
			       generic-definition)
			      expander-definition)))
		    (add-definition symbol category generic-definition
				    pool)))
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
		 (generic-writer-p
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
	;; #### WARNING: method combinations in CL don't have a real namespace
	;; (Cf. this blog: http://www.didierverna.net/blog/index.php?post/2013/08/16/Lisp-Corner-Cases%3A-Method-Combinations).
	;; As a consequence, in order to be 100% correct (and also 200%
	;; pedantic), I should normally document every single generic
	;; function's method combination separately. However, I will make the
	;; assumption that the programmer has some sanity and only defines one
	;; method combination for every name. The corresponding object will be
	;; documented like the other ones. In generic function documentations,
	;; there will be a reference to the method combination and only the
	;; method combination options will be documented there, as they may be
	;; generic function specific.
	(:combination
	 (let* ((method (find-method #'sb-mop:find-method-combination
				     nil
				     `(,(find-class 'generic-function)
				       (eql ,symbol)
				       t)
				     nil))
		(combination (when method
			       (sb-mop:find-method-combination
				;; #### NOTE: we could use any generic
				;; function instead of DOCUMENTATION here.
				;; Also, NIL options don't matter because they
				;; are not advertised as part of the method
				;; combination, but as part of the generic
				;; functions that use them.
				#'documentation symbol nil))))
	   (when combination
	     (add-definition
	      symbol
	      category
	      (etypecase combination
		(sb-pcl::short-method-combination
		 (make-short-combination-definition
		  :symbol symbol
		  :combination combination))
		(sb-pcl::long-method-combination
		 (make-long-combination-definition
		  :symbol symbol
		  :combination combination)))
	      pool))))
	(:condition
	 (let ((class (find-class symbol nil)))
	   (when (and class (typep class 'sb-pcl::condition-class))
	     (add-definition
	      symbol
	      category
	      (make-condition-definition
	       :symbol symbol
	       :slots (make-slot-definitions class))
	      pool))))
	(:structure
	 (let ((class (find-class symbol nil)))
	   (when (and class (typep class 'sb-pcl::structure-class))
	     (add-definition
	      symbol
	      category
	      (make-structure-definition
	       :symbol symbol
	       :slots (make-slot-definitions class))
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
	       :slots (make-slot-definitions class))
	      pool))))
	(:type
	 (when (eql (sb-int:info :type :kind symbol) :defined)
	   (add-definition
	    symbol
	    category
	    (make-type-definition :symbol symbol)
	    pool))))))

(defun add-symbol-definitions (symbol pool)
  "Add all categorized definitions for SYMBOL to POOL."
  (dolist (category *categories*)
    (add-symbol-definition symbol (first category) pool)))

;; #### PORTME.
(defun slot-property (slot property)
  "Return SLOT definition's PROPERTY value."
  (funcall
   (intern (concatenate 'string "SLOT-DEFINITION-" (symbol-name property))
	   :sb-mop)
   slot))

(defgeneric reader-definitions (slot pool1 pool2)
  (:documentation "Return a list of reader definitions for SLOT.")
  (:method (slot pool1 pool2)
    "Defaut method for class and condition slots."
    (mapcar
     (lambda (reader-name)
       (or (find-definition reader-name :generic pool1)
	   (find-definition reader-name :generic pool2)
	   (make-generic-definition :symbol reader-name :foreignp t)))
     (slot-property slot :readers)))
  ;; #### PORTME.
  (:method ((slot sb-pcl::structure-direct-slot-definition) pool1 pool2)
    "Method for structure slots."
    (list
     (let ((reader-name
	     (sb-pcl::slot-definition-defstruct-accessor-symbol slot)))
       (or (find-definition reader-name :function pool1)
	   (find-definition reader-name :function pool2)
	   (make-generic-definition :symbol reader-name :foreignp t))))))

(defgeneric writer-definitions (slot pool1 pool2)
  (:documentation "Return a list of writer definitions for SLOT.")
  (:method (slot pool1 pool2)
    "Default method for class and condition slots."
    (mapcar
     (lambda (writer-name &aux (setfp (listp writer-name)))
       (cond (setfp
	      ;; A SETF form is identified and stored in one of the pools,
	      ;; either as a standalone (toplevel) generic writer, or as part
	      ;; of a generic accessor definition.
	      (setq writer-name (second writer-name))
	      (or (find-definition writer-name :generic-writer pool1)
		  (find-definition writer-name :generic-writer pool2)
		  (make-generic-writer-definition
		   :symbol writer-name :foreignp t)))
	     (t
	      ;; A non SETF form is stored in one of the pools, as a plain
	      ;; generic definition (neither a generic writer, nor a generic
	      ;; accessor) because there's no way to tell that the function is
	      ;; actually a writer (until now).
	      (or (find-definition writer-name :generic pool1)
		  (find-definition writer-name :generic pool2)
		  (make-generic-definition
		   :symbol writer-name :foreignp t)))))
     (slot-property slot :writers)))
  ;; #### PORTME.
  (:method ((slot sb-pcl::structure-direct-slot-definition) pool1 pool2)
    "Method for structure slots."
    (list
     (let ((writer-name
	     (sb-pcl::slot-definition-defstruct-accessor-symbol slot)))
       (or (find-definition writer-name :writer pool1)
	   (find-definition writer-name :writer pool2)
	   (make-writer-definition :symbol writer-name :foreignp t))))))

;; #### PORTME.
(defgeneric definition-combination-users (definition combination)
  (:documentation "Return a list of definitions using method COMBINATION.
The list may boil down to a generic function definition, but may also contain
both a reader and a writer.")
  (:method (definition combination)
    "Default method, for non generic function definitions.
Return nil."
    nil)
  (:method ((definition generic-definition) combination)
    "Method for simple generic and writer definitions."
    (when (eq (sb-pcl::method-combination-type-name
	       (sb-mop:generic-function-method-combination
		(generic-definition-function definition)))
	      combination)
      (list definition)))
  (:method ((definition generic-accessor-definition) combination)
    "Method for generic accessor definitions."
    (nconc (call-next-method)
	   (definition-combination-users
	    (generic-accessor-definition-writer definition) combination))))

(defun pool-combination-users (pool combination)
  "Return a list of all generic definitions in POOL using method COMBINATION."
  (mapcan-definitions-pool
   (lambda (definition) (definition-combination-users definition combination))
   pool))

;; #### NOTE: this finalization step is required for two reasons:
;;   1. it makes it easier to handle cross references (e.g. class inheritance)
;;      because at that time, we know that all definitions have been created,
;;   2. it also makes it easier to handle foreign definitions (that we don't
;;      want to add in the definitions pools) because at that time, we know
;;      that if a definition doesn't exist in the pools, then it is foreign.
;; #### PORTME.
(defun finalize-definitions (pool1 pool2)
  "Finalize the definitions in POOL1 and POOL2.
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
  (labels ((classes-definitions (classes)
	     (mapcar
	      (lambda (name)
		;; #### NOTE: documenting inheritance works here because SBCL
		;; uses classes for reprensenting structures and conditions,
		;; which is not required by the standard. It also means that
		;; there may be intermixing of conditions, structures and
		;; classes in inheritance graphs, so we need to handle that.
		(or  (find-definition name :class pool1)
		     (find-definition name :class pool2)
		     (find-definition name :structure pool1)
		     (find-definition name :structure pool2)
		     (find-definition name :condition pool1)
		     (find-definition name :condition pool2)
		     (make-classoid-definition :symbol name :foreignp t)))
	      (reverse (mapcar #'class-name classes))))
	   (methods-definitions (methods)
	     (mapcar
	      (lambda (method)
		(or  (find-method-definition method pool1)
		     (find-method-definition method pool2)
		     (make-method-definition :symbol (method-name method)
					     :foreignp t)))
	      methods))
	   (compute-combination (generic)
	     (let ((name (sb-pcl::method-combination-type-name
			  (sb-mop:generic-function-method-combination
			   (generic-definition-function generic)))))
	       (setf (generic-definition-combination generic)
		     (or (find-definition name :combination pool1)
			 (find-definition name :combination pool2)
			 (make-combination-definition :symbol name
						      :foreignp t)))))
	   (finalize (pool)
	     (dolist (category '(:class :structure :condition))
	       (dolist (definition (category-definitions category pool))
		 (let ((class (find-class (definition-symbol definition))))
		   (setf (classoid-definition-parents definition)
			 (classes-definitions
			  (sb-mop:class-direct-superclasses class)))
		   (setf (classoid-definition-children definition)
			 (classes-definitions
			  (sb-mop:class-direct-subclasses class)))
		   (setf (classoid-definition-methods definition)
			 (methods-definitions
			  (sb-mop:specializer-direct-methods class)))
		   (dolist (slot (classoid-definition-slots definition))
		     (setf (slot-definition-readers slot)
			   (reader-definitions
			    (slot-definition-slot slot) pool1 pool2))
		     (setf (slot-definition-writers slot)
			   (writer-definitions
			    (slot-definition-slot slot) pool1 pool2))))))
	     (dolist (generic (category-definitions :generic pool))
	       (compute-combination generic)
	       (when (and (generic-accessor-definition-p generic)
			  (generic-accessor-definition-writer generic))
		 (compute-combination
		  (generic-accessor-definition-writer generic))))
	     (dolist
		 (combination (category-definitions :short-combination pool))
	       (let ((operator (sb-pcl::short-combination-operator
				(combination-definition-combination
				 combination))))
		 (setf (short-combination-definition-operator combination)
		       (or (find-definition operator :function pool1)
			   (find-definition operator :function pool2)
			   (find-definition operator :macro pool1)
			   (find-definition operator :macro pool2)
			   ;; #### NOTE: a foreign operator is not necessarily
			   ;; a regular function. However, since we don't
			   ;; actually document those (only print their name),
			   ;; we can just use a function definition here (it's
			   ;; out of laziness).
			   (make-function-definition :symbol operator
						     :foreignp t))))
	       (setf (combination-definition-users combination)
		     (nconc (pool-combination-users
			     pool1 (definition-symbol combination))
			    (pool-combination-users
			     pool2 (definition-symbol combination)))))
	     (dolist
		 (combination (category-definitions :long-combination pool))
	       (setf (combination-definition-users combination)
		     (nconc (pool-combination-users
			     pool1 (definition-symbol combination))
			    (pool-combination-users
			     pool2 (definition-symbol combination)))))
	     ;; #### NOTE: readers and writers belong to the same symbol so
	     ;; they can't be in different pools (i.e. they are both internal
	     ;; or external). Hence, we only need to look in the current pool.
	     (dolist (accessor (category-definitions :accessor pool))
	       (unless (accessor-definition-writer accessor)
		 (setf (accessor-definition-writer accessor)
		       (find-definition (definition-symbol accessor)
					:generic-writer pool))))
	     (dolist (writer (category-definitions :writer pool))
	       (assert (null (writer-definition-reader writer)))
	       (setf (writer-definition-reader writer)
		     (find-definition (definition-symbol writer)
				      :generic-accessor pool)))
	     (dolist (accessor (category-definitions :generic-accessor pool))
	       (unless (generic-accessor-definition-writer accessor)
		 (setf (generic-accessor-definition-writer accessor)
		       (find-definition (definition-symbol accessor)
					:writer pool))))
	     (dolist (writer (category-definitions :generic-writer pool))
	       (assert (null (generic-writer-definition-reader writer)))
	       (setf (generic-writer-definition-reader writer)
		     (find-definition (definition-symbol writer)
				      :accessor pool)))
	     ;; At that point, a short form setf expander definition contains
	     ;; a symbol naming the update object. We now need to transform
	     ;; that into an actual (and possibly foreign) definition.
	     (dolist (expander (category-definitions :setf-expander pool))
	       (let ((name (setf-expander-definition-update expander)))
		 (when (symbolp name)
		   (let ((update-definition
			   (or (find-definition name :function pool1)
			       (find-definition name :function pool2)
			       (find-definition name :generic pool1)
			       (find-definition name :generic pool2)
			       (find-definition name :macro pool1)
			       (find-definition name :macro pool2)
			       ;; #### NOTE: a foreign expander is not
			       ;; necessarily a regular function. However,
			       ;; since we don't actually document those (only
			       ;; print their name), we can just use a
			       ;; function definition here (it's out of
			       ;; laziness).
			       ;; #### FIXME: is using FDEFINITION correct? Or
			       ;; do we risk missing a closure or something
			       ;; like that? Also, see comment at the top of
			       ;; the file, and in the SOURCE method, about
			       ;; the need to fill the FUNCTION slot.
			       (make-function-definition :symbol name
							 :function
							 (fdefinition name)
							 :foreignp t))))
		     ;; #### NOTE: do you see why dropping structures and
		     ;; using mixin classes would help here ? ;-)
		     (etypecase update-definition
		       (macro-definition
			(setf (macro-definition-update-expander
			       update-definition)
			      expander))
		       (function-definition
			(setf (function-definition-update-expander
			       update-definition)
			      expander))
		       (generic-definition
			(setf (generic-definition-update-expander
			       update-definition)
			      expander)))
		     (setf (setf-expander-definition-update expander)
			   update-definition)))))))
    (finalize pool1)
    (finalize pool2)))



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

(defmethod name ((expander setf-expander-definition))
  "Return setf EXPANDER's name, that is (setf <name>)."
  (format nil "(SETF ~A)" (name (setf-expander-definition-symbol expander))))


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
  (definition-source (funcoid-definition-function funcoid)))

(defmethod source
    ((expander setf-expander-definition)
     &aux (update (setf-expander-definition-update expander)))
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
    (definition (source update))))

(defmethod source ((method method-definition))
  "Return METHOD's definition source."
  (definition-source (method-definition-method method)))

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


;; ---------------
;; Source protocol
;; ---------------

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
  (documentation (method-definition-method method) t))

(defmethod docstring ((writer generic-writer-definition))
  "Return generic WRITER's docstring."
  (documentation `(setf ,(definition-symbol writer)) 'function))

(defmethod docstring ((expander setf-expander-definition))
  "Return setf EXPANDER's docstring."
  (documentation (definition-symbol expander) 'setf))

;; #### PORTME.
(defmethod docstring ((slot slot-definition))
  "Return SLOT's docstring."
  (sb-pcl::%slot-definition-documentation (slot-definition-slot slot)))

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
