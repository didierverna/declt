;;; symbol.lisp --- Symbol based documentation

;; Copyright (C) 2010-2013, 2015-2017, 2020, 2021 Didier Verna

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


;;; Code:

(in-package :net.didierverna.declt)
(in-readtable :net.didierverna.declt)


;; ==========================================================================
;; Rendering protocols
;; ==========================================================================

;; #### NOTE: not all symbol-definitions may be setf ones, but for simplicity,
;; we define a single method here, and detect setf definitions, not by testing
;; the setf slot (which does not always exist), but by testing the type of the
;; definition's name.
(defmethod safe-name
    ((definition symbol-definition)
     &optional qualified
     &aux (name (reveal (princ-to-string (definition-symbol definition))))
	  (setfp (consp (name definition))))
  "Reveal symbol DEFINITION's name, possibly QUALIFIED.
A QUALIFIED name is of the form \"package:[:]symbol\", maybe in a setf list.
Uninterned symbols are denoted by the ∅ package."
  (when qualified
    (let ((home-package (home-package definition)))
      (setq name (concatenate 'string
		   (reveal (if home-package (name home-package) ""))
		   (if (publicp definition) ":" "::")
		   name))))
  (if setfp
    ;; #### NOTE: we don't reveal the space between SETF and the symbol,
    ;; because that would look rather weird in the output. Consequently, Declt
    ;; must expect to get names with unescaped spaces. @DEFFN, @DEFFNX, AND
    ;; @DEFTP take care of protecting their NAME argument with braces because
    ;; of that.
    ;; #### NOTE: Hack for future case-preserving implementation.
    (format nil "(~A ~A)" 'setf name)
    name))




;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun render-package-reference
    (definition context
     &optional force
     &aux (home-package (home-package definition)))
  "Render a reference to DEFINITION's home package definition in CONTEXT.
When FORCE, render a reference to the Common Lisp package, even if CONTEXT
says otherwise.
Possibly render an \"uninterned\" mention instead of an actual reference,
when there is no home package to reference."
  (when (or (not (eq home-package (find-package :common-lisp)))
	    (default-values context)
	    force)
    (item ("Package")
      (if home-package
	(reference home-package t)
	(format t "@i{none (uninterned)}.~%")))))

(defun render-definition-core (definition context)
  "Render DEFINITION's documentation core in CONTEXT.
More specifically, render DEFINITION's package and source file references."
  (render-package-reference definition context)
  (when-let (source (source-file definition))
    (item ("Source") (reference source t))))




;; ==========================================================================
;; Documentation Protocols
;; ==========================================================================

;; #### NOTE: all the indexing methods below perform sub-indexing only,
;; because the main index entries are created automatically in Texinfo by the
;; @defXXX routines.

;; -------
;; Varoids
;; -------

(defmethod document :open ((definition varoid-definition) context &key)
  "Open varoid DEFINITIONS's documentation environment in CONTEXT.
More specifically:
- open a @defvr environment,
- anchor and index DEFINITION,
- render DEFINITION's docstring,
- open a @table environment."
  (@defvr (string-capitalize (category-name definition))
      ;; #### WARNING: casing policy.
      (string-downcase (safe-name definition)))
  (anchor-and-index definition)
  (render-docstring definition)
  (@table))

;; #### NOTE: my first feeling about type conditionals in methods is that they
;; are kludgy workarounds for ill-designed hierarchies, or not expressive
;; enough method combinations. On the other hand, I'm not so sure anymore. The
;; advantage I see here is that it makes /exceptions/ very explicit in the
;; code, instead of scattering (obscuring) the behavior, as is often the case
;; in OO.
(defmethod document ((definition varoid-definition) context &key)
  "Render varoid DEFINITION's documentation in CONTEXT.
More specifically, render DEFINITION's package and source file references.
As a special exception, slots don't reference their package, unless it differs
from the slot's owner package, and never reference their source file, which is
the same as their owner."
  (typecase definition
    (slot-definition
     (unless (eq (home-package definition) (home-package (owner definition)))
       (render-package-reference definition context t)))
    (otherwise
     (render-package-reference definition context)
     (when-let (source (source-file definition))
       (item ("Source") (reference source t))))))

(defmethod document :close ((definition varoid-definition) context &key)
  "Close varoid DEFINITION's documentation environment in CONTEXT.
More specifically:
- close the @table environment,
- close the @defvr environment."
  (@end :table)
  (@end :defvr))



;; Constants
(defmethod category-name ((definition constant-definition))
  "Return \"constant\"."
  "constant")

(defmethod index-command-name ((definition constant-definition))
  "Return \"constantsubindex\"."
  "constantsubindex")



;; Special variables
(defmethod category-name ((definition special-definition))
  "Return \"special variable\"."
  "special variable")

(defmethod index-command-name ((definition special-definition))
  "Return \"specialsubindex\"."
  "specialsubindex")



;; Symbol macros
(defmethod category-name ((definition symbol-macro-definition))
  "Return \"symbol macro\"."
  "symbol macro")

(defmethod index-command-name ((definition symbol-macro-definition))
  "Return \"symbolmacrosubindex\"."
  "symbolmacrosubindex")


;; #### NOTE: slots are defined as varoids, but they will appear later on,
;; along with classoids.



;; --------
;; Funcoids
;; --------

;; Utilities
;; ---------
(defun merge-expander-p (definition expander)
  "Return T if function DEFINITION and setf EXPANDER can be documented jointly."
  ;; #### NOTE: a function and its expander share the same symbol, hence
  ;; package. The rest needs to be checked. Also, we don't want to merge short
  ;; form setf expanders because we have additional implementation details to
  ;; advertise (the writer operator).
  (and definition expander
       (typep expander 'long-expander-definition)
       (equal (source-file definition) (source-file expander))
       (equal (docstring definition) (docstring expander))))

;; #### TODO: there's the question of offering the option to qualify symbols.
(defun safe-lambda-list (lambda-list)
  "Return a safe LAMBDA-LIST, suitable to pass to Texinfo.
The original lambda-list's structure is preserved, but all symbols are
converted to revealed strings, and initform / supplied-p data is removed."
  (loop :with post-mandatory
	:for rest :on lambda-list
	:for element := (if (and (listp (car rest)) post-mandatory)
			  (if (listp (first (car rest)))
			    ;; keywords may also have an associated var
			    (first (first (car rest)))
			    (first (car rest)))
			  (car rest))
	:if (listp element)
	  :collect (safe-lambda-list element) :into safe-lambda-list
	:else :if (member element '(&optional &rest &key &allow-other-keys
				    &aux &environment &whole &body))
		;; #### WARNING: casing policy.
		:collect (string-downcase element) :into safe-lambda-list
		:and :do (setq post-mandatory t)
			 ;; #### WARNING: casing policy.
	:else :collect (reveal (string-downcase element))
		:into safe-lambda-list
	:finally (progn (when rest ;; dotted list
			  (setf (cdr (last safe-lambda-list))
				;; #### WARNING: casing policy.
				(reveal (string-downcase rest))))
			(return safe-lambda-list))))

(defun render-headline (definition)
  "Render a headline for DEFINITION. Also anchor and index it."
  (@deffnx (string-capitalize (category-name definition))
      (string-downcase (safe-name definition))
    (safe-lambda-list (lambda-list definition)))
  (anchor-and-index definition))



;; General / default behavior
(defmethod document :open ((definition funcoid-definition) context &key)
  "Open funcoid DEFINITION's documentation environment in CONTEXT.
More specifically:
- open a @deffn environment,
- anchor and index DEFINITION,
- render DEFINITION's docstring,
- open a @table environment,
- render DEFINITION's core documentation.
This is the default method."
  (@deffn (string-capitalize (category-name definition))
    ;; #### WARNING: casing policy.
    (string-downcase (safe-name definition))
    (safe-lambda-list (lambda-list definition)))
  (anchor-and-index definition)
  (render-docstring definition)
  (@table)
  (render-definition-core definition context))

(defmethod document :close ((definition funcoid-definition) context &key)
  "Close funcoid DEFINITION's documentation environment in CONTEXT.
More specifically:
- close the @table environment,
- close the @deffn environment."
  (@end :table)
  (@end :deffn))



;; Setfable funcoids
(defmethod document :around
    ;; #### NOTE: using the rest argument below allows us to chain :around
    ;; methods without worrying too much about their order.
    ((definition setfable-funcoid-definition) context &rest args &key)
  "Check for potential expander merging of setfable funcoid DEFINITION."
  (apply #'call-next-method
    definition context
    :merge-expander (merge-expander-p definition (expander-for definition))
    args))

(defmethod document :open
    ((definition setfable-funcoid-definition) context
     &key merge-expander writer)
  "Open setfable funcoid DEFINITION's documentation environment in CONTEXT.
More specifically:
- open a @deffn environment, possibly merging a related setf expander or
  writer,
- anchor and index DEFINITION,
- render DEFINITION's docstring,
- open a @table environment,
- render DEFINITION's core documentation."
  (@deffn (string-capitalize (category-name definition))
    ;; #### WARNING: casing policy.
    (string-downcase (safe-name definition))
    (safe-lambda-list (lambda-list definition)))
  (anchor-and-index definition)
  (when writer (render-headline writer))
  (when merge-expander (render-headline (expander-for definition)))
  (render-docstring definition)
  (@table)
  (render-definition-core definition context))

(defmethod document
    ((definition setfable-funcoid-definition) context
     &key merge-expander
     &aux (expander-for (expander-for definition))
	  (category (category-name definition)))
  "Render setfable funcoid DEFINITION's expanders information in CONTEXT.
More specifically:
- render a reference to a set expander for DEFINITION, unless the definitions
  are merged,
- render references to all setf expanders expanding to DEFINITION."
  (when (and expander-for (not merge-expander))
    (item ((format nil "Setf expander for this ~A" category))
      (reference expander-for t)))
  (when-let (expanders-to (expanders-to definition))
    (render-references (format nil "Setf expanders to this ~A" category)
      ;; #### WARNING: casing policy.
      (sort expanders-to #'string-lessp :key #'definition-symbol)
      t)))



;; Accessor mixin
(defmethod document ((definition accessor-mixin) context &key)
  "Render accessor mixin DEFINITION's target slot reference in CONTEXT."
  (item ("Target Slot") (reference (target-slot definition) t)))



;; Macros
(defmethod category-name ((definition macro-definition))
  "Return \"macro\"."
  "macro")

(defmethod index-command-name ((definition macro-definition))
  "Return \"macrosubindex\"."
  "macrosubindex")



;; Compiler macros
(defmethod category-name ((definition compiler-macro-definition))
  "Return \"compiler macro\"."
  "compiler macro")

(defmethod index-command-name ((definition compiler-macro-definition))
  "Return \"compilermacrosubindex\"."
  "compilermacrosubindex")



;; Types
(defmethod category-name ((definition type-definition))
  "Return \"type\"."
  "type")

(defmethod index-command-name ((definition type-definition))
  "Return \"typesubindex\"."
  "typesubindex")

;; #### WARNING: a type is a funcoid because it has a lambda list, but it's
;; really a @deftp, not a @deffn.
(defmethod document :open ((definition type-definition) context &key)
  "Open type DEFINITION's documentation environment in CONTEXT.
More specifically:
- open a @deftp environment,
- anchor and index DEFINITION,
- render DEFINITION's docstring,
- open a @table environment,
- render DEFINITION's core."
  (@deftp "Type"
    ;; #### WARNING: casing policy.
    (string-downcase (safe-name definition))
    (safe-lambda-list (lambda-list definition)))
  (anchor-and-index definition)
  (render-docstring definition)
  (@table)
  (render-definition-core definition context))

(defmethod document :close ((definition type-definition) context &key)
  "Close type DEFINITION's documentation environment in CONTEXT.
More specifically:
- close the @table environment,
- close the @deftp environment."
  (@end :table)
  (@end :deftp))



;; Setf expanders
(defmethod category-name ((expander expander-definition))
  "Return \"setf expander\"."
  "setf expander")

(defmethod index-command-name ((expander expander-definition))
  "Return \"expandersubindex\"."
  "expandersubindex")

(defmethod document :around
    ((definition long-expander-definition) context &key)
  "Prevent long expander DEFINITION from being documented when merging."
  (unless (merge-expander-p (standalone-reader definition) definition)
    (call-next-method)))

(defmethod document ((definition expander-definition) context &key)
  "Render setf expander DEFINITION's standalone reader reference in CONTEXT."
  (when-let (standalone-reader (standalone-reader definition))
    (item ("Reader") (reference standalone-reader))))

(defmethod document ((definition short-expander-definition) context &key)
  "Render short expander DEFINITION's standalone writer reference in CONTEXT."
  (let ((standalone-writer (standalone-writer definition)))
    (cond (standalone-writer
	   (item ("Writer") (reference standalone-writer)))
	  ((not (foreignp definition))
	   (item ("Writer") (princ "@i{missing}"))))))



;; Ordinary functions
(defmethod category-name ((definition ordinary-function-definition))
  "Return \"function\"."
  "function")

(defmethod index-command-name ((definition ordinary-function-definition))
  "Return \"functionsubindex\"."
  "functionsubindex")

(defun merge-ordinary-writer (reader writer)
  "Return WRITER if it can be documented jointly with READER."
  ;; #### NOTE: structure accessors necessarily share the same package and
  ;; source. The rest needs to be checked.
  (when (and reader writer
	     ;; #### NOTE: in SBCL, ordinary writers are always setf function,
	     ;; to we know that there's no expander-mixin contents for them.
	     (not (expander-for reader))
	     (not (expanders-to reader))
	     (equal (docstring reader) (docstring writer)))
    writer))

(defmethod category-name ((definition ordinary-reader-definition))
  "Return \"reader\"."
  "reader")

(defmethod document :around
    ;; #### NOTE: using the rest argument below allows us to chain :around
    ;; methods without worrying too much about their order.
    ((definition ordinary-reader-definition) context
     &rest args &key
     &aux (writer (first (writers (target-slot definition)))))
  "Check for potential writer merging with ordinary reader DEFINITION."
  (apply #'call-next-method
    definition context
    :writer (merge-ordinary-writer definition writer)
    args))

(defmethod category-name ((definition ordinary-writer-definition))
  "Return \"writer\"."
  "writer")

(defmethod document :around
    ((definition ordinary-writer-definition) context &key)
  "Prevent ordinary writer DEFINITION from being documented when merging."
  (unless (merge-ordinary-writer (first (readers (target-slot definition)))
				 definition)
    (call-next-method)))



;; Generic functions
(defmethod category-name ((definition generic-function-definition))
  "Return \"generic function\"."
  "generic function")

(defmethod index-command-name ((definition generic-function-definition))
  "Return \"genericsubindex\"."
  "genericsubindex")

(defmethod document
    ((definition generic-function-definition) context
     &key
     &aux (combination (combination definition)))
  "Render generic function DEFINITION's combination reference in CONTEXT."
  (when (and combination
	     (or (not (eq (name combination) 'standard))
		 (default-values context)))
    (item ("Method Combination")
      (reference combination t)
      (terpri)
      (when-let (options (mapcar (lambda (option)
				   ;; #### FIXME: see TODO on format-tables.
				   ;; #### WARNING: casing policy.
				   (escape (format nil "~(~S~)" option)))
			   (combination-options definition)))
	(table () (item ("Options") (format t "~{@t{~A}~^, ~}" options)))))))


(defmethod document :close
    ((definition generic-function-definition) context &key merged-methods)
"Close generic function DEFINITION's documentation.
More specifically:
- render DEFINITION's method references, possibly merging readers and writers,
- close the @table environment,
- close the @deffn environment."
  (if merged-methods
    (item ("Methods")
      (dolist (accessors (first merged-methods))
	(document (car accessors) context :writer (cdr accessors)))
      (dolist (reader-method (second merged-methods))
	(document reader-method context))
      (dolist (writer-method (third merged-methods))
	(document writer-method context)))
    (when-let ((methods (methods definition)))
      (item ("Methods") (dolist (method methods) (document method context)))))
  (@end :table)
  (@end :deffn))

;; #### NOTE: for generic accessors merging, we don't try to be extremely
;; clever, because it's probably pointless. For example, there's no merging
;; between explicit :reader and :writer definitions when they are separate.
;; The only thing that's attempted is to merge :accessor definitions, that is
;; foo and (setf foo).
(defun merge-methods
    (reader writer
     &aux (reader-methods (methods reader))
	  (writer-methods (methods writer))
	  accessors readers reader-method writer-method)
  "Attempt to merge READER and WRITER generic definitions methods. See
`merge-generic-writer' for the exact conditions under which merging may
occur. If merging is possible, return a list of three values:
1. a list of the form ((READER-METHOD . WRITER-METHOD) ...) for associated
   reader and writer methods,
2. a list of standalone readers,
3. a list of standalone writers,
Otherwise, return NIL."
  (while (setq reader-method (pop reader-methods))
    (setq writer-method (find (target-slot reader-method) writer-methods
			      :key #'target-slot))
    (if writer-method
      (cond ((and (equal (docstring reader-method) (docstring writer-method))
		  (eq (source-file reader-method) (source-file writer-method)))
	     (setq writer-methods (remove writer-method writer-methods))
	     (endpush (cons reader-method writer-method) accessors))
	    (t (return-from merge-methods)))
      (endpush reader-method readers)))
  (list accessors readers writer-methods))

(defun merge-generic-writer (reader writer)
  "Check if WRITER generic definition can be documented jointly with READER.
If so, return the generalized Boolean value of `merge-methods', which see.

Merging is only attempted on generic functions defined exclusively via slot
:accessor keywords. For merging to actually occur, there must not exist any
property specific to only one definition, or different between the two (no
related expander information, same method combination, same docstring, etc.).
The only exception is their lambda lists.

The same conditions apply to methods, which definitions are also merged. Only
unqualified methods must exist. Standalone reader and writer methods are still
permitted."
  ;; #### NOTE: since we constraint WRITER to be of the form (SETF READER),
  ;; there's no need to check for package equality between the two.
  (and reader writer
       (typep reader 'generic-reader-definition)
       (typep writer 'generic-writer-definition)
       (not (expander-for reader))
       (not (expanders-to reader))
       (eq (combination reader) (combination writer))
       (equal (docstring reader) (docstring writer))
       (eq (source-file reader) (source-file writer))
       (notany #'qualifiers (methods reader))
       (notany #'qualifiers (methods writer))
       (merge-methods reader writer)))

(defmethod category-name ((definition generic-reader-definition))
  "Return \"generic reader\"."
  "generic reader")

(defmethod document :around
    ;; #### NOTE: using the rest argument below allows us to chain :around
    ;; methods without worrying too much about their order.
    ((definition generic-reader-definition) context
     &rest args &key
     &aux (writer (find (list 'setf (name definition))
		      (mapcar #'owner
			(mapcat #'writers
			  (mapcar #'target-slot (methods definition))))
		    :key #'name :test #'equal))
	  (merged-methods (merge-generic-writer definition writer)))
  "Check for potential writer merging with generic reader DEFINITION."
  (apply #'call-next-method
    definition context
    :writer (when merged-methods writer)
    :merged-methods merged-methods
    args))

(defmethod category-name ((definition generic-writer-definition))
  "Return \"generic writer\"."
  "generic writer")

(defmethod document :around
    ((definition generic-writer-definition) context &key)
  "Prevent generic writer DEFINITION from being documented when merging."
  (unless (and (setfp definition)
	       (merge-generic-writer
		(find (definition-symbol definition)
		    (mapcar #'owner
		      (mapcat #'readers
			(mapcar #'target-slot (methods definition))))
		  :key #'definition-symbol)
		definition))
    (call-next-method)))



;; Method combinations
(defmethod category-name ((definition combination-definition))
  "Return \"method combination\"."
  "method combination")

(defmethod index-command-name ((definition combination-definition))
  "Return \"combinationsubindex\"."
  "combinationsubindex")

(defmethod document ((definition combination-definition) context &key)
  "Render method combination DEFINITION's client references in CONTEXT."
  (render-references "Client Functions"
    ;; #### WARNING: casing policy.
    (sort (clients definition) #'string-lessp :key #'definition-symbol)
    t))

(defmethod document ((definition short-combination-definition) context &key)
  "Render short method combination DEFINITION's documentation in CONTEXT."
  (let ((standalone-combinator (standalone-combinator definition)))
    (cond (standalone-combinator
	   (item ("Operator") (reference standalone-combinator)))
	  ((not (foreignp definition))
	   (item ("Operator") (princ "@i{missing}")))))
  (item ("Identity with one argument")
    (format t "@t{~(~A~)}" (identity-with-one-argument definition))))



;; Methods
(defmethod safe-name :around
    ((definition method-definition)
     &optional qualified
     &aux (safe-name (call-next-method)))
  "When QUALIFIED, append method DEFINITION's qualifiers and specializers."
  (if qualified
    (concatenate 'string
      safe-name
      ;; #### NOTE: I'm using an S for qualifiers, assuming they'll always be
      ;; symbols, in order to distinguish keywords from the rest.
      (format nil "~{ ~S~} (~{~A~^ ~})"
	(qualifiers definition)
	(mapcar (lambda (specializer)
		  (typecase specializer
		    (definition (safe-name specializer t))
		    ;; EQL specializer
		    (otherwise (format nil "~S" specializer))))
	  (specializers definition))))
    safe-name))

(defmethod category-name ((definition method-definition))
  "Return \"method\"."
  "method")

(defmethod category-name ((definition reader-method-definition))
  "Return \"reader method\"."
  "reader method")

(defmethod category-name ((definition writer-method-definition))
  "Return \"writer method\"."
  "writer method")

(defmethod index-command-name ((definition method-definition))
  "Return \"methodsubindex\"."
  "methodsubindex")

;; #### WARNING: in theory, it is possible to specialize the first argument of
;; a setf method implemented explicitly (I'm not sure what happens for
;; accessors generated automatically on typed slots), so when we use only the
;; specializers rest below, we may loose information. I don't think this is a
;; serious issue, tho. But perhaps it's a design choice that would gain being
;; customizable.
;; #### PORTME.
(defun safe-specializers
    (definition &aux (specializers (specializers definition)))
  "Return a list of safe specializers for method DEFINITION.
A safe specializer is the printed form of either a reference to a class
definition, or an EQL specializer's type name. For setf and writer
definitions, only the specializers rest is used, as these methods get the new
value as their first argument."
  (when (or (setfp definition) (typep definition 'writer-method-definition))
    (setq specializers (cdr specializers)))
  (loop :for rest :on specializers
	:for specializer := (car rest)
	:collect (typecase specializer
		   (definition
		    (with-output-to-string (*standard-output*)
		      (reference specializer t (when (cdr rest) #\,))))
		   ;; #### WARNING: casing policy.
		   (otherwise (format nil "~(~S~)~:[~;, ~]"
				(sb-pcl::specializer-type specializer)
				(cdr rest))))))

;; #### NOTE: methods don't advertise their package, as it is the same as that
;; of their owner.
;; #### TODO: except that we need to rethink this when methods will get the
;; ability to be documented as toplevel entities (e.g. for foreign
;; definitions).
(defmethod document :open
    ((definition method-definition) context &key writer)
  "Open method DEFINITION's documentation in CONTEXT.
More specifically:
- open a @deffn environment, possibly merging a WRITER method,
- anchor and index DEFINITION,
- render DEFINITION's docstring,
- open a @table environment,
- render DEFINITION's source file, unless identical to that of the owner."
  (@deffn (string-capitalize (category-name definition))
    ;; #### WARNING: casing policy.
    (string-downcase (safe-name definition))
    (safe-specializers definition)
    (when-let (qualifiers (qualifiers definition))
      (format nil "~(~{~S~^ ~}~)" qualifiers)))
  (anchor-and-index definition)
  (when writer
    (@deffnx (string-capitalize (category-name writer))
	;; #### WARNING: casing policy.
	(string-downcase (safe-name writer))
      (safe-specializers writer)
      (when-let (qualifiers (qualifiers writer))
	(format nil "~(~{~S~^ ~}~)" qualifiers)))
    (anchor-and-index writer))
  (render-docstring definition)
  (@table)
  (when-let (source-file (source-file definition))
    (unless (equal source-file (source-file (owner definition)))
      (item ("Source") (reference source-file t)))))



;; ---------
;; Classoids
;; ---------

(defmethod document :open ((definition classoid-definition) context &key)
  "Open classoid DEFINITION's documentation environment in CONTEXT.
More specifically:
- open a @deftp environment,
- anchor and index DEFINITION,
- render DEFINITION's docstring,
- open a @table environment,
- render DEFINITIONS's core documentation."
  (@deftp (string-capitalize (category-name definition))
    ;; #### WARNING: casing policy.
    (string-downcase (safe-name definition)))
  (anchor-and-index definition)
  (render-docstring definition)
  (@table)
  (render-definition-core definition context))

(defmethod document ((definition classoid-definition) context &key)
  "Render classoid DEFINITION's direct slots references in CONTEXT."
  (when-let (direct-slots (direct-slots definition))
    (item ("Direct slots")
      (dolist (direct-slot direct-slots)
	(document direct-slot context)))))

(defmethod document :close ((definition classoid-definition) context &key)
  "Close classoid DEFINITION's documentation environment in CONTEXT.
More specifically:
- close the @table environment,
- close the @deftp environment."
  (@end :table)
  (@end :deftp))



;; CLOS classoid mixins
(defmethod document ((definition clos-classoid-mixin) context &key)
  "Render CLOS classoid mixin DEFINITION's documentation in CONTEXT.
More specifically, render DEFINITION's direct superclasses, subclasses,
methods, and initargs references."
  ;; #### TODO: we may want to change the titles below to display not only
  ;; "classes", but "structures" and "conditions" directly.
  (let ((direct-superclassoids (direct-superclassoids definition)))
    (unless (default-values context)
      (setq direct-superclassoids
	    ;; #### TODO: this is not completely correct. What about non-empty
	    ;; superclass lists that do contain standard-object explicitly?
	    (remove 'standard-object direct-superclassoids :key #'name)))
    (render-references "Direct superclasses"
      ;; #### WARNING: casing policy.
      (sort direct-superclassoids #'string-lessp :key #'definition-symbol)
      t))
  (render-references "Direct subclasses"
    ;; #### WARNING: casing policy.
    (sort (direct-subclassoids definition) #'string-lessp
      :key #'definition-symbol)
    t)
  (render-references "Direct methods"
    ;; #### WARNING: casing policy.
    (sort (direct-methods definition) #'string-lessp
      :key #'definition-symbol)
    t)
  (when-let (initargs (direct-default-initargs definition))
    (item ("Direct Default Initargs")
      ;; #### FIXME: we should rather compute the longest initarg name and use
      ;; that as a template size for the @headitem specification.
      (multitable (.3f0 .5f0)
	(format t "@headitem Initarg @tab Value~%")
	(dolist (initarg initargs)
	  (format t "@item @t{~A}~%@tab @t{~A}~%"
	    ;; #### WARNING: casing policy.
	    ;; The ~S is to preserve the appearance of keywords.
	    (escape (format nil "~(~S~)" (first initarg)))
	    (escape (format nil "~(~A~)" (second initarg)))))))))



;; Structures
(defmethod category-name ((definition structure-definition))
  "Return \"structure\"."
  "structure")

(defmethod index-command-name ((definition structure-definition))
  "Return \"structuresubindex\"."
  "structuresubindex")

(defmethod document ((definition typed-structure-definition) context &key)
  "Render typed structure DEFINITION's type documentation in CONTEXT."
  (item ("Type")
    (if (eq (element-type definition) t)
      ;; #### WARNING: casing policy.
      (format t "@t{~(~S~)}~%" (structure-type definition))
      (format t "@t{(vector ~(~A~))}~%" (element-type definition)))))



;; Conditions
(defmethod category-name ((definition condition-definition))
  "Return \"condition\"."
  "condition")

(defmethod index-command-name ((definition condition-definition))
  "Return \"conditionsubindex\"."
  "conditionsubindex")



;; Classes
(defmethod category-name ((definition class-definition))
  "Return \"class\"."
  "class")

(defmethod index-command-name ((definition class-definition))
  "Return \"classsubindex\"."
  "classsubindex")



;; Slots
(defmethod safe-name :around
    ((definition slot-definition)
     &optional qualified
     &aux (safe-name (call-next-method)))
  "When QUALIFIED, prepend slot DEFINITION's classoid safe name."
  (if qualified
    (concatenate 'string (safe-name (owner definition) t) "->" safe-name)
    safe-name))

(defmethod category-name ((definition slot-definition))
  "Return \"slot\"."
  "slot")

(defmethod index-command-name ((definition slot-definition))
  "Return \"slotsubindex\"."
  "slotsubindex")

(defmethod document ((definition slot-definition) context &key)
  "Render slot DEFINITION's documentation in context.
More specifically, render DEFINITION's value type, and for CLOS slots render
allocation, initform, and initargs."
  (let ((value-type (value-type definition)))
    (when (or (not (eq value-type t)) (default-values context))
      (item ("Type")
	(format t "@t{~A}~%"
	  ;; #### WARNING: casing policy.
	  (escape (format nil "~(~S~)" value-type))))))
  ;; Somewhat kludgy, but we want those close to the type definition.
  (when (typep definition 'clos-slot-definition)
    (flet ((render (value)
	     (format t "@t{~A}~%"
	       ;; #### WARNING: casing policy.
	       (escape (format nil "~(~S~)" value)))))
      (let ((allocation (allocation definition)))
	(when (or (not (eq allocation :instance)) (default-values context))
	  (item ("Allocation") (render allocation))))
      (when-let (initform (initform definition))
	(item ("Initform") (render initform)))
      (when-let (initargs (initargs definition))
	(item ("Initargs")
	  ;; #### FIXME: format mess. There's gotta be a better way.
	  (let ((values (mapcar (lambda (val)
				  ;; #### WARNING: casing policy.
				  (escape (format nil "~(~S~)" val)))
			  initargs)))
	    (format t "@t{~A}~{, @t{~A}~}"
	      (first values)
	      (rest values))))))))

(defmethod document ((definition clos-slot-definition) context &key)
  "Render CLOS slot DEFINITION's documentation in CONTEXT.
More specifically, render DEFINITION's reader and writer references."
  (render-references "Readers"
    ;; #### WARNING: casing policy.
    (sort (readers definition) #'string-lessp :key #'definition-symbol)
    t)
  (if (and (readers definition) (not (writers definition)))
    (item ("Writers") (format t "@i{This slot is read-only.}~%"))
    (render-references "Writers"
      ;; #### WARNING: casing policy.
      (sort (writers definition) #'string-lessp :key #'definition-symbol)
      t)))

;; #### TODO: if/when we update RENDER-REFERENCES to handle plural, we can
;; factor this out with the above, and hence put this in the general method.
(defmethod document ((definition typed-structure-slot-definition) context &key)
  "Render typed structure slot DEFINITION's documentation in CONTEXT.
More specifically, render DEFINITION's reader and writer references."
  (render-references "Reader" (readers definition) t)
  (if (and (readers definition) (not (writers definition)))
    (item ("Writer") (format t "@i{This slot is read-only.}~%"))
    (render-references "Writer" (writers definition) t)))



;; -------
;; Aliases
;; -------

(defmethod category-name ((definition alias-definition))
  "Return the category name of alias DEFINITION's referee."
  (category-name (referee definition)))

(defmethod index-command-name ((definition alias-definition))
  "Return the index command name of alias DEFINITION's referee."
  (index-command-name (referee definition)))

(defmethod document ((definition alias-definition) context &key)
  "Render alias DEFINITION's documentation in CONTEXT."
  (deffn ((string-capitalize (category-name definition))
	  ;; #### WARNING: casing policy.
	  (string-downcase (safe-name definition))
	  (safe-lambda-list (lambda-list definition)))
    (anchor-and-index definition)
    (render-docstring definition)
    (table ()
      (render-definition-core definition context)
      (item ("Alias for") (reference (referee definition) t)))))




;; ==========================================================================
;; Definition Nodes
;; ==========================================================================

;; ----------
;; Categories
;; ----------

;; #### NOTE: the order in *CATEGORIES* is important (see
;; ADD-CATEGORIES-NODE). It conditions the order of appearance of the
;; definitions in the generated manual.

;; #### TODO: not all retained definitions end up being documented at the
;; toplevel, in particular when merging occurs, or depending of whether we
;; document foreign definitions or standard/default values. The current policy
;; is to leave those definitions lurking around in the nodes structure, and
;; let the DOCUMENT protocol decide on what to do with them (see some of the
;; :around methods for instance). There is one exception to this policy, which
;; is when undocumented definitions would lead to an empty section in the
;; reference manual. This currently happens only with setf expanders (see
;; below). In the future, we may want to refine this policy, and figure out
;; exactly when it would be most appropriate to filter definitions out.

(defparameter *categories*
  `(("constants" constant-definition)
    ("special variables" special-definition)
    ("symbol macros" symbol-macro-definition)
    ("macros"
     ,(lambda (definition)
	(or (typep definition 'macro-definition)
	    (typep definition 'macro-alias-definition))))
    ("compiler macros"
     ,(lambda (definition)
	(or (typep definition 'compiler-macro-definition)
	    (typep definition 'compiler-macro-alias-definition))))
    ("setf expanders" expander-definition)
    ("ordinary functions"
     ,(lambda (definition)
	(or (typep definition 'ordinary-function-definition)
	    (and (typep definition 'function-alias-definition)
		 (typep (referee definition) 'ordinary-function-definition)))))
    ("generic functions"
     ,(lambda (definition)
	(or (typep definition 'generic-function-definition)
	    (and (typep definition 'function-alias-definition)
		 (typep (referee definition) 'generic-function-definition)))))
    ("method combinations" combination-definition)
    ("conditions" condition-definition)
    ("structures" structure-definition)
    ("classes" class-definition)
    ("types" type-definition))
  "The list of definition categories.
Each category is of the form (TITLE FILTER).
- TITLE (a string) serves as the section title.
- FILTER can be either a definition type (symbol), in which case definitions
  of that type a retained, or a predicate function of one (definition)
  argument, which should return T if the definition is to be retained.")

(defun add-category-node
    (parent context status category definitions
     ;; #### WARNING: in some cases, an existing definition will not be
     ;; documented directly. For example, setf expanders could be merged with
     ;; their standalone reader, the standard method combination would be
     ;; skipped, etc. In some cases, this could result in the creation of an
     ;; empty documentation section, even though the list of definitions is
     ;; not nil. Rather than duplicating all the tests here, we simply let the
     ;; documentation protocol proceed, and check afterwards that something
     ;; was actually rendered before adding a new node for good.
     &aux (contents (render-to-string
		      (dolist (definition (sort definitions #'string-lessp
					    :key #'definition-symbol))
			(document definition context)))))
  "Add the STATUS CATEGORY node to PARENT for DEFINITIONS in CONTEXT."
  (unless (zerop (length contents))
    (add-child parent
      (make-node :name (format nil "~@(~A ~A~)" status category)
		 :section-name (format nil "~@(~A~)" category)
		 :before-menu-contents contents))))

(defun add-categories-node (parent context status definitions)
  "Add the STATUS DEFINITIONS categories nodes to PARENT in CONTEXT."
  (dolist (category *categories*)
    (let ((filter (etypecase (second category)
		    (symbol (lambda (definition)
			      (typep definition (second category))))
		    (function (second category)))))
      (when-let (type-definitions (remove-if-not filter definitions))
	(add-category-node parent context status (first category)
			   type-definitions)))))

(defun add-definitions-node
    (parent extract context
     &aux (public-definitions (public-definitions extract))
	  (public-definitions-number (length public-definitions))
	  (private-definitions (private-definitions extract))
	  (private-definitions-number (length private-definitions)))
  "Add EXTRACT's definitions node to PARENT in CONTEXT."
  (unless (zerop (+ public-definitions-number private-definitions-number))
    (let ((definitions-node
	    (add-child parent
	      (make-node :name "Definitions"
			 :synopsis "The symbols documentation"
			 :before-menu-contents(format nil "~
Definitions are sorted by export status, category, package, and then by
lexicographic order.")))))
      (unless (zerop public-definitions-number)
	(let ((node (add-child definitions-node
		      (make-node :name "Public Interface"))))
	  (add-categories-node node context "public" public-definitions)))
      (unless (zerop private-definitions-number)
	(let ((node (add-child definitions-node
		      (make-node :name "Internals"))))
	  (add-categories-node node context "private" private-definitions))))))

;;; symbol.lisp ends here
