;; finalize.lisp --- Definitions finalization

;; Copyright (C) 2021 Didier Verna

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

;; The finalization step is in charge of completing all the definitions that
;; have been created and collected by the initial introspection process.
;; Finalization occurs in two phases.
;; 1. Phase one is called "stabilization". The purpose of stabilization is
;;    primarily to compute cross-references between definitions. During
;;    stabilization, new definitions may be created (foreign ones, notably).
;;    When this happens, the previously computed cross-references may turn out
;;    to become incomplete. Because of that, stabilization is run over and
;;    over again until no new definition is created.
;; 2. Phase two is called "freezing". The purpose of freezing is to finish
;;    updating the definitions, with information that cannot be computed
;;    unless we know for sure that no new definition will ever be created.

;; #### WARNING: as a general rule of thumb, never assume anything about the
;; finalization state of the other definitions you access when finalizing a
;; definition (in particular, when stabilizing it). There are many small
;; methods here and there that are executed and it's too error-prone to even
;; try to rely on their execution order (scattered behavior, that's the
;; drawback of OO). For example, this is guaranteed to work all the time:
;; (symbol-package (definition-symbol definition) whereas this might fail if
;; the definition has not been stabilized yet: (definition-package
;; (home-package definition)).


;;; Code:

(in-package :net.didierverna.declt.extract)
(in-readtable :net.didierverna.declt)



;; ==========================================================================
;; Stabilization
;; ==========================================================================

(defvar *stabilized* nil
  "Whether the stabilization process is over.
This variable is set to NIL whenever new definitions are created during the
process. Stabilization is run over and over again until nothing moves
anymore.")

(defgeneric stabilize (definition definitions)
  (:documentation "Stabilize DEFINITION in DEFINITIONS.")
  (:method-combination progn))



;; ---------
;; Utilities
;; ---------

(defun make-generic-definition (generic &optional foreign)
  "Make a new GENERIC function definition, possibly FOREIGN."
  (let* ((name (generic-function-name generic))
	 (setf (consp name))
	 (symbol (if setf (second name) name)))
    (make-instance (if setf 'generic-setf-definition 'simple-generic-definition)
      :symbol symbol :generic generic :foreign foreign)))

(defun make-foreign-funcoid-definition
    (name &aux (macro-function (macro-function name)))
  "Make a new foreign macro or function definition for NAME."
  (if macro-function
    (make-macro-definition name macro-function t)
    (make-function-definition name (fdefinition name) :foreign t)))

(defun foreign-funcoid-definition (name)
  "Return a new foreign macro or function definition for NAME, or NIL."
  (when (fboundp name) (make-foreign-funcoid-definition name)))



;; ---------------
;; All Definitions
;; ---------------

(defmethod stabilize progn ((definition definition) definitions)
  "Compute DEFINITION's source file definition."
  (unless (source-file definition)
    (when-let (source-pathname (source-pathname definition))
      (setf (source-file definition)
	    (find* source-pathname definitions
		   :pre-test #'lisp-file-definition-p
		   :key (lambda (candidate)
			  (component-pathname (file candidate)))
		   :test #'equal)))))



;; ------------------
;; Symbol Definitions
;; ------------------

(defmethod stabilize progn
    ((definition symbol-definition) definitions
     &aux (package (symbol-package (definition-symbol definition))))
  "Compute symbol DEFINITION's home package definition.
New foreign package definitions may be created and added at the end of
DEFINITIONS in the process."
  ;; #### NOTE: every symbol definition gets a working package definition;
  ;; even the foreign ones.
  (unless (home-package definition)
    (setf (home-package definition)
	  (or (find-definition package definitions)
	      (let ((package-definition (make-package-definition package t)))
		(endpush package-definition definitions)
		(setq *stabilized* nil)
		package-definition)))))



;; Setf expander mixins
;; --------------------

(defmethod stabilize progn
    ((definition expander-mixin) definitions
     &aux (name (name definition)) ;; always a symbol here
	  (lambda-list (lambda-list definition)))
  "Compute DEFINITION's expander-for and expanders-to references."
  ;; #### NOTE: this definition and a potential expander-for share the same
  ;; symbol. Consequently, if this symbol is one of our own, we /will/ find an
  ;; expander-for in the already existing definitions, if it exists. If, on
  ;; the other hand, if DEFINITION is foreign, then so will the expander-for,
  ;; and thus we don't care if it's not found. To put it differently, we never
  ;; need to create a foreign expander definition here.
  (unless (expander-for definition)
    (setf (expander-for definition)
	  (find-if (lambda (candidate)
		     (and (typep candidate 'expander-definition)
			  ;; don't want the setf part
			  (eq (definition-symbol candidate) name)
			  (equal (lambda-list candidate) lambda-list)))
		   definitions)))
  ;; #### TODO: in the code below, we're looking for expanders-to only in
  ;; the current list of definitions. Although it may contain foreign
  ;; definitions, it could also be incomplete. In order to be exhaustive for
  ;; our own definitions if not for foreign ones, we would need to go through
  ;; all existing symbols.
  ;; #### NOTE: a case could be made to avoid rebuilding the whole list here,
  ;; and only add what's missing, but I don't think it's worth the trouble.
  (setf (expanders-to definition)
	(retain name definitions
	  :pre-test #'short-expander-definition-p
	  :key (lambda (definition) (car (expander definition))))))

;; #### WARNING: there is no stabilization method for the accessor mixin. This
;; is handled by the slot stabilization methods. Slot readers and writers are
;; searched for as regular functions, and when they are found, their
;; respective classes are upgraded.



;; Setf expanders
;; --------------

;; #### WARNING: in fact, take the comments above and below with a grain of
;; salt. Especially because of the way we handle expanders-to above, we
;; actually don't currently create foreign expanders, ever...

(defmethod stabilize progn
    ((definition expander-definition) definitions
     &aux (name (definition-symbol definition))) ;; don't want the setf part
  "Compute setf expander DEFINTIION's standalone reader definition."
  ;; #### NOTE: same remark as above when stabilizing and expander-for
  ;; definition: if this expander is our own, then we /will/ find a definition
  ;; for its standalone-reader if it exists, as it is for the same symbol.
  ;; Otherwise, we don't care if we find a definition or not. So we never need
  ;; to create a foreign standalone reader definition.
  (multiple-value-bind (lambda-list unavailable) (lambda-list definition)
    (unless (or (standalone-reader definition) unavailable)
      (setf (standalone-reader definition)
	    (find-if (lambda (candidate)
		       (and (or (typep candidate 'macro-definition)
				(typep candidate 'function-definition))
			    ;; this will filter out setf functions
			    (eq (name candidate) name)
			    (equal (lambda-list candidate) lambda-list)))
		     definitions)))))

(defmethod stabilize progn
    ((definition short-expander-definition) definitions
     &aux (name (car (expander definition))))
  "Computer short setf expander DEFINITION's standalone writer definition."
  (unless (standalone-writer definition)
    (setf (standalone-writer definition)
	  (find* name definitions
	    :pre-test (lambda (candidate)
			(or (typep candidate 'macro-definition)
			    (typep candidate 'function-definition)))
	    :key #'name))) ;; EQ test will filter out setf functions.
  (unless (or (standalone-writer definition) (foreignp definition))
    (let* ((writer-package
	     (find-definition (symbol-package name) definitions))
	   (writer
	     (unless (and writer-package (not (foreignp writer-package)))
	       (foreign-funcoid-definition name))))
      (cond (writer
	     (setq *stabilized* nil)
	     (endpush writer definitions)
	     (setf (standalone-writer definition) writer))
	    (t
	     (warn "~S: undefined writer for short form setf expander ~S."
		   name (name definition)))))))



;; Generic functions
;; -----------------

;; #### NOTE: contrary to the case of short form setf expanders and short form
;; method combinations, it seems that the method combination object must exist
;; when a generic function is created (I'm getting errors otherwise). This
;; means that if we cannot find the combination definition right now, it must
;; be a foreign one.

;; #### PORTME.
(defmethod stabilize progn
    ((definition generic-function-definition) definitions
     &aux (combination
	   (generic-function-method-combination (generic definition))))
  "Compute generic function DEFINITION's method combination definition."
  (unless (combination definition)
    (setf (combination definition) (find-definition combination definitions)))
  (unless (or (combination definition) (foreignp definition))
    (let ((combination-definition
	    (make-combination-definition
	     (sb-pcl::method-combination-type-name combination)
	     combination t)))
      (setq *stabilized* nil)
      (endpush combination-definition definitions)
      (setf (combination definition) combination-definition))))



;; Method combinations
;; -------------------

;; #### NOTE: after Christophe's changes to SBCL following my ELS paper, I
;; think we can reliably access the method combination object's hashtable of
;; generic functions to compute its users. This is better than than the old
;; way, which was to scan all known generic functions and look at their method
;; combination.

;; #### PORTME.
(defmethod stabilize progn
    ((definition combination-definition) definitions &aux clients)
  "Compute method combination DEFINITION's users."
  ;; #### NOTE: a case could be made to avoid rebuilding the whole list here,
  ;; and only add what's missing, but I don't think it's worth the trouble.
  (maphash (lambda (function unused)
	     (declare (ignore unused))
	     (let ((client (find-definition function definitions)))
	       (cond (client
		      (endpush client clients))
		     ((not (foreignp definition))
		      (setq client (make-generic-definition function t))
		      (setq *stabilized* nil)
		      (endpush client definitions)
		      (endpush client clients)))))
	   (sb-pcl::method-combination-%generic-functions
	    (combination definition)))
  (setf (clients definition) clients))

(defmethod stabilize progn
  ((definition short-combination-definition) definitions
   &aux (name (sb-pcl::short-combination-operator (combination definition))))
  "Compute short combination DEFINITION's standalone combinator definition."
  (unless (standalone-combinator definition)
    (setf (standalone-combinator definition)
	  (find* name definitions
	    :pre-test (lambda (candidate)
			(or (typep candidate 'macro-definition)
			    (typep candidate 'function-definition)))
	    :key #'name))) ;; EQ test will filter out setf functions
  (unless (or (standalone-combinator definition) (foreignp definition))
    (let* ((combinator-package
	     (find-definition (symbol-package name) definitions))
	   (combinator
	     (unless (and combinator-package
			  (not (foreignp combinator-package)))
	       (foreign-funcoid-definition name))))
      (cond (combinator
	     (setq *stabilized* nil)
	     (endpush combinator definitions)
	     (setf (standalone-combinator definition) combinator))
	    (t
	     (warn "~S: undefined operator for short method combination ~S."
		   name (name definition)))))))



;; Methods
;; -------

(defmethod stabilize progn
    ((definition method-definition) definitions)
  "Computer method DEFINITION's specializer references."
  (setf (specializers definition)
	(mapcar (lambda (specializer)
		  ;; #### FIXME: according to my former version of
		  ;; PRETTY-SPECIALIZER, the situation might be more
		  ;; complicated than just EQL or class specializers. Let's
		  ;; just see until this breaks on a concrete case.
		  (typecase specializer
		    (eql-specializer specializer)
		    (otherwise
		     (let ((class-specializer
			     (find-definition specializer definitions)))
		       (unless class-specializer
			 (setq class-specializer
			       (make-classoid-definition
				(class-name specializer)
				specializer
				t))
			 (setq *stabilized* nil)
			 (endpush class-specializer definitions))
		       class-specializer))))
	  (method-specializers (definition-method definition)))))



;; Classoids
;; ---------

;; #### NOTE: regardless of the classoid (structures included), there is no
;; relation between the foreign status of slots and their readers / writers.
;; It's pretty obvious for regular classes and generic accessors. Maybe less
;; so for structures, where the key factor is that with the :conc-name option,
;; accessor names are interned in the current package, /not/ the structure
;; name's package. In the end, we could get our own classoids with foreign
;; accessors, but also foreign classoids with our own accessors. In the
;; specific case of structures however,there is still one additional
;; constraint, which is that both readers and writers share the same status,
;; as their names always go hand in hand.

(defmethod stabilize progn
    ((definition clos-classoid-mixin) definitions &aux classoid-definitions)
  "Compute classoid DEFINITION's super/sub classoids, and method definitions."
  ;; #### NOTE: a case could be made to avoid rebuilding the whole lists here,
  ;; and only add what's missing, but I don't think it's worth the trouble.
  (flet ((get-classoid-definition (classoid)
	   (let ((classoid-definition (find-definition classoid definitions)))
	     (cond (classoid-definition
		    (endpush classoid-definition classoid-definitions))
		   ((not (foreignp definition))
		    (setq classoid-definition
			  (make-classoid-definition
			   (class-name classoid) classoid t))
		    (setq *stabilized* nil)
		    (endpush classoid-definition definitions)
		    (endpush classoid-definition classoid-definitions))))))
    (mapc #'get-classoid-definition
      (class-direct-superclasses (classoid definition)))
    (setf (direct-superclassoids definition) classoid-definitions)
    (setq classoid-definitions nil) ;; yuck.
    (mapc #'get-classoid-definition
      (class-direct-subclasses (classoid definition)))
    (setf (direct-subclassoids definition) classoid-definitions))
  (setf (direct-methods definition)
	(mapcan
	    (lambda (method)
	      (let* ((generic (method-generic-function method))
		     (generic-definition (find-definition generic definitions))
		     (method-definition
		       (when generic-definition
			 (find method (methods generic-definition)
			   :key #'definition-method))))
		(if method-definition
		  (list method-definition)
		  ;; Starting here, that is, if we're missing the method
		  ;; definition, or the whole generic definition, it means
		  ;; that we're dealing with a foreign definition.
		  (unless (foreignp definition)
		    (cond (generic-definition
			   (setq method-definition
				 (make-method-definition
				  method generic-definition t))
			   (setq *stabilized* nil)
			   (endpush method-definition definitions)
			   (endpush method-definition
				    (methods generic-definition))
			   (list method-definition))
			  (t
			   (setq generic-definition
				 (make-generic-definition generic t))
			   (setq method-definition
				 (make-method-definition
				  method generic-definition t))
			   (setq *stabilized* nil)
			   (endpush method-definition definitions)
			   (endpush method-definition
				    (methods generic-definition))
			   (endpush generic-definition definitions)
			   (list method-definition)))))))
	  (specializer-direct-methods (classoid definition)))))



;; Slots
;; -----

;; #### FIXME: this whole section needs factoring.

;; #### NOTE: we can't completely unify the stabilization of slot readers and
;; writers because structure classes behave differently from condition and
;; regular ones (or, we would need an additional super-class for only these
;; two). That's why we have 3 methods below, two of them actually using the
;; same helper function.

(defun stabilize-clos-classoid-slot
    (definition definitions
     &aux (slot (slot definition))
	  (owner (owner definition))
	  (classoid (classoid owner)))
  "Compute CLOS classoid slot DEFINITION's reader and writer definitions.
This function is used for regular class and condition slots."
  ;; #### NOTE: a case could be made to avoid rebuilding the whole list here,
  ;; and only add what's missing, but I don't think it's worth the trouble.
  (setf (readers definition)
	(mapcan
	    (lambda (name)
	      (let* ((generic (fdefinition name))
		     (reader (find-definition generic definitions)))
		(unless (or reader (foreignp owner))
		  (setq reader (make-generic-definition generic t))
		  (setq *stabilized* nil)
		  (endpush reader definitions))
		(when reader
		  (let* ((method
			   (find classoid (generic-function-methods generic)
			     :key (lambda (method)
				    (first (method-specializers method)))))
			 (reader-method
			   (find-definition method (methods reader))))
		    (unless (or reader-method (foreignp owner))
		      (setq reader-method
			    (make-method-definition method reader t))
		      (setq *stabilized* nil)
		      (endpush reader-method definitions)
		      (endpush reader-method (methods reader)))
		    (when reader-method
		      (change-class reader-method 'reader-method-definition
			:target-slot definition)
		      (list reader-method))))))
	  (slot-definition-readers slot)))
  (setf (writers definition)
	(mapcan
	    (lambda (name)
	      (let* ((generic (fdefinition name))
		     (writer (find-definition generic definitions)))
		(unless (or writer (foreignp owner))
		  (setq writer (make-generic-definition generic t))
		  (setq *stabilized* nil)
		  (endpush writer definitions))
		(when writer
		  (let* ((method
			   (find classoid (generic-function-methods generic)
			     :key (lambda (method)
				    ;; #### NOTE: whatever the kind of writer,
				    ;; that is, whether it is defined with
				    ;; :writer or :accessor, the argument list
				    ;; is always (NEW-VALUE OBJECT).
				    (second (method-specializers method)))))
			 (writer-method
			   (find-definition method (methods writer))))
		    (unless (or writer-method (foreignp owner))
		      (setq writer-method
			    (make-method-definition method writer t))
		      (setq *stabilized* nil)
		      (endpush writer-method definitions)
		      (endpush writer-method (methods writer)))
		    (when writer-method
		      (change-class writer-method
			  (if (typep writer-method 'setf-mixin)
			    'setf-writer-method-definition
			    'simple-writer-method-definition)
			:target-slot definition)
		      (list writer-method))))))
	  (slot-definition-writers slot))))

;; #### PORTME: SBCL defines writers as setf functions, but the standard
;; explicitly allows the use of setf expanders instead. Also, beware of this
;; trap! When a slot is read-only, SBCL still has an internal writer function
;; (for initialization, I suppose), but it's not a setf function; it's an
;; internal closure. As a consequence, we /will/ find a writer-function below,
;; but not a definition for it.
(defun stabilize-clos-structure-slot
    (definition definitions
     &aux (slot (slot definition))
	  (owner (owner definition)))
  "Compute CLOS structure slot DEFINITION's reader and writer definitions."
  ;; #### NOTE: in the case of structures, there is only one reader / writer
  ;; per slot, so if it's already there, we can save some time because the
  ;; list of it as a single element doesn't risk being out of date. Also,
  ;; remember that readers and writers share the same status, so we only need
  ;; to perform one test each time.
  (unless (readers definition)
    (let* ((accessor-name
	     (sb-pcl::slot-definition-defstruct-accessor-symbol slot))
	   (reader-function
	     (sb-pcl::slot-definition-internal-reader-function slot))
	   (writer-function
	     (sb-pcl::slot-definition-internal-writer-function slot))
	   (reader (find-definition reader-function definitions))
	   (writer (find-definition writer-function definitions)))
      ;; See PORTME comment above the function about this.
      (when (and reader (not writer)) (setq writer-function nil))
      (unless (or reader (foreignp owner))
	(setq reader (make-function-definition accessor-name reader-function
		       :foreign t))
	(when writer-function
	  (setq writer (make-function-definition accessor-name writer-function
			 :setf t :foreign t)))
	(setq *stabilized* nil)
	(endpush reader definitions)
	(when writer (endpush writer definitions)))
      (when reader
	(unless (typep reader 'reader-definition)
	  (change-class reader 'reader-definition :target-slot definition))
	(when writer
	  (unless (typep writer 'writer-definition)
	    (change-class writer 'writer-definition :target-slot definition))))
      ;; See comment on top of the SLOT-DEFINITION class about this.
      (setf (readers definition) (when reader (list reader)))
      (setf (writers definition) (when writer (list writer))))))

(defmethod stabilize progn ((definition clos-slot-definition) definitions)
  "Compute CLOS slot DEFINITION's reader and writer definitions."
  (typecase (owner definition)
    (clos-structure-definition
     (stabilize-clos-structure-slot definition definitions))
    (otherwise
     (stabilize-clos-classoid-slot definition definitions))))


;; #### PORTME: SBCL defines writers as setf functions, but the standard
;; explicitly allows the use of setf expanders instead.
(defmethod stabilize progn
    ((definition typed-structure-slot-definition) definitions
     &aux (slot (slot definition))
	  (owner (owner definition)))
  "Compute typed structure slot DEFINITION's reader and writer definitions."
  ;; #### NOTE: in the case of structures, there is only one reader / writer
  ;; per slot, so if it's already there, we can save some time because the
  ;; list of it as a single element doesn't risk being out of date. Also,
  ;; remember that readers and writers share the same status, so we only need
  ;; to perform one test each time (on the reader, though, because a slot may
  ;; be read-only).
  (unless (readers definition)
    (let* ((reader-name (sb-kernel:dsd-accessor-name slot))
	   (writer-name `(setf ,reader-name))
	   (reader-function (fdefinition reader-name))
	   (writer-function (when (fboundp writer-name)
			      (fdefinition writer-name)))
	   (reader (find-definition reader-function definitions))
	   (writer (when writer-function
		     (find-definition writer-function definitions))))
      (unless (or reader (foreignp owner))
	(setq reader (make-function-definition reader-name reader-function
		       :foreign t))
	(when writer-function
	  (setq writer (make-function-definition reader-name writer-function
			 :setf t :foreign t)))
	(setq *stabilized* nil)
	(endpush reader definitions)
	(when writer (endpush writer definitions)))
      (when reader
	(unless (typep reader 'reader-definition)
	  (change-class reader 'reader-definition :target-slot definition))
	(when writer
	  (unless (typep writer 'writer-definition)
	    (change-class writer 'writer-definition :target-slot definition))))
      ;; See comment on top of the SLOT-DEFINITION class about this.
      (setf (readers definition) (when reader (list reader)))
      (setf (writers definition) (when writer (list writer))))))



;; --------
;; Packages
;; --------

(defmethod stabilize progn
    ((definition package-definition) definitions
     &aux (package (definition-package definition))
	  package-definitions)
  "Compute package DEFINITION's use, used-by, and definitions lists.
New foreign package definitions may be created and added at the end of
DEFINITIONS in the process."
  ;; #### NOTE: a case could be made to avoid rebuilding the whole lists here,
  ;; and only add what's missing, but I don't think it's worth the trouble.
  (flet ((get-package-definition (package)
	   (let ((package-definition (find-definition package definitions)))
	     (cond (package-definition
		    (endpush package-definition package-definitions))
		   ((not (foreignp definition))
		    (setq package-definition
			  (make-package-definition package t))
		    (setq *stabilized* nil)
		    (endpush package-definition definitions)
		    (endpush package-definition package-definitions))))))
    ;; 1. Use list.
    (mapc #'get-package-definition (package-use-list package))
    (setf (use-list definition) package-definitions)
    ;; 2. Used-by list.
    (setq package-definitions nil) ;; yuck.
    (mapc #'get-package-definition (package-used-by-list package))
    (setf (used-by-list definition) package-definitions))
  ;; 3. Symbol definitions list.
  (setf (definitions definition)
	(retain package definitions
	  :pre-test #'symbol-definition-p
	  :key (lambda (definition)
		 (symbol-package (definition-symbol definition))))))



;; ---------------
;; ASDF Components
;; ---------------

(defun make-component-definition (component &optional foreign)
  "Make a new COMPONENT definition of the appropriate class, possibly FOREIGN."
  (etypecase component
    (asdf:system (make-system-definition component foreign))
    (asdf:module (make-module-definition component foreign))
    (asdf:file-component (make-file-definition component foreign))))

(defun resolve-dependency-specification
    (specification component definitions foreign &aux inner)
  "Resolve dependency SPECIFICATION for (FOREIGN) COMPONENT in DEFINITIONS.
SPECIFICATION must already be reordered (see `reorder-dependency-def' for more
information). The specification's component name is replaced with its
corresponding definition. A foreign definition may be created in the process.

If such a definition is neither found, nor created, return NIL. Otherwise,
return a list of the updated specification (suitable to MAPCAN)."
  (unless (listp specification) (setq specification (list specification)))
  (setq inner specification)
  (while (listp (car inner)) (setq inner (car inner)))
  (let* ((name (car inner))
	 (dependency (resolve-dependency-name component name))
	 (definition (find-definition dependency definitions)))
    (unless (or definition foreign)
      (setq definition (make-component-definition dependency t))
      (setq *stabilized* nil)
      (endpush definition definitions))
    (when definition
      (rplaca inner definition)
      (list specification))))

(defmethod stabilize progn
    ((definition component-definition) definitions
     &aux (foreign (foreignp definition))
	  (component (component definition)))
  "Compute component DEFINITION's parent and dependency definitions.
Those definitions are guaranteed to be in the original component's order."
  ;; #### WARNING: systems are components, but don't have a parent so PARENT
  ;; is NIL for them here. We don't want to search definitions for a NIL
  ;; object because we'd fall on constants, special variables, symbol macros,
  ;; or types. So we need this workaround (or check that the definition's
  ;; actual type is not SYSTEM-DEFINITION). The parent-definition slot for
  ;; system definitions will actually be set to NIL through the overloaded
  ;; :initform provided in the corresponding class. There are no foreign
  ;; components, apart from systems.
  (when-let (parent (component-parent component))
    (setf (parent definition) (find-definition parent definitions)))
  ;; #### NOTE: a case could be made to avoid rebuilding the whole list here,
  ;; and only add what's missing, but I don't think it's worth the trouble.
  (setf (dependencies definition)
	(mapcan (lambda (dependency)
		  (resolve-dependency-specification
		   dependency component definitions foreign))
	  (mapcar #'reorder-dependency-def
	    (component-sideway-dependencies component)))))



;; Files
;; -----

(defmethod stabilize progn
    ((definition lisp-file-definition) definitions
     &aux (pathname (component-pathname (file definition))))
  "Compute Lisp file DEFINITION's definitions list."
  (setf (definitions definition)
	(retain pathname definitions :test #'equal :key #'source-pathname)))



;; Modules
;; -------

(defmethod stabilize progn ((definition module-definition) definitions)
  "Compute module DEFINITION's child definitions.
Those definitions are guaranteed to be in the module's original order."
  (setf (children definition)
	;; #### NOTE: remember that we may not have all children, in the case
	;; of foreign (system) definitions. Also, note that this
	;; implementation not only preserves the original order, but also
	;; filters out our hacked system files.
	(mapcan (lambda (component)
		  (when-let (child (find-definition component definitions))
		    (list child)))
	  (component-children (module definition)))))



;; Systems
;; -------

(defmethod stabilize progn
    ((definition system-definition) definitions
     &aux (foreign (foreignp definition))
	  (system (system definition)))
  "Compute system DEFINITION's defsystem dependency definitions.
Those definitions are guaranteed to be in the original system's order."
  ;; #### NOTE: a case could be made to avoid rebuilding the whole list here,
  ;; and only add what's missing, but I don't think it's worth the trouble.
  (setf (defsystem-dependencies definition)
	(mapcan (lambda (dependency)
		  (resolve-dependency-specification
		   dependency system definitions foreign))
	  (mapcar #'reorder-dependency-def
	    (system-defsystem-depends-on system)))))




;; ==========================================================================
;; Freezing
;; ==========================================================================

(defun freeze (definitions)
  "Freeze DEFINITIONS.
Currently, this means potentially upgrading generic definitions to reader or
 writer definitions."
  ;; #### NOTE: we can't upgrade regular generic functions to reader or writer
  ;; ones until we're sure to have all the methods around. That's why the
  ;; freezing step was introduced.
  (mapc (lambda (definition)
	  (when (typep definition 'generic-function-definition)
	    (cond ((every #'reader-method-definition-p (methods definition))
		   (change-class definition 'generic-reader-definition))
		  ((every #'writer-method-definition-p (methods definition))
		   (change-class definition
		       (etypecase definition
			 (simple-generic-definition
			  'simple-generic-writer-definition)
			 (generic-setf-definition
			  'generic-setf-writer-definition)))))))
    definitions))




;; ==========================================================================
;; Finalization Entry Point
;; ==========================================================================

(defun finalize (definitions)
  "Finalize DEFINITIONS.
For more information, see `stabilize' and `freeze'."
  ;; #### NOTE: the Common Lisp standard doesn't specify what happens when an
  ;; object being traversed is modified (see Section 3.6 of the CLHS). So I
  ;; can't reliably use DOLIST here, even though I'm only pushing new
  ;; definitions at the end of the list. I believe however that the code below
  ;; is reliable. Also, note that the finalization process traverses ALL
  ;; definitions, including the foreign ones added during the process. This
  ;; means that we end up with a potentially large number of definitions that
  ;; will probably not be documented. But again, you never know what people
  ;; will want to do with that.
  (setq *stabilized* nil)
  (while (not *stabilized*)
    (setq *stabilized* t)
    (do ((remaining definitions (cdr remaining))) ((endp remaining))
      (stabilize (first remaining) definitions)))
  (freeze definitions))

;; finalize.lisp ends here
