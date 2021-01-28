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

;; #### WARNING: as a general rule of thumb, never assume anything about the
;; finalization state of the other definitions you access when finalizing a
;; definition. There are many small methods here and there that are executed
;; and it's too error-prone to even try to rely on their execution order
;; (scattered behavior, that's the drawback of OO).
;; For example, this is guaranteed to work all the time:
;; (symbol-package (definition-symbol definition)
;; whereas this might fail if the definition has not been finalized yet:
;; (definition-package (package-definition definition)).


;;; Code:

(in-package :net.didierverna.declt)
(in-readtable :net.didierverna.declt)


(defvar *finalized* nil
  "Whether the finalization process is over.
This variable is dynamically set to NIL whenever new definitions are
created during the finalization process. The finalization process is run over
and over again until nothing moves anymore.")

(defgeneric finalize (definition definitions)
  (:documentation "Finalize DEFINITION in DEFINITIONS.")
  (:method-combination progn))



;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun make-generic-definition (generic &optional foreign)
  "Make a new GENERIC function definition, possibly FOREIGN."
  (let* ((name (sb-mop:generic-function-name generic))
	 (setf (consp name))
	 (symbol (if setf (second name) name)))
    (make-instance (if setf 'generic-setf-definition 'generic-definition)
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




;; ==========================================================================
;; All Definitions
;; ==========================================================================

(defmethod finalize progn ((definition definition) definitions)
  "Compute DEFINITION's source file definition."
  (unless (source-file definition)
    (when-let (source-pathname (source-pathname definition))
      (setf (source-file definition)
	    ;; #### FIXME: FIND[-IF] with test and key reversed.
	    (find-if (lambda (definition)
		       (and (typep definition 'lisp-file-definition)
			    (equal (component-pathname (file definition))
				   source-pathname)))
		     definitions)))))




;; ==========================================================================
;; Symbol Definitions
;; ==========================================================================

(defmethod finalize progn
    ((definition symbol-definition) definitions
     &aux (package (symbol-package (definition-symbol definition))))
  "Compute symbol DEFINITION's package definition.
New foreign package definitions may be created and added at the end of
DEFINITIONS in the process."
  ;; #### NOTE: every symbol definition gets a working package definition;
  ;; even the foreign ones.
  (unless (package-definition definition)
    (setf (package-definition definition)
	  (or (find-definition package definitions)
	      (let ((package-definition (make-package-definition package t)))
		(endpush package-definition definitions)
		(setq *finalized* nil)
		package-definition)))))



;; --------------------
;; Setf expander mixins
;; --------------------

(defmethod finalize progn
    ((definition expander-mixin) definitions
     &aux (name (name definition)) ;; always a symbol here
	  (lambda-list (lambda-list definition)))
  "Compute DEFINITION's expander-for and expanders-to references."
  ;; #### NOTE: this definition and a potential expander-for share the same
  ;; symbol. Consequently, if this symbol is one of our own, we /will/ find an
  ;; expander-for in the already existing definitions, if it exists. If, on
  ;; the other hand, DEFINITION is foreign, then so will the expander-for, and
  ;; thus we don't care if it's not found. To put it differently, we never
  ;; need to create a foreign expander definition here.
  (unless (expander-for definition)
    (setf (expander-for definition)
	  (find-if (lambda (candidate)
		     (and (typep candidate 'expander-definition)
			  ;; don't want the setf part
			  (eq (definition-symbol candidate) name)
			  (equal (lambda-list candidate) lambda-list)))
		   definitions)))
  ;; #### FIXME: in the code below, we're looking for expanders-to only in
  ;; the current list of definitions. Although it may contain foreign
  ;; definitions, it could also be incomplete. In order to be exhaustive for
  ;; our own definitions if not for foreign ones, we would need to go through
  ;; all existing symbols.
  ;; #### NOTE: a case could be made to avoid rebuilding the whole list here,
  ;; and only add what's missing, but I don't think it's worth the trouble.
  (setf (expanders-to definition)
	(remove-if-not (lambda (candidate)
			 (and (typep candidate 'short-expander-definition)
			      (eq (update-fn-name candidate) name)))
	    definitions)))


;; #### WARNING: there is no finalization method for the accessor mixin. This
;; is handled by the sot finalization methods. Slot readers and writers are
;; searched for as regular functions, and when they are found, their
;; respective classes are upgraded.



;; --------------
;; Setf expanders
;; --------------

;; #### WARNING: in fact, take the comments above and below with a grain of
;; salt. Especially because of the way we handle expanders-to above, we
;; actually don't currently create foreign expanders, ever...

(defmethod finalize progn
    ((definition expander-definition) definitions
     &aux (name (definition-symbol definition)) ;; don't want the setf part
	  (lambda-list (lambda-list definition)))
  "Compute setf expander DEFINTIION's access definition."
  ;; #### NOTE: same remark as above when finalizing and expander-for
  ;; definition: if this expander is our own, then we /will/ find a definition
  ;; for its access-definition if it exists, as it is for the same symbol.
  ;; Otherwise, we don't care if we find a definition or not. So we never need
  ;; to create a foreign access definition.
  (unless (access-definition definition)
    (setf (access-definition definition)
	  (find-if (lambda (candidate)
		     (and (or (typep candidate 'macro-definition)
			      (typep candidate '%function-definition))
			  ;; this will filter out setf functions
			  (eq (name candidate) name)
			  (equal (lambda-list candidate) lambda-list)))
		   definitions))))

;; #### FIXME: a setf expander may be defined without its update-fn actually
;; existing. This just means that the expander cannot be used (yet). The code
;; below assumes that this never happens and so also assumes that if the
;; update-fn is not found, then it's gotta be a foreign definition. This is
;; incorrect and will break if the function is indeed not defined. What we
;; need to do is check if the update-fn /should/ belong to us, in which case
;; it should perhaps be left NULL and issue a buggy program warning.
(defmethod finalize progn
    ((definition short-expander-definition) definitions
     &aux (name (update-fn-name definition)))
  "Computer short setf expander DEFINITION's update definition."
  (unless (update-definition definition)
    (setf (update-definition definition)
	  (find-if (lambda (candidate)
		     (and (or (typep candidate 'macro-definition)
			      (typep candidate '%function-definition))
			  ;; this will filter out setf functions
			  (eq (name candidate) name)))
		   definitions)))
  (unless (or (update-definition definition) (foreignp definition))
    (when-let (update-definition (foreign-funcoid-definition name))
      (endpush update-definition definitions)
      (setq *finalized* nil)
      (setf (update-definition definition) update-definition))))



;; -------------------
;; Method combinations
;; -------------------

;; #### NOTE: after Christophe's changes to SBCL following my ELS paper, I
;; think we can reliably access the method combination object's hashtable of
;; generic functions to compute its users. This is better than than the old
;; way, which was to scan all known generic functions and look at their method
;; combination.

;; #### PORTME.
(defmethod finalize progn
    ((definition combination-definition) definitions &aux users)
  "Compute method combination DEFINITION's users."
  ;; #### NOTE: a case could be made to avoid rebuilding the whole list here,
  ;; and only add what's missing, but I don't think it's worth the trouble.
  (maphash (lambda (function unused)
	     (declare (ignore unused))
	     (let ((user (find-definition function definitions)))
	       (cond (user
		      (push user users))
		     ((not (foreignp definition))
		      (setq user (make-generic-definition function t))
		      (setq *finalized* nil)
		      (endpush user definitions)
		      (push user users)))))
	   (sb-pcl::method-combination-%generic-functions
	    (combination definition)))
  (setf (user-definitions definition) users))

;; #### FIXME: the situation here is very similar to that of short form setf
;; expanders: a short form method combination may be defined without its
;; operator actually existing. This just means that the combination cannot be
;; used (yet). The code below assumes that this never happens and so also
;; assumes that if the operator is not found, then it's gotta be a foreign
;; definition. This is incorrect and will break if the function is indeed not
;; defined. What we need to do is check if the operator /should/ belong to us,
;; in which case it should perhaps be left NULL and issue a buggy program
;; warning.
(defmethod finalize progn
  ((definition short-combination-definition) definitions
   &aux (name (sb-pcl::short-combination-operator (combination definition))))
  "Compute short method combination DEFINITION's operator definition."
  (unless (operator-definition definition)
    (setf (operator-definition definition)
	  (find-if (lambda (candidate)
		     (and (or (typep candidate 'macro-definition)
			      (typep candidate '%function-definition))
			  ;; this will filter out setf functions
			  (eq (name candidate) name)))
		   definitions)))
  (unless (or (operator-definition definition) (foreignp definition))
    (when-let (operator-definition (foreign-funcoid-definition name))
      (setq *finalized* nil)
      (endpush operator-definition definitions)
      (setf (operator-definition definition) operator-definition))))



;; -----------------
;; Generic functions
;; -----------------

;; #### NOTE: contrary to the case of short form setf expanders and short form
;; method combinations, it seems that the method combination object must exist
;; when a generic function is created (I'm getting errors otherwise). This
;; means that if we cannot find the combination definition right now, it must
;; be a foreign one.

;; #### PORTME.
(defmethod finalize progn
    ((definition generic-function-definition) definitions
     &aux (combination
	   (sb-mop:generic-function-method-combination (generic definition))))
  "Compute generic function DEFINITION's method combination definition.
Also finalize all methods."
  (unless (combination-definition definition)
    (setf (combination-definition definition)
	  (find-definition combination definitions)))
  (unless (or (combination-definition definition) (foreignp definition))
    (let ((combination-definition
	    (make-combination-definition
	     (sb-pcl::method-combination-type-name combination)
	     combination t)))
      (setq *finalized* nil)
      (endpush combination-definition definitions)
      (setf (combination-definition definition) combination-definition)))
  (mapc (lambda (method-definition) (finalize method-definition definitions))
    (method-definitions definition)))



;; ---------
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


;; Slots

;; #### NOTE: we can't completely unify the finalization of slot readers and
;; writers because structure classes behave differently from condition and
;; regular ones (or, we would need an additional super-class for only these
;; two). That's why we have 3 methods below, two of them actually using the
;; same helper function.

;; #### PORTME.
(defun finalize-classoid-slot
    (definition definitions
     &aux (slot (slot definition))
	  (classoid-definition (classoid-definition definition))
	  (classoid (classoid classoid-definition)))
  "Compute classoid slot DEFINITION's reader and writer definitions.
This function is used for regular class and condition slots."
  ;; #### NOTE: a case could be made to avoid rebuilding the whole list here,
  ;; and only add what's missing, but I don't think it's worth the trouble.
  (setf (reader-definitions definition)
	(mapcan
	    (lambda (name)
	      (let* ((generic (fdefinition name))
		     (reader-definition (find-definition generic definitions)))
		(unless (or reader-definition (foreignp classoid-definition))
		  (setq reader-definition (make-generic-definition generic t))
		  (setq *finalized* nil)
		  (endpush reader-definition definitions))
		(when reader-definition
		  (let* ((method
			   (find classoid
			       (sb-mop:generic-function-methods generic)
			     :key (lambda (method)
				    (first
				     (sb-mop:method-specializers method)))))
			 (method-definition
			   (find-definition
			    method
			    (method-definitions reader-definition))))
		    (unless (or method-definition
				(foreignp classoid-definition))
		      (setq method-definition
			    (make-method-definition
			     method reader-definition t))
		      (setq *finalized* nil)
		      (push method-definition
			    (method-definitions reader-definition)))
		    (when method-definition
		      (change-class method-definition 'reader-method-definition
			:slot-definition definition)
		      (list method-definition))))))
	  (sb-mop:slot-definition-readers slot)))
  (setf (writer-definitions definition)
	(mapcan
	    (lambda (name)
	      (let* ((generic (fdefinition name))
		     (writer-definition (find-definition generic definitions)))
		(unless (or writer-definition (foreignp classoid-definition))
		  (setq writer-definition (make-generic-definition generic t))
		  (setq *finalized* nil)
		  (endpush writer-definition definitions))
		(when writer-definition
		  (let* ((method
			   (find classoid
			       (sb-mop:generic-function-methods generic)
			     :key (lambda (method)
				    ;; #### NOTE: whatever the kind of writer,
				    ;; that is, whether it is defined with
				    ;; :writer or :accessor, the argument list
				    ;; is always (NEW-VALUE OBJECT).
				    (second
				     (sb-mop:method-specializers method)))))
			 (method-definition
			   (find-definition
			    method
			    (method-definitions writer-definition))))
		    (unless (or method-definition
				(foreignp classoid-definition))
		      (setq method-definition
			    (make-method-definition
			     method writer-definition t))
		      (setq *finalized* nil)
		      (push method-definition
			    (method-definitions writer-definition)))
		    (when method-definition
		      (change-class method-definition
			  (if (typep method-definition 'setf-mixin)
			    'setf-writer-method-definition
			    'writer-method-definition)
			:slot-definition definition)
		      (list method-definition))))))
	  (sb-mop:slot-definition-writers slot))))

;; #### PORTME.
(defun finalize-structure-slot
    (definition definitions
     &aux (slot (slot definition))
	  (structure-definition (classoid-definition definition)))
  "Compute structure slot DEFINITION's reader and writer definitions."
  ;; #### NOTE: in the case of structures, there is only one reader / writer
  ;; per slot, so if it's already there, we can save some time because the
  ;; list of it as a single element doesn't risk being out of date. Also,
  ;; remember that readers and writers share the same status, so we only need
  ;; to perform one test each time.
  (unless (reader-definitions definition)
    (let* ((accessor-name
	     (sb-pcl::slot-definition-defstruct-accessor-symbol slot))
	   (reader-function
	     (sb-pcl::slot-definition-internal-reader-function slot))
	   (writer-function
	     (sb-pcl::slot-definition-internal-writer-function slot))
	   (reader-definition (find-definition reader-function definitions))
	   (writer-definition (find-definition writer-function definitions)))
      (unless (or reader-definition (foreignp structure-definition))
	(setq reader-definition
	      (make-function-definition accessor-name reader-function
		:foreign t))
	(setq writer-definition
	      (make-function-definition accessor-name writer-function
		:setf t :foreign t))
	(setq *finalized* nil)
	(endpush reader-definition definitions)
	(endpush writer-definition definitions))
      (when reader-definition
	(unless (typep reader-definition 'reader-definition)
	  (change-class reader-definition 'reader-definition
	    :slot-definition definition))
	(unless (typep writer-definition 'writer-definition)
	  (change-class writer-definition 'writer-definition
	    :slot-definition definition)))
      ;; See comment on top of the SLOT-DEFINITION class about this.
      (setf (reader-definitions definition)
	    (when reader-definition (list reader-definition)))
      (setf (writer-definitions definition)
	    (when writer-definition (list writer-definition))))))

(defmethod finalize progn ((definition slot-definition) definitions)
  "Compute slot DEFINITION's reader and writer definitions."
  (if (typep (classoid-definition definition) 'structure-definition)
    (finalize-structure-slot definition definitions)
    (finalize-classoid-slot definition definitions)))



;; Classes

;; #### PORTME.
(defmethod finalize progn
    ((definition classoid-definition) definitions &aux classoid-definitions)
  "Compute classoid DEFINITION's super/sub classoids, and method definitions.
Also finalize all slots."
  ;; #### NOTE: a case could be made to avoid rebuilding the whole lists here,
  ;; and only add what's missing, but I don't think it's worth the trouble.
  (flet ((get-classoid-definition (classoid)
	   (let ((classoid-definition (find-definition classoid definitions)))
	     (cond (classoid-definition
		    (push classoid-definition classoid-definitions))
		   ((not (foreignp definition))
		    (setq classoid-definition
			  (make-classoid-definition
			   (class-name classoid) classoid t))
		    (setq *finalized* nil)
		    (endpush classoid-definition definitions)
		    (push classoid-definition classoid-definitions))))))
    (mapc #'get-classoid-definition
      (sb-mop:class-direct-superclasses (classoid definition)))
    (setf (superclassoid-definitions definition) classoid-definitions)
    (setq classoid-definitions nil)
    (mapc #'get-classoid-definition
      (sb-mop:class-direct-subclasses (classoid definition)))
    (setf (subclassoid-definitions definition) classoid-definitions))
  (setf (method-definitions definition)
	(mapcan
	    (lambda (method)
	      (let* ((generic (sb-mop:method-generic-function method))
		     (generic-definition (find-definition generic definitions))
		     (method-definition
		       (when generic-definition
			 (find method (method-definitions generic-definition)
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
			   (setq *finalized* nil)
			   (push method-definition
				 (method-definitions generic-definition))
			   (list method-definition))
			  (t
			   (setq generic-definition
				 (make-generic-definition generic t))
			   (setq method-definition
				 (make-method-definition
				  method generic-definition t))
			   (setq *finalized* nil)
			   (push method-definition
				 (method-definitions generic-definition))
			   (endpush generic-definition definitions)
			   (list method-definition)))))))
	  (sb-mop:specializer-direct-methods (classoid definition))))
  (mapc (lambda (slot-definition) (finalize slot-definition definitions))
    (slot-definitions definition)))



;; --------
;; Packages
;; --------

(defmethod finalize progn
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
		    (push package-definition package-definitions))
		   ((not (foreignp definition))
		    (setq package-definition
			  (make-package-definition package t))
		    (setq *finalized* nil)
		    (endpush package-definition definitions)
		    (push package-definition package-definitions))))))
    ;; 1. Use list.
    (mapc #'get-package-definition (package-use-list package))
    (setf (use-definitions definition) package-definitions)
    ;; 2. Used-by list.
    (setq package-definitions nil)
    (mapc #'get-package-definition (package-used-by-list package))
    (setf (used-by-definitions definition) package-definitions))
  ;; 3. Symbol definitions list.
  (setf (definitions definition)
	(mapcan (lambda (definition)
		  (cond ((typep definition 'classoid-definition)
			 ;; #### FIXME: RETAIN.
			 (remove-if-not
			     (lambda (definition)
			       (eq (symbol-package
				    (definition-symbol definition))
				   package))
			     (cons definition (slot-definitions definition))))
			((and (typep definition 'symbol-definition)
			      (eq (symbol-package
				   (definition-symbol definition))
				  package))
			 (list definition))))
	  definitions)))




;; ==========================================================================
;; Components
;; ==========================================================================

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
      (setq *finalized* nil)
      (endpush definition definitions))
    (when definition
      (rplaca inner definition)
      (list specification))))

(defmethod finalize progn
    ((definition component-definition) definitions
     &aux (foreign (foreignp definition))
	  (component (component definition)))
  "Compute component DEFINITION's parent and dependency definitions."
  ;; #### WARNING: systems are components, but don't have a parent so PARENT
  ;; is NIL for them here. We don't want to search definitions for a NIL
  ;; object because we'd fall on constants, special variables, symbol macros,
  ;; or types. So we need this workaround (or check that the definition's
  ;; actual type is not SYSTEM-DEFINITION). The parent-definition slot for
  ;; system definitions will actually be set to NIL through the overloaded
  ;; :initform provided in the corresponding class. There are no foreign
  ;; components, apart from systems.
  (when-let (parent (component-parent component))
    (setf (parent-definition definition) (find-definition parent definitions)))
  ;; #### NOTE: a case could be made to avoid rebuilding the whole list here,
  ;; and only add what's missing, but I don't think it's worth the trouble.
  (setf (dependencies definition)
	(mapcan (lambda (dependency)
		  (resolve-dependency-specification
		   dependency component definitions foreign))
	  (mapcar #'reorder-dependency-def
	    (component-sideway-dependencies component)))))



;; -----
;; Files
;; -----

(defmethod finalize progn
    ((definition lisp-file-definition) definitions
     &aux (pathname (component-pathname (file definition))))
  "Compute Lisp file DEFINITION's definitions list."
  (setf (definitions definition)
	(mapcan (lambda (definition)
		  (cond ((typep definition 'generic-function-definition)
			 ;; #### FIXME: RETAIN.
			 (remove-if-not
			     (lambda (definition)
			       (equal (source-pathname definition) pathname))
			     (cons definition
				   (method-definitions definition))))
			((equal (source-pathname definition) pathname)
			 (list definition))))
	  definitions)))



;; -------
;; Modules
;; -------

(defmethod finalize progn
    ((definition module-definition) definitions
     &aux (module (module definition)))
  "Compute module DEFINITION's child definitions."
  (setf (child-definitions definition)
	;; #### FIXME: write a RETAIN or KEEP function, also inverting the
	;; order of TEST and KEY arguments.
	(remove-if-not (lambda (definition)
			 (and (typep definition 'component-definition)
			      (eq (component-parent (component definition))
				  module)))
	    definitions)))



;; -------
;; Systems
;; -------

(defmethod finalize progn
    ((definition system-definition) definitions
     &aux (foreign (foreignp definition))
	  (system (system definition)))
  "Compute system DEFINITION's defsystem dependency definitions."
  ;; #### NOTE: a case could be made to avoid rebuilding the whole list here,
  ;; and only add what's missing, but I don't think it's worth the trouble.
  (setf (defsystem-dependencies definition)
	(mapcan (lambda (dependency)
		  (resolve-dependency-specification
		   dependency system definitions foreign))
	  (mapcar #'reorder-dependency-def
	    (system-defsystem-depends-on system)))))

;; finalize.lisp ends here
