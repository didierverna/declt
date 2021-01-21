;;; asdf.lisp --- ASDF definitions

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
;; Component Definition Basics
;; ==========================================================================

;; #### NOTE: we more or less need to follow the ASDF hierarchy, which is not
;; always correct. For example, the COMPONENT class has a PARENT slot, so we
;; do the same here. It's technically wrong, however, because systems are
;; components, but they never have a parent (the slot is always NIL). Maybe
;; ASDF does this for simplicity. It surely makes our life simpler as well,
;; especially since we have a peculiar order for calling the DOCUMENT methods,
;; not following the hierarchy (SYSTEM -> COMPONENT -> MODULE). It would be
;; more difficult to advertise the PARENT slot at the right place if it didn't
;; belong here.

(defabstract component-definition (definition)
  ((object :reader component) ;; slot overload
   (parent-definition :documentation "The corresponding parent definition."
		      :accessor parent-definition))
  (:documentation "The COMPONENT-DEFINITION class.
This is the base class for ASDF definitions."))



;; ----------------
;; Pseudo-accessors
;; ----------------

(defmethod name ((definition component-definition))
  "Return component DEFINITION's component name."
  (component-name (component definition)))

(defun description (definition)
  "Return component DEFINITION's description."
  (component-description (component definition)))

(defmethod docstring ((definition component-definition))
  "Return component DEFINITION's description.
This is the same as the `description' function."
  (description definition))

(defun long-description (definition)
  "Return component DEFINITION's long description."
  (component-long-description (component definition)))

(defun definition-version (definition)
  "Return component DEFINITION's version string."
  (component-version (component definition)))

(defun if-feature (definition)
  "Return component DEFINITION's if-feature."
  (component-if-feature (component definition)))

(defun dependencies (definition)
  "Return component DEFINITION's dependencies."
  (component-sideway-dependencies (component definition)))

(defmethod source
    ((definition component-definition) &aux (component (component definition)))
  "Return component DEFINITION's source pathname.
This actually is the corresponding system's source file."
  (while (component-parent component)
    (setq component (component-parent component)))
  (system-source-file component))




;; ==========================================================================
;; Files
;; ==========================================================================

;; The hierarchy below mimics that of ASDF (apart from the 3 kinds of Lisp
;; files), which is probably overkill, but we never know.

(defclass file-definition (component-definition)
  ((object :initarg :file :reader file)) ;; slot overload
  (:documentation "The FILE-DEFINITION class.
This is the base class for ASDF file definitions."))

(defclass source-file-definition (file-definition)
  ()
  (:documentation "The SOURCE-FILE-DEFINITION class."))

(defclass lisp-file-definition (source-file-definition)
  ((definitions :documentation "The corresponding definitions."
		:accessor definitions))
  (:documentation "The LISP-FILE-DEFINITION class."))

(defun lisp-file-definition-p (definition)
  "Return T if DEFINITION is for a Lisp file.
Note that ASDF system files are considered as Lisp files."
  (typep definition 'lisp-file-definition))

;; #### WARNING: gross hack going on below. ASDF system files are technically
;; Lisp files because what they contain is Lisp, but they are not ASDF
;; components. We still want to document them as other Lisp files, without
;; replicating too much of the infrastructure. The solution below consists in
;; using a fake subclass of ASDF's CL-SOURCE-FILE, called CL-SOURCE-FILE.ASD.
;; Each system file has an instance of this class stored as its
;; pseudo-component object. This instance is not completely filled in, as the
;; real ones; only what's necessary is there. Namely, this includes:
;; - the component's name (file basename) for the NAME protocol,
;; - the component's absolute pathname, for the COMPONENT-PATHNAME function,
;;   #### FIXME: this is probably not required anymore.
;; - the component's parent (system), for the SOURCE protocol which needs to
;;   climb up the parents in order to find the system source file. Note that a
;;   system file may contain several system definitions, but it doesn't
;;   matter; we use any of these as the parent since the point is only to get
;;   to the system file.
;; Of course, we don't mess up the original ASDF system object by telling it
;; that it has a new unwanted child (it would go to a foster home directly).

(defclass cl-source-file.asd (asdf:cl-source-file)
  ((type :initform "asd"))
  (:documentation "A fake ASDF Lisp file component class for system files."))

(defclass system-file-definition (lisp-file-definition)
  ()
  (:documentation "The System File Definition class.
This class represents ASDF system files as Lisp files. Because system files
are not components, we use an ad-hoc fake component class for them,
`cl-source-file.asd', which see."))

(defmethod initialize-instance :after
    ((definition system-file-definition)
     &key system
     &aux (pathname (system-source-file system))
	  (component (make-instance 'cl-source-file.asd
		       :name (pathname-name pathname)
		       :parent system)))
  "Create and store a fake ASDF comoponent representing the system file."
  ;; #### NOTE: this slot is internal, and has no initarg in ASDF.
  (setf (slot-value component 'asdf::absolute-pathname) pathname)
  (setf (slot-value definition 'object) component))

(defun make-system-file-definition (system)
  "Make a new system file definition for SYSTEM."
  (make-instance 'system-file-definition :system system))

(defun make-system-file-definitions (systems)
  "Make a list of system file definitions for SYSTEMS.
Multiple systems may be defined in the same file. There is however only one
definition for each file."
  ;; #### FIXME: remind me why/when the system source file can be be null?
  (setq systems (remove-if #'null systems :key #'system-source-file))
  (mapcar #'make-system-file-definition
    (remove-duplicates systems
      :key #'system-source-file :test #'equal :from-end t)))

(defclass c-file-definition (source-file-definition)
  ()
  (:documentation "The C-FILE-DEFINITION class."))

(defclass java-file-definition (source-file-definition)
  ()
  (:documentation "The JAVA-FILE-DEFINITION class."))

(defclass static-file-definition (source-file-definition)
  ()
  (:documentation "The STATIC-FILE-DEFINITION class."))

(defclass doc-file-definition (static-file-definition)
  ()
  (:documentation "The DOC-FILE-DEFINITION class."))

(defclass html-file-definition (doc-file-definition)
  ()
  (:documentation "The HTML-FILE-DEFINITION class."))

(defun make-file-definition (file)
  "Make a new FILE definition.
The concrete class of the new definition depends on the kind of FILE."
  (make-instance
      (etypecase file
	;; #### WARNING: the order is important!
	(asdf:cl-source-file 'lisp-file-definition)
	(asdf:c-source-file 'c-file-definition)
	(asdf:java-source-file 'java-file-definition)
	(asdf:html-file 'html-file-definition)
	(asdf:doc-file 'doc-file-definition)
	(asdf:static-file 'static-file-definition)
	(asdf:source-file 'source-file-definition)
	(asdf:file-component 'file-definition))
    :file file))


;; ------------------------------------
;; Definition file definitions protocol
;; ------------------------------------

;; #### FIXME: similarly to DEFINITION-PACKAGE-DEFINITIONS, this function is
;; #### likely to be wrong in some corner cases such as slot definitions. I
;; #### should check that thoroughly.

#+()(defgeneric definition-file-definitions (definition file)
  (:documentation
   "Return the list of definitions from DEFINITION that belong to FILE.")
  (:method (definition file)
    "Default method for definitions not containing sub-definitions."
    (when (equal (source definition) file)
      (list definition)))
  (:method ((macro macro-definition) file)
    "Handle MACRO and its setf expander."
    (nconc (call-next-method)
	   (when (access-expander-definition macro)
	     (definition-file-definitions
	      (access-expander-definition macro)
	      file))))
  (:method ((accessor accessor-definition) file)
    "Handle ACCESSOR, its writer and its setf expander."
    (nconc (call-next-method)
	   (when (writer-definition accessor)
	     (definition-file-definitions
	      (writer-definition accessor)
	      file))
	   (when (access-expander-definition accessor)
	     (definition-file-definitions
	      (access-expander-definition accessor)
	      file))))
  (:method ((accessor-method accessor-method-definition) file)
    "Handle ACCESSOR-METHOD and its writer method."
    (nconc (call-next-method)
	   (definition-file-definitions
	    (writer-definition accessor-method)
	    file)))
  (:method ((generic generic-definition) file)
    "Handle GENERIC function and its methods."
    (nconc (call-next-method)
	   (mapcan (lambda (method)
		     (definition-file-definitions method file))
		   (method-definitions generic))))
  (:method ((generic-accessor generic-accessor-definition) file)
    "Handle GENERIC-ACCESSOR, its generic writer and its setf expander."
    (nconc (call-next-method)
	   (when (writer-definition generic-accessor)
	     (definition-file-definitions
	      (writer-definition generic-accessor)
	      file))
	   (when (access-expander-definition generic-accessor)
	     (definition-file-definitions
	      (access-expander-definition generic-accessor)
	      file)))))




;; ==========================================================================
;; Modules
;; ==========================================================================

(defclass module-definition (component-definition)
  ((object :initarg :module :reader module) ;; slot overload
   (child-definitions :documentation "The list of module child definitions."
		      :accessor child-definitions))
  (:documentation "The Module Definition class."))

(defun make-module-definition (module)
  "Make a new MODULE definition."
  (make-instance 'module-definition :module module))




;; ==========================================================================
;; Systems
;; ==========================================================================

(defclass system-definition (module-definition)
  ((object :initarg :system :reader system) ;; slot overload
   (parent-definition :initform nil) ;; slot -overload
   (maintainer-names :documentation "The list of maintainer names."
		     :initform nil :accessor maintainer-names)
   (maintainer-emails :documentation "The list of maintainer emails."
		      :initform nil :accessor maintainer-emails)
   (author-names :documentation "The list of author names."
		 :initform nil :accessor author-names)
   (author-emails :documentation "The list of maintainer emails."
		  :initform nil :accessor author-emails))
  (:documentation "The System Definition class."))

(defmethod initialize-instance :after
    ((definition system-definition) &key &aux (system (system definition)))
  "Extract names and emails for authors and maintainers."
  (multiple-value-bind (maintainers emails)
      (|parse-contact(s)| (system-maintainer system))
    (when maintainers
      (setf (maintainer-names definition) maintainers)
      (setf (maintainer-emails definition) emails)))
  (multiple-value-bind (authors emails)
      (|parse-contact(s)| (system-author system))
    (when authors
      (setf (author-names definition) authors)
      (setf (author-emails definition) emails))))

(defun make-system-definition (system)
  "Make a new SYSTEM definition."
  (make-instance 'system-definition :system system))


;; ----------------
;; Pseudo-accessors
;; ----------------

(defun long-name (definition)
  "Return system DEFINITION's long name, or NIL."
  (system-long-name (system definition)))

(defun mailto (definition)
  "Return system DEFINITION's mailto, or NIL."
  (system-mailto (system definition)))

(defun homepage (definition)
  "Return system DEFINITION's homepage, or NIL."
  (system-homepage (system definition)))

(defun source-control (definition)
    "Return system DEFINITION's source control, or NIL."
  (system-source-control (system definition)))

(defun bug-tracker (definition)
  "Return system DEFINITION's bug tracker, or NIL."
  (system-bug-tracker (system definition)))

(defun license-name (definition)
  "Return system DEFINITION's license name, or NIL."
  (system-license (system definition)))




;; ==========================================================================
;; Utilities
;; ==========================================================================

;; #### WARNING: remember that a Lisp file definition contains symbol
;; definitions, but also other things like packages and asdf components.

(defmethod public-definitions ((definition lisp-file-definition))
  "Return Lisp file DEFINITION's public definitions."
  (remove-if-not (lambda (definition)
		   (and (typep definition 'symbol-definition)
			(publicp definition)))
      (definitions definition)))

(defmethod private-definitions ((definition lisp-file-definition))
  "Return Lisp file DEFINITION's private definitions."
  (remove-if (lambda (definition)
	       (or (not (typep definition 'symbol-definition))
		   (publicp definition)))
      (definitions definition)))

;;; asdf.lisp ends here
