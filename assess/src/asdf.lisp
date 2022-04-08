;;; asdf.lisp --- ASDF definitions

;; Copyright (C) 2010-2013, 2015-2017, 2020-2022 Didier Verna

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

(in-package :net.didierverna.declt.assess)
(in-readtable :net.didierverna.declt)



;; ==========================================================================
;; Components
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
   (location
    :documentation "The component's location (a namestring)."
    :initform nil :accessor location)
   (parent
    :documentation "The parent definition for this definition's component."
    :accessor parent)
   (dependencies
    :documentation
    "The list of dependency definitions for this definition's component."
    :accessor dependencies))
  (:documentation "Abstract root class for ASDF components."))

(defmethod initialize-instance :after
    ((definition component-definition)
     &key
     &aux (pathname  (component-pathname (component definition)))
	  ;; #### NOTE: we don't really care if the file actually exists here
	  ;; or not. If it does, it could disappear later on, and vice versa.
	  ;; We still try to probe it, however, because it could allow us to
	  ;; dereference symbolic links.
	  (probed-pathname (when pathname (probe-file pathname))))
  "Compute component DEFINITION's location."
  (when (or probed-pathname pathname)
    (setf (location definition) (namestring (or probed-pathname pathname)))))

(defun component-definition-p (definition)
  "Return T if DEFINITION is a component definition."
  (typep definition 'component-definition))



;; ---------------------------
;; Public definition protocols
;; ---------------------------

(defmethod name ((definition component-definition))
  "Return component DEFINITION's component name."
  (component-name (component definition)))

(defmethod docstring ((definition component-definition))
  "Return component DEFINITION's description.
This is the same as the `description' function."
  (description definition))

(defun description (definition)
  "Return component DEFINITION's description."
  (component-description (component definition)))

(defun long-description (definition)
  "Return component DEFINITION's long description."
  (component-long-description (component definition)))

(defun definition-version (definition)
  "Return component DEFINITION's version string."
  (component-version (component definition)))

(defun if-feature (definition)
  "Return component DEFINITION's if-feature."
  (component-if-feature (component definition)))



;; --------------------------
;; Internal utility protocols
;; --------------------------

(defmethod source-pathname
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
  (:documentation "The class of ASDF file definitions.
All file definitions respond to the following public protocols, which see:
- `extension'."))

(defun file-definition-p (definition)
  "Return T if DEFINITION is a file definition."
  (typep definition 'file-definition))

(defun extension (definition)
  "Return file DEFINITION's file extension, if any."
  (file-type (file definition)))

(defclass source-file-definition (file-definition)
  ()
  (:documentation "The class of ASDF source file definitions."))

(defclass lisp-file-definition (source-file-definition)
  ((definitions
    :documentation "The list of definitions for this definition's file."
    :accessor definitions))
  (:documentation "The class of ASDF Lisp file definitions."))

(defun lisp-file-definition-p (definition)
  "Return T if DEFINITION is a Lisp file definition."
  (typep definition 'lisp-file-definition))

(defclass c-file-definition (source-file-definition)
  ()
  (:documentation "The class of ASDF C file definitions."))

(defclass java-file-definition (source-file-definition)
  ()
  (:documentation "The class of ASDF Java file definitions."))

(defclass static-file-definition (source-file-definition)
  ()
  (:documentation "The class of ASDF static file definitions."))

(defclass doc-file-definition (static-file-definition)
  ()
  (:documentation "The class of ASDF doc file definitions."))

(defclass html-file-definition (doc-file-definition)
  ()
  (:documentation "The class of ASDF HTML file definitions."))

(defun make-file-definition (file &optional foreign)
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
    :file file :foreign foreign))



;; ------------
;; System files
;; ------------

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
;; - the component's parent (system), for the SOURCE-PATHNAME protocol which
;;   needs to climb up the parents in order to find the system source file.
;;   Note that a system file may contain several system definitions, but it
;;   doesn't matter; we use any of these as the parent since the point is only
;;   to get to the system file.
;; Of course, we don't mess up the original ASDF system object by telling it
;; that it has a new unwanted child (it would go to a foster home directly).

(defclass cl-source-file.asd (asdf:cl-source-file)
  ((type :initform "asd"))
  (:documentation "A fake ASDF Lisp file component class for system files."))

(defclass system-file-definition (lisp-file-definition)
  ()
  (:documentation "The class of ASDF system file definitions.
This class represents ASDF system files as Lisp files. Because system files
are not components, we use an ad-hoc fake component class for them,
`cl-source-file.asd', which see."))

(defmethod initialize-instance :before
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




;; ==========================================================================
;; Modules
;; ==========================================================================

(defclass module-definition (component-definition)
  ((object :initarg :module :reader module) ;; slot overload
   (children
    :documentation "The list of child definitions for this definition's module."
    :accessor children))
  (:documentation "The class of ASDF module definitions."))

(defun module-definition-p (definition)
  "Return T if DEFINITION is a module definition."
  (typep definition 'module-definition))

(defun make-module-definition (module &optional foreign)
  "Make a new MODULE definition."
  (make-instance 'module-definition :module module :foreign foreign))




;; ==========================================================================
;; Systems
;; ==========================================================================

(defclass system-definition (module-definition)
  ((object :initarg :system :reader system) ;; slot overload
   (parent :initform nil) ;; slot -overload
   (maintainer-names
    :documentation "The list of maintainer names."
    :initform nil :accessor maintainer-names)
   (maintainer-emails
    :documentation "The list of maintainer emails."
    :initform nil :accessor maintainer-emails)
   (author-names
    :documentation "The list of author names."
    :initform nil :accessor author-names)
   (author-emails
    :documentation "The list of maintainer emails."
    :initform nil :accessor author-emails)
   (defsystem-dependencies
     :documentation "The list of defsystem dependency definitions."
     :accessor defsystem-dependencies))
  (:documentation "The class of ASDF system definitions."))

(defun system-definition-p (definition)
  "Return T if DEFINITION is a system definition."
  (typep definition 'system-definition))

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

(defun make-system-definition (system &optional foreign)
  "Make a new SYSTEM definition."
  (make-instance 'system-definition :system system :foreign foreign))



;; ---------------------------
;; Public definition protocols
;; ---------------------------

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

;; #### FIXME: maybe not public. See what to do about licenses.
(defun license-name (definition)
  "Return system DEFINITION's license name, or NIL."
  (system-license (system definition)))

;;; asdf.lisp ends here
