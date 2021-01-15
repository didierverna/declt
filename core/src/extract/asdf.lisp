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
   (parent :documentation "The parent definition." :accessor parent))
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

(defun long-description (definition)
  "Return component DEFINITION's long description."
  (component-long-description (component definition)))

;; #### NOTE: this is not very satisfactory, but Declt has a VERSION regular
;; function for its own version information.
(defun version-string (definition)
  "Return component DEFINITION's version string."
  (component-version (component definition)))

(defun if-feature (definition)
  "Return component DEFINITION's if-feature."
  (component-if-feature (component definition)))

(defun dependencies (definition)
  "Return component DEFINITION's dependencies."
  (component-sideway-dependencies (component definition)))



;; ==========================================================================
;; Files
;; ==========================================================================

;; The hierarchy below mimics that of ASDF (apart from the 3 kinds of Lisp
;; files), which is probably overkill, but we never know.
;; #### NOTE: we currently don't create foreign files.

(defclass file-definition (component-definition)
  ((object :initarg :file :reader file)) ;; slot overload
  (:documentation "The FILE-DEFINITION class.
This is the base class for ASDF file definitions."))

(defun make-file-definition (file &optional foreign)
  "Make a new FILE definition, possibly FOREIGN."
  (make-instance 'file-definition :file file :foreign foreign))


(defclass source-file-definition (file-definition)
  ()
  (:documentation "The SOURCE-FILE-DEFINITION class."))

(defun make-source-file-definition (file &optional foreign)
  "Make a new source FILE definition, possibly FOREIGN."
  (make-instance 'source-file-definition :file file :foreign foreign))


(defclass lisp-file-definition (source-file-definition)
  ((package-definitions
    :documentation "The corresponding package definitions."
    :accessor package-definitions)
   (symbol-definitions
    :documentation "The corresponding symbol definitions."
    :accessor symbol-definitions))
  (:documentation "The LISP-FILE-DEFINITION class."))

(defun make-lisp-file-definition (file &optional foreign)
  "Make a new Lisp FILE definition, possibly FOREIGN."
  (make-instance 'lisp-file-definition :file file :foreign foreign))


;; #### FIXME: those two functions have to be utterly inefficient. Hopefully,
;; this will go away with the addition of an EXPORTED slot.
(defmethod external-definitions ((definition lisp-file-definition))
  "Return Lisp file DEFINITION's external definitions."
  (remove-if-not
      (lambda (symbol)
	(member symbol (package-external-symbols (symbol-package symbol))))
      (symbol-definitions definition)
    :key #'definition-symbol))

(defmethod internal-definitions ((definition lisp-file-definition))
  "Return Lisp file DEFINITION's internal definitions."
  (remove-if
      (lambda (symbol)
	(member symbol (package-external-symbols (symbol-package symbol))))
      (symbol-definitions definition)
    :key #'definition-symbol))

;; #### WARNING: gross hack going on below. ASDF system files are technically
;; Lisp files because what they contain is Lisp, but they are not ASDF
;; components. We still want to document them as other Lisp files, without
;; replicating too much of the infrastructure. The solution is to use a fake
;; subclass of ASDF's CL-SOURCE-FILE class, and create fake ASDF components
;; corresponding to system files.

(defclass cl-source-file.asd (asdf:cl-source-file)
  ((type :initform "asd"))
  (:documentation "A fake ASDF Lisp file component class for system files."))

(defclass system-file-definition (lisp-file-definition)
  ((system-definitions :documentation "The corresponding system definitions."
		       :accessor system-definitions))
  (:documentation "The System File Definition class.
This class represents ASDF system files as Lisp files. Because system files
are not components, we use an ad-hoc fake component class for them,
`cl-source-file.asd', which see."))

(defmethod initialize-instance :after
    ((definition system-file-definition)
     &key pathname
     &aux (component (make-instance 'cl-source-file.asd
		       :name (pathname-name pathname))))
  "Create and store a fake ASDF comoponent representing the system file."
  ;; #### NOTE: this slot is internal, and has no initarg in ASDF.
  (setf (slot-value component 'asdf::absolute-pathname) pathname)
  (setf (slot-value definition 'object) component))

(defun make-system-file-definition (pathname)
  "Make a new system file definition for system PATHNAME."
  (make-instance 'system-file-definition :pathname pathname))

(defun make-system-file-definitions (system-definitions &aux definitions)
  "Make a list of system file definitions for SYSTEM-DEFINITIONS.
Multiple systems may be defined in the same file. There is however only one
definition for each file."
  (dolist (system (mapcar #'system system-definitions))
    ;; #### FIXME: remind me why/when the system source file can be be null?
    (when-let (source-file (system-source-file system))
      (unless (find source-file definitions
		:key (lambda (definition)
		       (component-pathname (file definition)))
		:test #'equal)
	(push (make-system-file-definition source-file) definitions))))
  (nreverse definitions))


(defclass c-file-definition (source-file-definition)
  ()
  (:documentation "The C-FILE-DEFINITION class."))

(defun make-c-file-definition (file &optional foreign)
  "Make a new C FILE definition, possibly FOREIGN."
  (make-instance 'c-file-definition :file file :foreign foreign))


(defclass java-file-definition (source-file-definition)
  ()
  (:documentation "The JAVA-FILE-DEFINITION class."))

(defun make-java-file-definition (file &optional foreign)
  "Make a new Java FILE definition, possibly FOREIGN."
  (make-instance 'java-file-definition :file file :foreign foreign))


(defclass static-file-definition (source-file-definition)
  ()
  (:documentation "The STATIC-FILE-DEFINITION class."))

(defun make-static-file-definition (file &optional foreign)
  "Make a new static FILE definition, possibly FOREIGN."
  (make-instance 'static-file-definition :file file :foreign foreign))


(defclass doc-file-definition (static-file-definition)
  ()
  (:documentation "The DOC-FILE-DEFINITION class."))

(defun make-doc-file-definition (file &optional foreign)
  "Make a new doc FILE definition, possibly FOREIGN."
  (make-instance 'doc-file-definition :file file :foreign foreign))


(defclass html-file-definition (doc-file-definition)
  ()
  (:documentation "The HTML-FILE-DEFINITION class."))

(defun make-html-file-definition (file &optional foreign)
  "Make a new HTML FILE definition, possibly FOREIGN."
  (make-instance 'html-file-definition :file file :foreign foreign))


;; ------------------------------------
;; Definition file definitions protocol
;; ------------------------------------

;; #### FIXME: similarly to DEFINITION-PACKAGE-DEFINITIONS, this function is
;; #### likely to be wrong in some corner cases such as slot definitions. I
;; #### should check that thoroughly.

(defgeneric definition-file-definitions (definition file)
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

;; #### FIXME: tmp hack
(defun definitions-from-file (file definitions)
  "Return the subset of DEFINITIONS that belong to FILE."
  (mapcan (lambda (definition) (definition-file-definitions definition file))
    definitions))



;; ==========================================================================
;; Modules
;; ==========================================================================

(defclass module-definition (component-definition)
  ((object :initarg :module :reader module) ;; slot overload
   (children :documentation "The module's children." :accessor children))
  (:documentation "The Module Definition class."))

;; #### NOTE: we currently don't create foreign modules.
(defun make-module-definition (module &optional foreign)
  "Make a new MODULE definition, possibly FOREIGN."
  (make-instance 'module-definition :module module :foreign foreign))



;; ==========================================================================
;; Systems
;; ==========================================================================

(defclass system-definition (module-definition)
  ((object :initarg :system :reader system) ;; slot overload
   (parent :initform nil) ;; slot -overload
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
  "Perform post-initialization of system DEFINITION.
More specifically:
- extract names and emails for authors and maintainers."
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

;; #### NOTE: we currently don't create foreign systems.
(defun make-system-definition (system &optional foreign)
  "Make a new SYSTEM definition, possibly FOREIGN."
  (make-instance 'system-definition :system system :foreign foreign))


;; ----------------
;; Pseudo-accessors
;; ----------------

(defun long-name (definition)
  "Return system DEFINITION's long name, if any."
  (system-long-name (system definition)))

(defun mailto (definition)
  "Return system DEFINITION's mailto, if any."
  (system-mailto (system definition)))

(defun homepage (definition)
  "Return system DEFINITION's homepage, if any."
  (system-homepage (system definition)))

(defun source-control (definition)
    "Return system DEFINITION's source control, if any."
  (system-source-control (system definition)))

(defun bug-tracker (definition)
  "Return system DEFINITION's bug tracker, if any."
  (system-bug-tracker (system definition)))

;; #### NOTE: there's a LICENSE accessor on extracts, so this needs to be a
;; #### method.
(defmethod license ((definition system-definition))
  "Return system DEFINITION's license, if any."
  (system-license (system definition)))

(defun defsystem-dependencies (definition)
  "Return system DEFINITION's defsystem dependencies."
  (system-defsystem-depends-on (system definition)))


;; ---------
;; Utilities
;; ---------

(defun lisp-pathnames
    (system &aux (file (system-source-file system))
		 (lisp-pathnames
		  (mapcar #'component-pathname (lisp-components system))))
  "Return the list of all ASDF SYSTEM's Lisp source file pathnames.
The list includes the system definition file."
  (if file
    (cons file lisp-pathnames)
    lisp-pathnames))

(defun file-packages (file)
  "Return the list of all packages defined in FILE."
  (remove-if-not (lambda (source) (equal source file)) (list-all-packages)
    :key #'source))

(defun system-located-packages (system)
  "Return the list of located packages defined in ASDF SYSTEM.
These are the packages for which source location is available via
introspection. We can hence verify that the file defining them indeed belongs
to SYSTEM."
  (mapcan #'file-packages (lisp-pathnames system)))

;; #### WARNING: shaky heuristic, bound to fail one day or another.
(defun system-unlocated-packages
    (system &aux (prefix (concatenate 'string (component-name system) "/"))
		 (length (length prefix)))
  "Return the list of unlocated packages defined in ASDF SYSTEM.
These are the packages for which source location is unavailable via
introspection. We thus need to guess. The current heuristic considers packages
named SYSTEM/foobar, regardless of case."
  (remove-if-not
      (lambda (package)
	(let ((package-name (package-name package)))
	  (and (not (source package))
	       (> (length package-name) length)
	       (string-equal prefix (subseq package-name 0 length)))))
      (list-all-packages)))

;; #### FIXME: this should become a PACKAGES protocol.
(defun system-packages (system)
  "Return the list of packages defined in ASDF SYSTEM."
  (append (system-located-packages system) (system-unlocated-packages system)))

;;; asdf.lisp ends here
