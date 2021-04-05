;; extract.lisp --- Definitions extraction

;; Copyright (C) 2020, 2021 Didier Verna

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

;; #### NOTE: there are more clever ways to create definitions, notably by
;; avoiding traversing the same structures several times. For example, modules
;; belong to a single system, and files belong to a single module, etc. On the
;; other hand, there are corner cases which would make it tricky to be clever
;; (e.g. complex systems which belong to the same file as the corresponding
;; simple system). So the way it's done below is much simpler and less
;; error-prone: create definitions type after type, independently from each
;; other, and resolve the cross-references later (that's what the finalization
;; process does), even those which were known right from the start.


;;; Code:

(in-package :net.didierverna.declt.extract)
(in-readtable :net.didierverna.declt)


;; ==========================================================================
;; Definitions Creation
;; ==========================================================================

;; ---------------
;; Local utilities
;; ---------------

(defun components (module type)
  "Return the list of all (sub)TYPE components found in MODULE's tree."
  ;; #### NOTE: we accept subtypes of TYPE because ASDF components might be
  ;; subclassed. An example of this is SBCL's grovel facility which subclasses
  ;; asdf:cl-source-file.
  (loop :for component :in (component-children module)
	:if (typep component type)
	  :collect component
	:if (typep component 'asdf:module)
	  :nconc (components component type)))



;; ------------------
;; System definitions
;; ------------------

(defun system-dependencies (system)
  "Return all system names from SYSTEM dependencies.
This includes both :defsystem-depends-on and :depends-on."
  (mapcar (lambda (dependency-def)
	    (reordered-dependency-def-system
	     (reorder-dependency-def dependency-def)))
    (append (system-defsystem-depends-on system)
	    (component-sideway-dependencies system))))

(defun sub-component-p
    (component directory
     ;; #### FIXME: not sure this is still valid, as we now have a specific
     ;; way of loading UIOP and ASDF.
     ;; #### NOTE: COMPONENT-PATHNAME can return nil when it's impossible to
     ;; locate the component's source. This happens for example with UIOP when
     ;; ASDF is embedded in a Lisp implementation like SBCL. Sabra Crolleton
     ;; fell on this issue when trying to document CL-PROJECT, which
     ;; explicitly depends on UIOP.
     &aux (component-pathname (component-pathname component)))
  "Return T if COMPONENT can be found under DIRECTORY."
  (when component-pathname
    (pathname-match-p component-pathname
		      (make-pathname :name :wild
				     :directory
				     (append (pathname-directory directory)
					     '(:wild-inferiors))))))

(defun subsystem
    (name system directory
     &aux (subsystem (resolve-dependency-name system name)))
  "Return NAME'd SYSTEM dependency if found under DIRECTORY, or nil."
  (when (sub-component-p subsystem directory)
    subsystem))

(defun subsystems (system directory)
  "Return the list of SYSTEM and all its dependencies found under DIRECTORY.
All dependencies are descended recursively. Both :defsystem-depends-on and
:depends-on are included. Potential duplicates are removed."
  (cons
   system
   (remove-duplicates
    (mapcan (lambda (subsystem) (subsystems subsystem directory))
      (remove-if #'null
	  (mapcar
	      (lambda (name) (subsystem name system directory))
	    (system-dependencies system))))
    :from-end t)))

(defun make-all-system-definitions (system)
  "Return a list of all system definitions for SYSTEM.
The only guarantee is that the definition for SYSTEM comes first.
The other considered systems are those found recursively in SYSTEM's
dependencies, and located under SYSTEM's directory.
See `subsystems' for more information."
  (mapcar #'make-system-definition
    (subsystems system (component-pathname system))))



;; ------------------
;; Module definitions
;; ------------------

;; #### WARNING: do not confuse this function with ASDF's MODULE-COMPONENTS
;; (which, BTW, is deprecated in favor of COMPONENT-CHILDREN).
(defun module-components (module)
  "Return the list of all module components found in MODULE's tree."
  (components module 'asdf:module))

(defun make-all-module-definitions (definitions)
  "Return a list of all module definitions for system DEFINITIONS."
  (mapcar #'make-module-definition
    (mapcan #'module-components
      (mapcar #'system definitions))))



;; ----------------
;; File definitions
;; ----------------

;; #### WARNING: in the unlikely but possible case that physical files would
;; be shared by different systems being documented at the same time, we would
;; end up with duplicate file documentation. The problem is that these files
;; would still be logically different, because they would belong to different
;; modules. We cannot really merge their definitions because they would have
;; different logical names (hence anchors etc.). So in the end, it's better to
;; leave it like that.

(defun file-components (module)
  "Return the list of all file components found in MODULE's tree."
  (components module 'asdf:file-component))

(defun make-all-file-definitions
    (definitions &aux (systems (mapcar #'system definitions)))
  "Return a list of all file definitions for system DEFINITIONS."
  (append (make-system-file-definitions systems)
	  (mapcar #'make-file-definition
	    (mapcan #'file-components systems))))



;; -------------------
;; Package definitions
;; -------------------

(defun make-all-package-definitions
    (file-definitions system-definitions
     &aux (packages (list-all-packages))
     ;; #### NOTE: I don't bother filtering out non-Lisp files here. No
     ;; package could be defined in those anyway.
	  (pathnames (mapcar (lambda (definition)
			       (component-pathname (file definition)))
		       file-definitions))
	  (prefixes (mapcar (lambda (definition)
			      (concatenate 'string
				(component-name (system definition))
				"/"))
		      system-definitions))
	  definitions)
  "Return a list of all package definitions for FILE- and SYSTEM-DEFINITIONS.
This list contains definitions for packages defined in the corresponding
files, or for which the source is not found, but the name is of the form
SYSTEM/... (case insensitive) for one of the corresponding systems."
  (dolist (package packages)
    (let ((pathname (object-source-pathname package))
	  (name (package-name package)))
      (when (or (member pathname pathnames :test #'equal)
		;; #### FIXME: remind me why we need that stuff?
		;; #### WARNING: shaky heuristic, bound to fail one day or
		;; another.
		(and (null pathname)
		     (find-if (lambda (prefix)
				(let ((pos (search prefix name
						   :test #'char-equal)))
				  (and pos (zerop pos))))
			      prefixes)))
	(push (make-package-definition package) definitions))))
  definitions)



;; ------------------
;; Symbol definitions
;; ------------------

;; #### PORTME.
(defun make-symbol-definitions
    (symbol &aux (setf-symbol `(setf ,symbol)) definitions)
  "Make and return a list of all existing definitions for SYMBOL."
  ;; Constants.
  (when (eql (sb-int:info :variable :kind symbol) :constant)
    (endpush (make-constant-definition symbol) definitions))
  ;; Special variables.
  (when (eql (sb-int:info :variable :kind symbol) :special)
    (endpush (make-special-definition symbol) definitions))
  ;; Symbol macros.
  (when (eql (sb-int:info :variable :kind symbol) :macro)
    (endpush (make-symbol-macro-definition symbol) definitions))
  ;; Macros.
  (when-let (macro (macro-function symbol))
    (endpush (make-macro-definition symbol macro) definitions))
  ;; Compiler macros.
  (when-let (compiler-macro (compiler-macro-function symbol))
    (endpush (make-compiler-macro-definition symbol compiler-macro)
	     definitions))
  ;; Setf compiler macros.
  (when-let (compiler-macro (compiler-macro-function setf-symbol))
    (endpush (make-compiler-macro-definition symbol compiler-macro t)
	     definitions))
  ;; Setf expanders
  (when-let (expander (sb-int:info :setf :expander symbol))
    (endpush (make-expander-definition symbol expander) definitions))
  ;; (Generic) functions.
  (when-let* ((function (and (fboundp symbol)
			     (not (macro-function symbol))
			     (fdefinition symbol)))
	      ;; #### NOTE: technically, the symbol can be extracted from the
	      ;; generic function object. However, using this general
	      ;; constructor is more homogeneous with the rest.
	      (definition (make-function-definition symbol function)))
    (if (typep definition 'generic-function-definition)
      (setq definitions
	    (append definitions
		    (cons definition (copy-list (methods definition)))))
      (endpush definition definitions)))
  ;; (Generic) setf functions.
  (when-let* ((function (and (fboundp setf-symbol) (fdefinition setf-symbol)))
	      ;; #### NOTE: technically, the symbol can be extracted from the
	      ;; generic function object. However, using this general
	      ;; constructor is more homogeneous with the rest.
	      (definition (make-function-definition symbol function :setf t)))
    (if (typep definition 'generic-function-definition)
      (setq definitions
	    (append definitions
		    (cons definition (copy-list (methods definition)))))
      (endpush definition definitions)))
  ;; Method combinations.
  ;; #### WARNING: method combinations are ill-defined in the Common Lisp
  ;; standard. In particular, they are not necessarily global objects and
  ;; don't have an actual namespace. This has been explained, first in a blog
  ;; (https://cutt.ly/AjIJXwA), and then in a ELS paper
  ;; (http://www.doi.org/10.5281/zenodo.3247610). As a consequence, in order
  ;; to be 100% correct (and also 200% pedantic), we should normally document
  ;; every single generic function's method combination as a local object. We
  ;; will assume, however, that the programmer has some sanity, and only
  ;; defines one method combination for every name. The corresponding object
  ;; will be documented like the other ones, and generic functions using it
  ;; will provide a cross-reference to it, also advertising the options in
  ;; use.
  ;;
  ;; After my ELS paper, Christophe made some changes to SBCL that
  ;; considerably sanitized the situation. Method combinations are now reified
  ;; in the SB-PCL::**METHOD-COMBINATIONS** hash table. Each entry is in fact
  ;; an SB-PCL::METHOD-COMBINATION-INFO structure, which contains, among other
  ;; things, a cache associating method combination options with actual method
  ;; combination objects. Thus, a method combination, as a general entity, and
  ;; as opposed to every single instantiation of it, is adequately and
  ;; uniquely represented by the entry in SB-PCL::**METHOD-COMBINATIONS**. See
  ;; also the comment about generic function stabilization in finalize.lisp.
  (when-let (combination (gethash symbol sb-pcl::**method-combinations**))
    (endpush (make-combination-definition symbol combination) definitions))
  ;; Structures, classes, and conditions,
  (when-let* ((classoid (find-class symbol nil))
	      (definition (make-classoid-definition symbol classoid)))
    (setq definitions
	  (append definitions
		  (cons definition (copy-list (direct-slots definition))))))
  ;; Typed structures
  (when-let* ((structure (sb-int:info :typed-structure :info symbol))
	      (definition (make-classoid-definition symbol structure)))
    (setq definitions
	  (append definitions
		  (cons definition (copy-list (direct-slots definition))))))
  ;; Types
  (when-let (expander (sb-int:info :type :expander symbol))
    (endpush (make-type-definition symbol expander) definitions))

  definitions)

(defun package-symbols (package &aux symbols)
  "Return the list of symbols from home PACKAGE."
  (do-symbols (symbol package symbols)
    (when (eq (symbol-package symbol) package)
      (push symbol symbols))))

(defun make-all-symbol-definitions (definitions)
  "Return a list of all symbol definitions for package DEFINITIONS."
  (mapcan #'make-symbol-definitions
    (mapcan #'package-symbols
      ;; #### NOTE: at that point, we don't have any foreign package
      ;; definitions here, so we don't need to filter them.
      (mapcar #'definition-package definitions))))




;; ==========================================================================
;; Extract Class
;; ==========================================================================

(defclass extract ()
  ((library-name :documentation "The library's name."
		 :accessor library-name)
   (tagline :documentation "The reference manual's tagline."
	    :accessor tagline)
   (library-version :documentation "The library's version."
		    :accessor library-version)
   (contact-names :documentation "The list of contact names for the library."
		  :accessor contact-names)
   (contact-emails :documentation "The list of contact emails for the library."
		   :accessor contact-emails)
   (copyright-years :documentation "A copyright years string."
		    :accessor copyright-years)
   (license :documentation "The library's license."
	    :accessor license)
   (introduction :documentation "Contents for an introduction chapter."
		 :accessor introduction)
   (conclusion :documentation "Contents for a conclusion chapter."
	       :accessor conclusion)
   (definitions :documentation "The list of definitions."
		:accessor definitions))
  (:documentation "The Extract class.
This is the class holding all extracted documentation information."))

(defmethod print-object ((extract extract) stream)
  "Show EXTRACT's library name."
  (print-unreadable-object (extract stream :type t)
    (princ (library-name extract) stream)))

(defun make-extract ()
  "Make a new extract."
  (make-instance 'extract))




;; ==========================================================================
;; Entry Point
;; ==========================================================================

(defun load-system (system-name &aux (system (find-system system-name)))
  "Load ASDF SYSTEM-NAME in a manner suitable to extract documentation.
Return the corresponding ASDF system.
SYSTEM-NAME is an ASDF system designator."
  ;;  Because of some bootstrapping issues, ASDF and UIOP need some
  ;; special-casing.
  (cond ((string= (asdf:coerce-name system-name) "uiop")
	 (load (merge-pathnames "uiop/uiop.asd"
				(system-source-directory
				 (asdf:find-system :asdf))))
	 (mapc #'load
	   (asdf:input-files :monolithic-concatenate-source-op
			     "asdf/driver")))
	((string= (asdf:coerce-name system-name) "asdf")
	 (setq system (find-system "asdf/defsystem"))
	 (mapc #'load
	   (asdf:input-files :monolithic-concatenate-source-op
			     "asdf/defsystem")))
	(t
	 (asdf:load-system system-name)))
  system)

(defun extract
    (system-name
     &key (library-name (if (stringp system-name)
			  system-name
			  (string-downcase system-name)))
	  (tagline nil taglinep)
	  (library-version nil library-version-p)
	  (contact nil contactp)
	  copyright-years
	  license
	  introduction
	  conclusion
     &allow-other-keys ;; lazy calling from DECLT
     &aux (system (load-system system-name))
	  contact-names contact-emails
	  (extract (make-extract)))
  "Extract and return documentation information for ASDF SYSTEM-NAME.
The documentation information is returned in a EXTRACT structure, which see.

SYSTEM-NAME is an ASDF system designator. The following keyword parameters
allow to specify or override some bits of information.
- LIBRARY-NAME: name of the library being documented. Defaults to the system
  name.
- TAGLINE: small text to be used as the manual's subtitle, or NIL.
  Defaults to the system long name or description.
- LIBRARY-VERSION: version information, or NIL.
  Defaults to the system version.
- CONTACT: contact information, or NIL. Defaults to the system maintainer(s)
  and author(s). Accepts a contact string, or a list of such. See
  `parse-contact-string' for more information.
- COPYRIGHT-YEARS: copyright years information or NIL. Defaults to the current
  year.
- LICENSE: license information. Defaults to NIL. Also accepts :mit, :boost,
  :bsd, :gpl, and :lgpl.
- INTRODUCTION: introduction chapter contents in Texinfo format.
  Defaults to NIL.
- CONCLUSION: conclusion chapter contents in Texinfo format.
  Defaults to NIL."

  (check-type library-name non-empty-string)
  (setf (library-name extract) library-name)
  (unless taglinep
    (setq tagline (or (system-long-name system)
		      (component-description system))))
  (unless (one-liner-p tagline)
    (setq tagline nil))
  (when (and tagline (char= (aref tagline (1- (length tagline))) #\.))
    (setq tagline (subseq tagline 0 (1- (length tagline)))))
  (setf (tagline extract) tagline)
  (unless library-version-p
    (setq library-version (component-version system)))
  (unless (one-liner-p library-version)
    (setq library-version nil))
  (setf (library-version extract) library-version)
  (unless contactp
    (setq contact (system-author system))
    (when (stringp contact) (setq contact (list contact)))
    (cond ((stringp (system-maintainer system))
	   (push (system-maintainer system) contact))
	  ((consp (system-maintainer system))
	   (setq contact (append (system-maintainer system) contact)))))
  (multiple-value-bind (names emails) (|parse-contact(s)| contact)
    (setq contact-names names
	  contact-emails emails))
  (when (and (= (length contact-names) 1)
	     (not contactp)
	     (null (car contact-emails))
	     (one-liner-p (system-mailto system)))
    (setq contact-emails (list (system-mailto system))))
  (setf (contact-names extract) contact-names)
  (setf (contact-emails extract) contact-emails)
  (setq copyright-years
	(or copyright-years
	    (multiple-value-bind (second minute hour date month year)
		(get-decoded-time)
	      (declare (ignore second minute hour date month))
	      (format nil "~A" year))))
  (unless (one-liner-p copyright-years)
    (setq copyright-years nil))
  (setf (copyright-years extract) copyright-years)
  (when license
    (setq license (assoc license *licenses*))
    (unless license
      (error "License not found.")))
  (setf (license extract) license)
  (setf (introduction extract) introduction)
  (setf (conclusion extract) conclusion)

  (let* ((system-definitions (make-all-system-definitions system))
	 (module-definitions (make-all-module-definitions system-definitions))
	 (file-definitions (make-all-file-definitions system-definitions))
	 (package-definitions
	   (make-all-package-definitions file-definitions system-definitions))
	 (symbol-definitions (make-all-symbol-definitions package-definitions)))
    (setf (definitions extract)
	  (append system-definitions module-definitions file-definitions
		  package-definitions symbol-definitions)))

  (finalize (definitions extract))

  extract)

;;; extract.lisp ends here
