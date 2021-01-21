;; extract.lisp --- Documentation information extraction

;; Copyright (C) 2020 Didier Verna

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
		:accessor definitions)
   (hyperlinksp :documentation "Whether to produce hyperlinks."
		:accessor hyperlinksp))
  (:documentation "The Extract class.
This is the class holding all extracted documentation information."))

(defun make-extract ()
  "Make a new extract."
  (make-instance 'extract))

;; #### FIXME: should this become a general definition protocol?
;; This is used rather often (in fact, not so much! ;-)) so it is worth a
;; shortcut.
(defun location (extract)
  "Return EXTRACT's main system location."
  (system-directory (system (car (system-definitions extract)))))

;; #### NOTE: there are currently two equivalent ways to compute the values of
;; the two functions below. Namely, by filtering on:
;; 1. extract -> symbol definitions (as it is done below),
;; 2. extract -> packages -> symbol definitions.
;; The reason we use solution #1 is that later on, when/if we add some kind of
;; foreign definitions (like, those pertaining to one of our files, but not to
;; one of our packages), there will be a difference in the results. On top of
;; that, I will surely add an EXPORTED slot to the definition class, which
;; will make things even easier.




;; ==========================================================================
;; Definitions Creation
;; ==========================================================================

;; #### NOTE: there are more clever ways to populate the extract with the
;; relevant definitions, notably by avoiding traversing the same structures
;; several times. For example, modules belong to a single system, and files
;; belong to a single module, etc. On the other hand, there are corner cases
;; which would make it tricky to be clever (e.g. complex systems which belong
;; to the same file as the corresponding simple system). So the way it's done
;; below is much simpler and less error-prone: create everything first, and
;; resolve the cross-references later, even those which were known right from
;; the start.

;; ------------------
;; System definitions
;; ------------------

(defun dependency-def-system (dependency-def)
  "Extract a system name from ASDF DEPENDENCY-DEF specification."
  (typecase dependency-def ;; RTE to the rescue!
    (list (ecase (car dependency-def)
	    (:feature (dependency-def-system (third dependency-def)))
	    (:version (second dependency-def))
	    (:require nil)))
    (otherwise dependency-def)))

;; #### FIXME: there is redundancy with RENDER-DEPENDENCIES. I should write a
;; more abstract dependency walker.
(defun system-dependency-subsystem (dependency-def system directory)
  "Return SYSTEM's DEPENDENCY-DEF subsystem if found under DIRECTORY, or nil."
  (when-let* ((name (dependency-def-system dependency-def))
	      (dependency (resolve-dependency-name system name)))
    (when (sub-component-p dependency directory)
      dependency)))

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
	      (lambda (dependency)
		(system-dependency-subsystem dependency system directory))
	    (system-dependencies system))))
    :from-end t)))

(defun make-all-system-definitions (system)
  "Return a list of all system definitions for SYSTEM.
The definition for SYSTEM is first. The other considered systems are those
found recursively in SYSTEM's dependencies, and located under SYSTEM's
directory."
  (mapcar #'make-system-definition
    (subsystems system (system-directory system))))



;; ------------------
;; Module definitions
;; ------------------

;; #### WARNING: do not confuse this function with ASDF's MODULE-COMPONENTS
;; (which, BTW, is deprecated in favor of COMPONENT-CHILDREN).
(defun module-components (parent)
  "Return the list of all module components from ASDF PARENT."
  (components parent 'asdf:module))

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

(defun file-components (parent)
  "Return the list of all file components from ASDF PARENT."
  (components parent 'asdf:file-component))

(defun make-all-file-definitions
    (definitions &aux (systems (mapcar #'system definitions)))
  "Return a list of all file definitions for system DEFINITIONS."
  (append (make-system-file-definitions systems)
	  (mapcar #'make-file-definition (mapcan #'file-components systems))))



;; -------------------
;; Package definitions
;; -------------------

(defun file-packages (file)
  "Return the list of all packages defined in FILE."
  (remove-if-not (lambda (source) (equal source file)) (list-all-packages)
    :key #'object-source-pathname))

;; #### FIXME: remind me why we need that stuff?
;; #### WARNING: shaky heuristic, bound to fail one day or another.
(defun unfiled-packages
    (system &aux (prefix (concatenate 'string (component-name system) "/"))
		 (length (length prefix)))
  "Return the list of unlocated packages defined in ASDF SYSTEM.
These are the packages for which source location is unavailable via
introspection. We thus need to guess. The current heuristic considers packages
named SYSTEM/foobar, regardless of case."
  (remove-if-not
      (lambda (package)
	(let ((package-name (package-name package)))
	  (and (not (object-source-pathname package))
	       (> (length package-name) length)
	       (string-equal prefix (subseq package-name 0 length)))))
      (list-all-packages)))

(defun make-all-package-definitions (files systems)
  "Return a list of all package definitions for FILES and SYSTEMS definitions."
  (mapcar #'make-package-definition
    (append (mapcan #'file-packages
	      (mapcar #'component-pathname
		(mapcar #'file
		  (remove-if-not #'lisp-file-definition-p files))))
	    (mapcan #'unfiled-packages
	      (mapcar #'system systems)))))



;; ------------------
;; Symbol definitions
;; ------------------

(defun package-symbols (package &aux symbols)
  "Return the list of symbols from home PACKAGE."
  (do-symbols (symbol package symbols)
    (when (eq (symbol-package symbol) package)
      (push symbol symbols))))

(defun make-all-symbol-definitions (packages)
  "Return a list of all symbol definitions for PACKAGES definitions."
  (mapcan #'make-symbol-definitions
    (mapcan #'package-symbols
      ;; #### NOTE: at that point, we don't have any foreign package
      ;; definitions here, so we don't need to filter them.
      (mapcar #'definition-package packages))))




;; ==========================================================================
;; Definitions Finalization
;; ==========================================================================

;; #### WARNING: as a general rule of thumb, never assume anything about the
;; finalization state of the other definitions you access when finalizing a
;; definition. There are many small methods here and there that are executed
;; and it's too error-prone to even try to rely on their execution order
;; (scattered behavior, that's the drawback of OO).
;; For example, this is guaranteed to work all the time:
;; (symbol-package (definition-symbol definition)
;; whereas this might fail if the definition has not been finalized yet:
;; (definition-package (package-definition definition)).

(defgeneric finalize (definition definitions)
  (:documentation "Finalize DEFINITION in DEFINITIONS.")
  (:method-combination progn))



;; ------------------
;; Symbol definitions
;; ------------------

(defmethod finalize progn ((definition symbol-definition) definitions)
  "Fill in symbol DEFINITION's package definition.
New foreign package definitions may be created and added at the end of
DEFINITIONS in the process."
  (setf (package-definition definition)
	(get-package-definition (symbol-package (definition-symbol definition))
				definitions)))



;; -------------------
;; Package definitions
;; -------------------

;; #### NOTE: admittedly, it's a bit too much to make this a FINALIZE method,
;; as there's only one package definition class. But it's more coherent like
;; that, and who knows what's gonna happen in the future.

(defmethod finalize progn
    ((definition package-definition) definitions
     &aux (package (definition-package definition)))
  "Fill in package DEFINITION's use, used-by, and definitions lists.
New foreign package definitions may be created and added at the end of
DEFINITIONS in the process."
  ;; 1. Use list.
  (setf (use-definitions definition)
	(mapcar (lambda (package)
		  (get-package-definition package definitions))
	  (package-use-list package)))
  ;; 2. Used-by list.
  (setf (used-by-definitions definition)
	(mapcar (lambda (package)
		  (get-package-definition package definitions))
	  (package-used-by-list package)))
  ;; 3. Symbol definitions list.
  ;; #### FIXME: this will break in corner cases because it's in contradiction
  ;; with the general rule of thumb mentioned above: if new symbol definitions
  ;; are added later on, we may miss them here. We probably need a two-phase
  ;; finalization: one for adding new definitions, and one when we're sure
  ;; it's not gonna happen again.
  (setf (definitions definition)
	;; #### FIXME: write a RETAIN or KEEP function, also inverting the
	;; order of TEST and KEY arguments.
	(remove-if-not (lambda (definition)
			 (and (typep definition 'symbol-definition)
			      (eq (symbol-package
				   (definition-symbol definition))
				  package)))
	    definitions)))



;; ---------------------
;; Component definitions
;; ---------------------

(defmethod finalize progn
    ((definition component-definition) definitions
     &aux (parent (component-parent (component definition))))
  "Fill in component DEFINITION's parent definition."
  ;; #### WARNING: systems are components, but don't have a parent so PARENT
  ;; is NIL for them here. We don't want to search definitions for a NIL
  ;; object because we'd fall on constants, special variables, symbol macros,
  ;; or types. So we need this workaround. The parent-definition slot for
  ;; system definitions will actually be set to NIL through the overloaded
  ;; :initform provided in the corresponding class.
  (when parent
    (setf (parent-definition definition)
	  (find-definition parent definitions))))



;; ----------------
;; File definitions
;; ----------------

(defmethod finalize progn
    ((definition lisp-file-definition) definitions
     &aux (pathname (component-pathname (file definition))))
  "Fill in Lisp file DEFINITION's definitions list."
  (setf (definitions definition)
	;; #### FIXME: write a RETAIN or KEEP function, also inverting the
	;; order of TEST and KEY arguments.
	(remove-if-not (lambda (definition)
			 (equal (source definition) pathname))
	    definitions)))



;; ------------------
;; Module definitions
;; ------------------

(defmethod finalize progn
    ((definition module-definition) definitions
     &aux (module (module definition)))
  "Fill in module DEFINITION's child definitions."
  (setf (child-definitions definition)
	;; #### FIXME: write a RETAIN or KEEP function, also inverting the
	;; order of TEST and KEY arguments.
	(remove-if-not (lambda (definition)
			 (and (typep definition 'component-definition)
			      (eq (component-parent (component definition))
				  module)))
	    definitions)))




;; ==========================================================================
;; Documentation Information Extraction
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

  ;; #### NOTE: the Common Lisp standard doesn't specify what happens when an
  ;; object being traversed is modified (see Section 3.6 of the CLHS). So I
  ;; can't reliably use DOLIST here, even though I'm only pushing new
  ;; definitions at the end of the list. I believe however that the code below
  ;; is reliable. Also, note that the finalization process traverses ALL
  ;; definitions, including the foreign ones added during the process. This
  ;; means that we end up with a potentially large number of definitions that
  ;; will probably not be documented. But again, you never know what people
  ;; will want to do with that.
  (do ((definitions (definitions extract) (cdr definitions)))
      ((endp definitions))
    (finalize (first definitions) (definitions extract)))

  extract)




;; ==========================================================================
;; Utilities
;; ==========================================================================

;; #### FIXME: this is the same code as for Lisp files.

(defmethod public-definitions ((extract extract))
  "Return EXTRACT's public definitions."
  (remove-if-not (lambda (definition)
		   (and (typep definition 'symbol-definition)
			(publicp definition)))
      (definitions extract)))

(defmethod private-definitions ((extract extract))
  "Return EXTRACT's private definitions."
  (remove-if (lambda (definition)
	       (or (not (typep definition 'symbol-definition))
		   (publicp definition)))
      (definitions extract)))

;;; extract.lisp ends here
