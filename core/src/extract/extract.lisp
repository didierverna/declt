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
   (system-definitions :documentation "The list of system definitions."
		       :accessor system-definitions)
   (module-definitions :documentation "The list of module definitions."
		       :accessor module-definitions)
   (file-definitions :documentation "The list of file definitions."
		     :accessor file-definitions)
   (package-definitions :documentation "The list of package definitions."
			:accessor package-definitions)
   (symbol-definitions :documentation "The list of symbol definitions."
		       :accessor symbol-definitions)
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
(defmethod external-definitions
    ((extract extract)
     &aux (external-symbols
	   (mapcan #'package-external-symbols
	     (mapcar #'definition-package
	       (remove-if #'foreignp (package-definitions extract))))))
  "Return EXTRACT's external definitions."
  (remove-if-not (lambda (symbol) (member symbol external-symbols))
      (symbol-definitions extract)
    :key #'definition-symbol))

(defmethod internal-definitions
    ((extract extract)
     &aux (internal-symbols
	   (mapcan #'package-internal-symbols
	     (mapcar #'definition-package
	       (remove-if #'foreignp (package-definitions extract))))))
  "Return EXTRACT's internal definitions."
  (remove-if-not (lambda (symbol) (member symbol internal-symbols))
      (symbol-definitions extract)
    :key #'definition-symbol))




;; ==========================================================================
;; Extract Population
;; ==========================================================================

;; #### NOTE: there are more clever ways to populate the extract and finalize
;; it, notably by avoiding traversing the same lists several times. In
;; particular, modules belong to a single system, and files belong to a single
;; module, so we could created those at the same time. On the other hand, the
;; way it's done below is simpler, and unlikely to affect performance that
;; much. The number of ASDF definitions is probably much smaller than the
;; number of symbol definitions.

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

(defun add-system-definitions (extract system)
  "Add all (sub)system definitions to EXTRACT.
The considered systems are those found recursively in SYSTEM's dependencies,
and located under SYSTEM's directory. The main system appears first."
  (setf (system-definitions extract)
	(mapcar #'make-system-definition
	  (subsystems system (system-directory system)))))



;; ------------------
;; Module definitions
;; ------------------

;; #### WARNING: do not confuse this function with ASDF's MODULE-COMPONENTS
;; (which, BTW, is deprecated in favor of COMPONENT-CHILDREN).
(defun module-components (parent)
  "Return the list of all module components from ASDF PARENT."
  (components parent 'asdf:module))

(defun add-module-definitions (extract)
  "Add all module definitions to EXTRACT."
  (setf (module-definitions extract)
	(mapcar #'make-module-definition
	  (mapcan #'module-components
	    (mapcar #'system
	      (system-definitions extract))))))



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

(defun add-file-definitions
    (extract &aux (systems (mapcar #'system (system-definitions extract))))
  "Add all file definitions to EXTRACT."
  (setf (file-definitions extract)
	(append (make-system-file-definitions systems)
		(mapcar #'make-file-definition
		  (mapcan #'file-components systems)))))



;; -------------------
;; Package definitions
;; -------------------

(defun file-packages (file)
  "Return the list of all packages defined in FILE."
  (remove-if-not (lambda (source) (equal source file)) (list-all-packages)
    :key #'source))

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

(defun add-package-definitions (extract)
  "Add all package definitions to EXTRACT."
  (setf (package-definitions extract)
	(mapcar #'make-package-definition
	  (append (mapcan #'file-packages
		    (mapcar #'component-pathname
		      (mapcar #'file
			(remove-if-not #'lisp-file-definition-p
			    (file-definitions extract)))))
		  (mapcan #'system-unlocated-packages
		    (mapcar #'system
		      (system-definitions extract)))))))



;; ------------------
;; Symbol definitions
;; ------------------

(defun package-symbols (package &aux symbols)
  "Return the list of symbols from home PACKAGE."
  (do-symbols (symbol package symbols)
    (when (eq (symbol-package symbol) package)
      (push symbol symbols))))

(defun add-symbol-definitions (extract)
  "Add all symbol definitions to EXTRACT."
  (setf (symbol-definitions extract)
	(mapcan #'make-symbol-definitions
	  (mapcan #'package-symbols
	    (mapcar #'definition-package
	      ;; #### NOTE: at that point, we don't have any foreign package
	      ;; definitions here, so we don't need to filter them.
	      (package-definitions extract))))))




;; ==========================================================================
;; Extract Finalization
;; ==========================================================================

(defgeneric finalize (definition extract)
  (:documentation "Finalize DEFINITION in EXTRACT.")
  (:method-combination progn))


;; ------------------
;; Symbol definitions
;; ------------------

(defmethod finalize progn
    ((definition symbol-definition) extract
     &aux (package (symbol-package (definition-symbol definition)))
	  (package-definition (find package (package-definitions extract)
				:key #'definition-package)))
  "Fill in DEFINITION's package definition.
If no package definition is found, create and add a foreign one."
  (unless package-definition
    (endpush
     (setq package-definition (make-package-definition package t))
     (package-definitions extract)))
  (setf (package-definition definition) package-definition))

(defun finalize-symbol-definitions (extract)
  "Finalize EXTRACT's symbol definitions."
  (dolist (definition (symbol-definitions extract))
    (finalize definition extract)))



;; -------------------
;; Package definitions
;; -------------------

;; #### NOTE: admittedly, it's a bit too much to make this a FINALIZE method,
;; as there's only one package definition class. But it's more coherent like
;; that, and who knows what's gonna happen in the future.

(defmethod finalize progn ((definition package-definition) extract)
  "Finalize package DEFINITION in EXTRACT.
This means populating its use, used-by, and symbol definitions lists.
New foreign package definitions may be created and added at the end of
EXTRACT's package definitions in the process."
  ;; 1. Use and used-by lists.
  (flet ((find-package-definition (package)
	   "Find PACKAGE definition in EXTRACT.
If not found, create and add a foreign one."
	   (or (find package (package-definitions extract)
		 :key #'definition-package)
	       (let ((definition (make-package-definition package t)))
		 (endpush definition (package-definitions extract))
		 definition))))
    (let ((package (definition-package definition)))
      (setf (use-definitions definition)
	    (mapcar #'find-package-definition
	      (package-use-list package)))
      (setf (used-by-definitions definition)
	    (mapcar #'find-package-definition
	      (package-used-by-list package)))))
  ;; 2. Symbol definitions list.
  (setf (symbol-definitions definition)
	;; #### FIXME: write a RETAIN or KEEP function.
	(remove-if-not (lambda (package-definition)
			 (eq package-definition definition))
	    (symbol-definitions extract)
	  :key #'package-definition)))

(defun finalize-package-definitions (extract)
  "Finalize EXTRACT's package definitions."
  ;; #### NOTE: the Common Lisp standard doesn't specify what happens when an
  ;; object being traversed is modified (see Section 3.6 of the CLHS). So I
  ;; can't reliably use DOLIST here, even though I'm only pushing new package
  ;; definitions at the end of the list. I believe however that the code below
  ;; is reliable. Also, note that the finalization process traverses ALL
  ;; package definitions, including the foreign ones. This means that we end
  ;; up with a potentially large number of packages (because of the use and
  ;; used-by list) that will probably not be documented. But again, you never
  ;; know what people will want to do with that.
  (do ((definitions (package-definitions extract) (cdr definitions)))
      ((endp definitions))
    (finalize (first definitions) extract)))




;; ----------------
;; File definitions
;; ----------------

(defun finalize-file-definitions (extract)
  "Finalize EXTRACT's file definitions.
More specifically, for each file definition:
- fill in its parent,
- populate a system file's system definitions list,
- populate a Lisp file's package definitions list,
- populate a Lisp file's symbol definitions list."
  (dolist (definition (file-definitions extract))
    ;; 1. Parent.
    (setf (parent definition)
	  (let ((parent (component-parent (file definition))))
	    (or (find parent (module-definitions extract) :key #'module)
		(find parent (system-definitions extract) :key #'system))))
    ;; Lisp-specific
    ;; 2. System definitions list.
    (when (typep definition 'system-file-definition)
      (setf (system-definitions definition)
	    (remove-if-not (lambda (system)
			     (equal (system-source-file system)
				    (component-pathname (file definition))))
		(system-definitions extract)
	      :key #'system)))
    (when (typep definition 'lisp-file-definition)
      ;; 3. Package definitions list.
      (setf (package-definitions definition)
	    (remove-if-not (lambda (package)
			     (equal (source package)
				    (component-pathname (file definition))))
		(package-definitions extract)
	      :key #'definition-package))
      ;; 4. Symbol definitions list.
      (setf (symbol-definitions definition)
	    (sort (definitions-from-file
		   (component-pathname (file definition))
		   (symbol-definitions extract))
		  #'string-lessp :key #'definition-symbol)))))



;; ------------------
;; Module definitions
;; ------------------

(defun finalize-module-definitions (extract)
  "Finalize EXTRACT's module definitions.
More specifically, for each module definition:
- fill in its parent,
- fill in its children, in the module's order."
  (dolist (definition (module-definitions extract))
    (setf (parent definition)
	  (let ((parent (component-parent (module definition))))
	    (or (find parent (module-definitions extract) :key #'module)
		(find parent (system-definitions extract) :key #'system)))))
  (let ((children
	  (append (module-definitions extract) (file-definitions extract))))
    (dolist (definition (module-definitions extract))
      (setf (children definition)
	    (mapcar (lambda (child) (find child children :key #'component))
	      (component-children (module definition)))))))



;; ------------------
;; System definitions
;; ------------------

;; #### NOTE: there's some duplication from the above here. Creating the
;; children list is exactly the same thing in systems and modules because
;; systems /are/ modules. Not a big deal though.
(defun finalize-system-definitions (extract)
  "Finalize EXTRACT's system definitions.
More specifically, for each system definition:
- fill in its children, in the system's order."
  ;; At that point, all files and module definitions have their PARENT slot
  ;; properly set already.
  (let ((children
	  (append (module-definitions extract) (file-definitions extract))))
    (dolist (definition (system-definitions extract))
      (setf (children definition)
	    (mapcar (lambda (child) (find child children :key #'component))
	      (component-children (system definition)))))))




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

  ;; #### NOTE: because of the way the EXTRACT structure is filled in, the
  ;; call order below is important. Each addition relies on the previous ones
  ;; having been performed, and the various finalization steps need to be
  ;; performed in reverse order.
  (add-system-definitions extract system)
  (add-module-definitions extract)
  (add-file-definitions extract)
  (add-package-definitions extract)
  (add-symbol-definitions extract)
  (finalize-symbol-definitions extract)
  (finalize-package-definitions extract)
;;  (finalize-file-definitions extract)
  (finalize-module-definitions extract)
  (finalize-system-definitions extract)

  extract)

;;; extract.lisp ends here
