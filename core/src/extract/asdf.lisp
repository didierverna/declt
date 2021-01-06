;;; asdf.lisp --- ASDF definitions

;; Copyright (C) 2010-2013, 2015-2017, 2020 Didier Verna

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
;; Files
;; ==========================================================================

;; --------------------
;; Extraction protocols
;; --------------------

(defmethod type-name ((source-file asdf:source-file))
  "Return \"file\""
  "file")


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

(defun file-definitions (file definitions)
  "Return the subset of DEFINITIONS that belong to FILE."
  (mapcan-definitions-pool
   (lambda (definition) (definition-file-definitions definition file))
   definitions))



;; ==========================================================================
;; Modules
;; ==========================================================================

;; --------------------
;; Extraction protocols
;; --------------------

(defmethod type-name ((module asdf:module))
  "Return \"module\""
  "module")



;; ==========================================================================
;; System
;; ==========================================================================

;; --------------------
;; Extraction protocols
;; --------------------

(defmethod type-name ((system asdf:system))
  "Return \"system\""
  "system")


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
  (loop :for package :in (list-all-packages)
	:when (equal (source package) file)
	  :collect package))

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
  (loop :for package :in (list-all-packages)
	:for package-name := (package-name package)
	:when (and (not (source package))
		   (> (length package-name) length)
		   (string-equal prefix (subseq package-name 0 length)))
	  :collect package))

(defun system-packages (system)
  "Return the list of packages defined in ASDF SYSTEM."
  (append (system-located-packages system) (system-unlocated-packages system)))

;;; asdf.lisp ends here
