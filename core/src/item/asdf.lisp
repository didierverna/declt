;;; asdf.lisp --- ASDF Items

;; Copyright (C) 2010-2013 Didier Verna

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

;; -------------------
;; Rendering protocols
;; -------------------

(defmethod name ((component asdf:component))
  "Return COMPONENT's name."
  (component-name component))



;; ==========================================================================
;; Files
;; ==========================================================================

;; -------------------
;; Item protocols
;; -------------------

;; Type name protocol

(defmethod type-name ((cl-source-file asdf:cl-source-file))
  "Return \"Lisp file\""
  "Lisp file")

(defmethod type-name ((c-source-file asdf:c-source-file))
  "Return \"C file\""
  "C file")

(defmethod type-name ((java-source-file asdf:java-source-file))
  "Return \"Java file\""
  "Java file")

(defmethod type-name ((static-file asdf:static-file))
  "Return \"File\""
  "File")

(defmethod type-name ((doc-file asdf:doc-file))
  "Return \"Doc file\""
  "Doc file")

(defmethod type-name ((html-file asdf:html-file))
  "Return \"HTML file\""
  "HTML file")


;; ------------------------------------
;; Definition file definitions protocol
;; ------------------------------------

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
	   (when (macro-definition-access-expander macro)
	     (definition-file-definitions
	      (macro-definition-access-expander macro)
	      file))))
  (:method ((accessor accessor-definition) file)
    "Handle ACCESSOR, its writer and its setf expander."
    (nconc (call-next-method)
	   (when (accessor-definition-writer accessor)
	     (definition-file-definitions
	      (accessor-definition-writer accessor)
	      file))
	   (when (accessor-definition-access-expander accessor)
	     (definition-file-definitions
	      (accessor-definition-access-expander accessor)
	      file))))
  (:method ((accessor-method accessor-method-definition) file)
    "Handle ACCESSOR-METHOD and its writer method."
    (nconc (call-next-method)
	   (definition-file-definitions
	    (accessor-method-definition-writer accessor-method)
	    file)))
  (:method ((generic generic-definition) file)
    "Handle GENERIC function and its methods."
    (nconc (call-next-method)
	   (mapcan (lambda (method)
		     (definition-file-definitions method file))
		   (generic-definition-methods generic))))
  (:method ((generic-accessor generic-accessor-definition) file)
    "Handle GENERIC-ACCESSOR, its generic writer and its setf expander."
    (nconc (call-next-method)
	   (when (generic-accessor-definition-writer generic-accessor)
	     (definition-file-definitions
	      (generic-accessor-definition-writer generic-accessor)
	      file))
	   (when (generic-accessor-definition-access-expander generic-accessor)
	     (definition-file-definitions
	      (generic-accessor-definition-access-expander generic-accessor)
	      file)))))

(defun file-definitions (file definitions)
  "Return the subset of DEFINITIONS that belong to FILE."
  (mapcan-definitions-pool
   (lambda (definition) (definition-file-definitions definition file))
   definitions))



;; ==========================================================================
;; Modules
;; ==========================================================================

;; -------------------
;; Item protocols
;; -------------------

;; Type name protocol

(defmethod type-name ((module asdf:module))
  "Return \"module\""
  "module")



;; ==========================================================================
;; System
;; ==========================================================================

;; -------------------
;; Item protocols
;; -------------------

;; Type name protocol

(defmethod type-name ((system asdf:system))
  "Return \"system\""
  "system")


;; ---------
;; Utilities
;; ---------

(defun lisp-pathnames (system)
  "Return the list of all ASDF SYSTEM's Lisp source file pathnames.
The list includes the system definition file."
  (cons (system-source-file system)
	(mapcar #'component-pathname (lisp-components system))))

(defun system-packages (system)
  "Return the list of packages defined in ASDF SYSTEM."
  (remove-duplicates (mapcan #'file-packages (lisp-pathnames system))))

(defun system-external-symbols (system)
  "Return the list of ASDF SYSTEM's external symbols."
  (mapcan #'package-external-symbols (system-packages system)))

(defun system-internal-symbols (system)
  "Return the list of ASDF SYSTEM's internal symbols."
  (mapcan #'package-internal-symbols (system-packages system)))


;;; asdf.lisp ends here
