;;; asdf.lisp --- ASDF Items

;; Copyright (C) 2010, 2011, 2012 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>

;; This file is part of Declt.

;; Declt is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version3,
;; as published by the Free Software Foundation.

;; Declt is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; Contents management by FCM version 0.1.


;;; Code:

(in-package :com.dvlsoft.declt)
(in-readtable :com.dvlsoft.declt)


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
  (:method ((accessor accessor-definition) file)
    "Handle ACCESSOR and its writer function."
    (nconc (call-next-method)
	   (definition-file-definitions
	    (accessor-definition-writer accessor)
	    file)))
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
    "Handle GENERIC-ACCESSOR and its generic writer function."
    (nconc (call-next-method)
	   (definition-file-definitions
	    (generic-accessor-definition-writer generic-accessor)
	    file))))

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
  (cons (system-definition-pathname system)
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
