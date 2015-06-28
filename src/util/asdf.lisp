;;; asdf.lisp --- ASDF Utilities

;; Copyright (C) 2010, 2011, 2013 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of Declt.

;; Declt is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 3,
;; as published by the Free Software Foundation.

;; Declt is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:



;;; Code:

(in-package :com.dvlsoft.declt)
(in-readtable :com.dvlsoft.declt)


;; ==========================================================================
;; Miscellaneous
;; ==========================================================================

(defun relative-location (component relative-to)
  "Return COMPONENT's location RELATIVE-TO."
  (enough-namestring (component-pathname component) relative-to))

(defun sub-component-p (component relative-to)
  "Return T if COMPONENT can be found under RELATIVE-TO."
  (pathname-match-p (component-pathname component)
		    (make-pathname :name :wild
				   :directory
				   (append (pathname-directory relative-to)
					   '(:wild-inferiors)))))

(defun components (module type)
  "Return the list of all components of (sub)TYPE from ASDF MODULE."
  ;; #### NOTE: we accept subtypes of TYPE because ASDF components might be
  ;; subclassed. An example of this is SBCL's grovel facility which subclasses
  ;; asdf:cl-source-file.
  (loop :for component :in (asdf:module-components module)
	:if (typep component type)
	  :collect component
	:if (typep component 'asdf:module)
	  :nconc (components component type)))

(defun lisp-components (module)
  "Return the list of all Lisp source file components from ASDF MODULE."
  (components module 'asdf:cl-source-file))

;; #### WARNING: do not confuse this function with asdf:module-components!
(defun module-components (module)
  "Return the list of all module components from ASDF MODULE."
  (components module 'asdf:module))

(defun system-directory (system)
  "Return ASDF SYSTEM's directory."
  (component-pathname system))

(defun system-base-name (system)
  "Return the basename part of ASDF SYSTEM's definition file."
  (file-namestring (system-source-file system)))

(defun system-file-name (system)
  "Return the name part of ASDF SYSTEM's definition file."
  (pathname-name (system-source-file system)))

(defun system-file-type (system)
  "Return the type part of ASDF SYSTEM's definition file."
  (pathname-type (system-source-file system)))

;; #### FIXME: there is redundancy with RENDER-DEPENDENCIES. I should write a
;; more abstract dependency walker.
(defgeneric system-dependency-subsystem (dependency-def system relative-to)
  (:documentation "Return SYSTEM's subsystem from DEPENDENCY-DEF or nil.")
  (:method (simple-component-name system relative-to
	    &aux (dependency
		  (resolve-dependency-name system simple-component-name)))
    "Return SYSTEM's subsystem named SIMPLE-COMPONENT-NAME or nil."
    (when (sub-component-p dependency relative-to)
      dependency))
  ;; #### NOTE: this is where I'd like more advanced pattern matching
  ;; capabilities.
  (:method ((dependency-def list) system relative-to)
    "Return SYSTEM's subsystem from DEPENDENCY-DEF or nil."
    (cond ((eq (car dependency-def) :feature)
	   (system-dependency-subsystem
	    (caddr dependency-def) system relative-to))
	  ((eq (car dependency-def) :version)
	   (system-dependency-subsystem
	    (cadr dependency-def) system relative-to))
	  ((eq (car dependency-def) :require))
	  (t (warn "Invalid ASDF dependency.")))))

(defun system-subsystems (system &aux (relative-to (system-directory system)))
  "Return the list of SYSTEM's subsystems.
A subsystem is a SYSTEM dependency located somewhere under SYSTEM's
directory."
  (remove-duplicates
   (loop :for dependency :in (append (defsystem-dependencies system)
				     (component-sideway-dependencies system))
	 :when (system-dependency-subsystem dependency system relative-to)
	   :collect :it)))

;;; asdf.lisp ends here
