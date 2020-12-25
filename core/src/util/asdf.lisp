;;; asdf.lisp --- ASDF Utilities

;; Copyright (C) 2010, 2011, 2013, 2016-2017, 2020 Didier Verna

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
;; Miscellaneous
;; ==========================================================================

(defun relative-location (component relative-to)
  "Return COMPONENT's location RELATIVE-TO."
  (enough-namestring (component-pathname component) relative-to))

(defun sub-component-p
    (component relative-to
     ;; #### NOTE: COMPONENT-PATHNAME can return nil when it's impossible to
     ;; locate the component's source. This happens for example with UIOP when
     ;; ASDF is embedded in a Lisp implementation like SBCL. Sabra Crolleton
     ;; fell on this issue when trying to document CL-PROJECT, which
     ;; explicitly depends on UIOP.
     &aux (component-pathname (component-pathname component)))
  "Return T if COMPONENT can be found under RELATIVE-TO."
  (when component-pathname
    (pathname-match-p component-pathname
		      (make-pathname :name :wild
				     :directory
				     (append (pathname-directory relative-to)
					     '(:wild-inferiors))))))

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

(defun defsystem-dependencies (system)
  "Return ASDF SYSTEM's defsystem dependencies."
  (when (slot-boundp system 'asdf::defsystem-depends-on)
    (asdf::system-defsystem-depends-on system)))

(defun system-dependencies (system)
  "Return all SYSTEM dependencies.
This includes both :defsystem-depends-on and :depends-on."
  (append (defsystem-dependencies system)
	  (component-sideway-dependencies system)))

;; #### WARNING: do not confuse this function with asdf:module-components!
(defun module-components (module)
  "Return the list of all module components from ASDF MODULE."
  (components module 'asdf:module))

(defun system-directory (system)
  "Return ASDF SYSTEM's directory."
  (component-pathname system))

(defun system-base-name (system &aux (file (system-source-file system)))
  "Return the basename part of ASDF SYSTEM's definition file."
  (when file (file-namestring file)))

;; #### NOTE: currently unused.
(defun system-file-name (system &aux (file (system-source-file system)))
  "Return the name part of ASDF SYSTEM's definition file."
  (when file (pathname-name file)))

;; #### NOTE: currently unused.
(defun system-file-type (system &aux (file (system-source-file system)))
  "Return the type part of ASDF SYSTEM's definition file."
  (when file (pathname-type file)))

;; #### FIXME: there is redundant with RENDER-DEPENDENCIES. I should write a
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
	  ((eq (car dependency-def) :require) nil)
	  (t (warn "Invalid ASDF dependency.")))))

(defun subsystems (system relative-to)
  "Return the list of SYSTEM subsystems RELATIVE-TO.
This function recursively descends all found subsystems."
  (loop :with subsystem
	:for dependency :in (system-dependencies system)
	:do (setq subsystem (system-dependency-subsystem
			     dependency system relative-to))
	:when subsystem
	  :nconc (cons subsystem (subsystems subsystem relative-to))))

;;; asdf.lisp ends here
