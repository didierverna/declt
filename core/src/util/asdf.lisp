;;; asdf.lisp --- ASDF Utilities

;; Copyright (C) 2010, 2011, 2013, 2016-2017, 2020, 2021 Didier Verna

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

(defun components (parent type)
  "Return the list of all components of (sub)TYPE from ASDF PARENT."
  ;; #### NOTE: we accept subtypes of TYPE because ASDF components might be
  ;; subclassed. An example of this is SBCL's grovel facility which subclasses
  ;; asdf:cl-source-file.
  (loop :for component :in (component-children parent)
	:if (typep component type)
	  :collect component
	:if (typep component 'asdf:module)
	  :nconc (components component type)))

;; #### WARNING: do not confuse this function with ASDF's MODULE-COMPONENTS
;; (which, BTW, is deprecated in favor of COMPONENT-CHILDREN).
(defun module-components (parent)
  "Return the list of all module components from ASDF PARENT."
  (components parent 'asdf:module))

(defun file-components (parent)
  "Return the list of all file components from ASDF PARENT."
  (components parent 'asdf:file-component))

(defun lisp-components (parent)
  "Return the list of all Lisp source file components from ASDF PARENT."
  (components parent 'asdf:cl-source-file))

(defun system-dependencies (system)
  "Return all SYSTEM dependencies.
This includes both :defsystem-depends-on and :depends-on."
  (append (system-defsystem-depends-on system)
	  (component-sideway-dependencies system)))

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

(defun dependency-def-system (dependency-def)
  "Extract a system name from ASDF DEPENDENCY-DEF specification."
  (typecase dependency-def ;; RTE to the rescue!
    (list (ecase (car dependency-def)
	    (:feature (dependency-def-system (third dependency-def)))
	    (:version (second dependency-def))
	    (:require nil)))
    (otherwise dependency-def)))

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

;;; asdf.lisp ends here
